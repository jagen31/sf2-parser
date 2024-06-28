#lang racket/base
(require "riff.rkt" "parse-sf2.rkt" "sf2.rkt"
         ffi/vector rsound
         racket/match racket/function
         data/interval-map)
(provide (struct-out soundfont) (all-from-out "sf2.rkt") (all-from-out "parse-sf2.rkt") parse-soundfont load-preset preset-midi->rsound)

(define (parse-soundfont data)
  (define-values (riff _) (parse-chunk data))

  (define sdta (get-subchunk "sdta" riff))
  (define pdta (get-subchunk "pdta" riff))

  (match-define (list shdr-chunk phdr-chunk pbag-chunk pgen-chunk inst-chunk ibag-chunk igen-chunk)
    (map (λ(x) (get-subchunk x pdta)) 
         (list "shdr" "phdr" "pbag" "pgen" "inst" "ibag" "igen")))

  (define (make-stream ck) (open-input-bytes (byte-data-data (chunk-data ck))))

  (match-define (list shdr-stream phdr-stream pbag-stream pgen-stream
                      inst-stream ibag-stream igen-stream)
  (map make-stream (list shdr-chunk phdr-chunk pbag-chunk pgen-chunk inst-chunk ibag-chunk igen-chunk)))

  (define (parse-all f data)
    (if (equal? (peek-byte data) eof)
        '()
        (cons (f data) (parse-all f data))))

  (match-define (list shdrs phdrs pbags pgens ihdrs ibags igens)
    (map (λ(f s) (parse-all f s))
         (list parse-sample-header
               parse-preset-header (curry parse-bag preset-bag) (curry parse-gen preset-gen)
               parse-inst-header (curry parse-bag inst-bag) (curry parse-gen inst-gen))
         (list shdr-stream phdr-stream pbag-stream pgen-stream
               inst-stream ibag-stream igen-stream)))

  (define presets (construct-presets phdrs pbags pgens))
  (define insts (construct-insts ihdrs ibags igens))
  (define sample-data (byte-data-data (chunk-data (get-subchunk "smpl" sdta))))
  (soundfont presets insts shdrs sample-data))

(struct soundfont [presets insts samples sample-data])
(struct sample [root-key data rate start-loop end-loop])

(define (load-preset sf name)
  (for/or ([p (soundfont-presets sf)])
    (match p
      [(preset name* zones) #:when (equal? name name*)
       (for/fold ([map (make-interval-map)])
                 ([z zones])
         (match z
           [(zone (? number? inst-ix) range _ _)
            (match-define (cons lo hi) (or range (cons 0 100)))
            (match (list-ref (soundfont-insts sf) inst-ix)
              [(inst _ zones)
               (for/fold ([acc '()])
                         ([z zones])
                 (match z
                   [(zone samp-ix (cons lo* hi*) root-key* _)
                    #:when (and (>= lo* lo*) (<= hi* hi))
                    (match (list-ref (soundfont-samples sf) samp-ix)
                      [(sample-header _ start end root-key rate sl el)
                       (define data (subbytes (soundfont-sample-data sf) (* start 2) (* end 2)))
                       (interval-map-set!
                        map lo*
                        (add1 hi*)
                        (sample (or root-key* root-key) data rate sl el))
                       map])]
                   [_ acc]))])]
           [_ map]))]
      [_ #f])))

(define (key+sample->rsound key s len)
  (match s
    [(sample root-key data rate sl el)
     (define diff (- len (bytes-length data)))
     (define data*
       (if (< diff 0)
           data
           (let ()
             (define loop-len (- el sl))
             (define num-loops (inexact->exact (ceiling (/ diff loop-len))))
             (define loop (subbytes data (* sl 2) (* el 2)))
             (define loop-data (apply bytes-append (build-list num-loops (λ(_) loop))))
             (bytes-append data loop-data))))
     (define data**
       (let loop ([bs (bytes->list data*)])
         (match bs
           ['() '()]
           [(cons a '()) (loop (list a 0))]
           [(list* a b more)
            (define samp (integer-bytes->integer (bytes a b) #t #f))
            (cons samp (cons samp (loop more)))]
           [(cons a more) '()])))
     (define sound (vec->rsound (list->s16vector data**) rate))
     (resample-to-rate 44100
                       (resample/interp (/ (midi-note-num->pitch key)
                                           (midi-note-num->pitch root-key))
                                        sound))]))

(define (preset-midi-sample preset midi) (interval-map-ref preset midi))

(define (preset-midi->rsound preset midi len)
  (key+sample->rsound midi (preset-midi-sample preset midi) len))
