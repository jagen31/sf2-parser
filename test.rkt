#lang racket
(require "riff.rkt" "parse-sf2.rkt" "sf2.rkt" ffi/vector rsound)
(provide parse-soundfont 
         get-sample-ix
         sample-ix->rsound)

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

(define fluid (parse-soundfont (open-input-file "FluidR3_GM.sf2")))

(define (get-sample-ix sf num name)
  (let/ec break
  (for/or ([p (soundfont-presets sf)])
    (match p
      [(preset name* zones) #:when (equal? name name*)
       (for/or ([z zones])
         (match z
           [(zone inst-ix (or #f (? (λ(p) (<= (car p) num) (> (cadr p) num)))) _)
            (match (list-ref (soundfont-insts sf) inst-ix)
              [(inst _ zones)
               (for/or ([z zones])
                 (match z
                   [(zone samp-ix (or #f (? (λ(p) (<= (car p) num) (> (cadr p) num)))) _) 
                    samp-ix]
                   [_ #f]))])]
           [_ #f]))]
      [_ #f]))))

(define (sample-ix->rsound sf n)
  (match (list-ref (soundfont-samples sf) n)
    [(sample-header _ start end _)
     (define data (subbytes (soundfont-sample-data sf) start end))
     (vec->rsound (list->s16vector (bytes->list data)) (default-sample-rate))]))

(play (sample-ix->rsound fluid (get-sample-ix fluid 60 "Alto Sax")))

