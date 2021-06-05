#lang racket
(require "riff.rkt" "parse-sf2.rkt" "sf2.rkt" ffi/vector rsound)

(define fluid (open-input-file "FluidR3_GM.sf2"))
(define-values (riff _) (parse-chunk fluid))

(define sdta (get-subchunk "sdta" riff))
(define pdta (get-subchunk "pdta" riff))

(define samples (byte-data-data (chunk-data (get-subchunk "smpl" sdta))))

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

(define (get-sample num name)
  (let/ec break
  (for/or ([p presets])
    (match p
      [(preset name* zones) #:when (equal? name name*)
       (for/or ([z zones])
         (match z
           [(zone inst-ix (or #f (? (λ(p) (<= (car p) num) (> (cadr p) num)))) _)
            (match (list-ref insts inst-ix)
              [(inst _ zones)
               (for/or ([z zones])
                 (match z
                   [(zone samp-ix (or #f (? (λ(p) (<= (car p) num) (> (cadr p) num)))) _) 
                    samp-ix]
                   [_ #f]))])]
           [_ #f]))]
      [_ #f]))))

(define (sample->rsound n)
  (match (list-ref shdrs n)
    [(sample-header _ start end _)
     (define data (subbytes samples start end))
     (vec->rsound (list->s16vector (bytes->list data)) (default-sample-rate))]))

(play (sample->rsound (get-sample 60 "Alto Sax")))
