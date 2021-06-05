#lang racket
(require "riff.rkt" "parse-sf2.rkt" "sf2.rkt" ffi/vector rsound)

(define fluid (open-input-file "FluidR3_GM.sf2"))
(define-values (riff _) (parse-chunk fluid))

(define sdta (get-subchunk "sdta" riff))
(define pdta (get-subchunk "pdta" riff))

(define samples (byte-data-data (chunk-data (get-subchunk "smpl" sdta))))

(match-define (list shdr phdr pbag pgen inst ibag igen) 
  (map (λ(x) (get-subchunk x pdta)) 
       (list "shdr" "phdr" "pbag" "pgen" "inst" "ibag" "igen")))

(define (make-stream ck) (open-input-bytes (byte-data-data (chunk-data ck))))

(match-define (list shdr-stream phdr-stream pbag-stream pgen-stream
                    inst-stream ibag-stream igen-stream)
  (map make-stream (list shdr phdr pbag pgen inst ibag igen)))

(define (parse-all f data)
  (if (equal? (peek-byte data) eof)
      '()
      (cons (f data) (parse-all f data))))

(println shdr)

(match-define (list shdrs phdrs pbags pgens ihdrs ibags igens)
  (map (λ(f s) (parse-all f s))
       (list parse-sample-header
             parse-preset-header (curry parse-bag preset-bag) (curry parse-gen preset-gen)
             parse-inst-header (curry parse-bag inst-bag) (curry parse-gen inst-gen))
       (list shdr-stream phdr-stream pbag-stream pgen-stream
             inst-stream ibag-stream igen-stream)))

(define presets (construct-presets phdrs pbags pgens))
(define insts (construct-insts ihdrs ibags igens))
#;(define samples (construct-samples shdrs ))

#;(define (play-note num name)
  (let/ec break
  (for ([p presets])
    (match p
      [(preset name* zones) #:when (equal? name name*)
       (for ([z zones])
         (match z
           [(zone (or #f (? (λ(p) (<= (car p) num) (> (cdr p) num)))) inst-ix)
            (match (list-ref insts inst-ix)
              [(inst _ zones)
               (for ([z zones])
                 (match z
                   [(zone (or #f (? (λ(p) (<= (car p) num) (> (cdr p) num)))) samp-ix)]))
                 ])]))]))))

(define (sample->rsound n)
  (match (list-ref shdrs n)
    [(sample-header _ start end _)
     (define data (subbytes samples start end))
     (define double-data
       (for/list ([b (bytes->list data)])
         (integer-bytes->integer (bytes b b) #t)))
     (println (length double-data))
     (println (s16vector-length (list->s16vector double-data)))
     (rsound (list->s16vector double-data) 0 (/ (- end start) 2) (default-sample-rate))]))

(play (sample->rsound 0))
