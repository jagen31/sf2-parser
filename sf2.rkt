#lang racket

(require "parse-sf2.rkt")
(provide construct-presets construct-insts
         (struct-out zone)
         (struct-out preset) (struct-out inst))

(module+ test (require rackunit))

(struct preset [name zones] #:transparent)
(struct inst [name zones] #:transparent)

;; ix is an instrument for presets, and a sample for inst
;; alt-key is an override for the root key of the sample.
(struct zone [ix range alt-key aux] #:transparent)

(define (sublist li start end)
  (take (drop li start) (- end start)))

(module+ test
 (check-equal? (sublist '() 0 0) '())
 (check-equal? (sublist '(1 2 3 4) 1 3) '(2 3)))

;; sample-ix is either the index of the instrument or the index of the
;; sample, depending on whether we are parsing pgens or igens
(define (construct-gens sample-ix gens)
  (define-values (ix range key aux)
    (for/fold ([ix #f] [range #f] [key #f] [aux '()])
              ([g gens])
      (match g
        [(gen x data) #:when (= sample-ix x)
         (values (integer-bytes->integer data #f #f) range key aux)]
        [(gen 43 data)
         (values ix (bytes->list data) key aux)]
        [(gen 58 data) (values ix range (integer-bytes->integer data #f #f) aux)]
        ;; has key range
        [_ (values ix range key (cons g aux))])))
  (zone ix range key aux))


(define (construct-zoned construct sample-ix headers bags gens)

  (define (construct-zones bags)
    (match bags
      [(cons _ '()) '()]
      [(list* (bag ix) (and next (bag ix*)) more)
       (define gens* (sublist gens ix ix*))
       (define gens** (construct-gens sample-ix gens*))
       (define rest (construct-zones (cons next more)))
       (if gens** (cons gens** rest) rest)]))

  (match headers
    [(cons _ '()) '()]
    [(list* (header name ix) (and next (header _ ix*)) more)
     ;; add a dummy so we can get the end of last bag
     (define bags* (sublist bags ix (add1 ix*)))
     (define zones (construct-zones bags*))
     (cons (construct name zones) 
           (construct-zoned construct sample-ix (cons next more) bags gens))]))


(define construct-presets (curry construct-zoned preset 41))
(define construct-insts (curry construct-zoned inst 53))

