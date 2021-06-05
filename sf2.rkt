#lang racket

(require "parse-sf2.rkt")
(provide construct-presets construct-insts
         (struct-out preset) (struct-out inst))

(module+ test (require rackunit))

;; zones is a list of lists of generators
(struct preset [name zones] #:transparent)
(struct inst [name zones] #:transparent)

;; ix is an instrument for presets, and a sample for inst
(struct zone [ix range aux] #:transparent)

(define (sublist li start end)
  (take (drop li start) (- end start)))

(module+ test
 (check-equal? (sublist '() 0 0) '())
 (check-equal? (sublist '(1 2 3 4) 1 3) '(2 3)))

(define (construct-gens sample-ix gens)
  (println "Here")
  (match gens
   ['() '()]
   ;; has key range
   [(list (gen 43 data) gens ... (gen (? (curry = sample-ix)) data2))
    (define data* (bytes->list data))
    (zone (integer-bytes->integer data2 #f) 
          (cons (car data*)
                (cdr data*))
          gens)]
   [(list gens ... (gen (? (curry = sample-ix)) data2))
    (zone (integer-bytes->integer data2 #f) #f gens)]
   ;; ignoring global
   [_ #f]))


(define (construct-zoned construct sample-ix headers bags gens)

  (define (construct-zones bags)
    (println "constructing zones")
    (println bags)
    (match bags
      [(cons _ '()) '()]
      [(list* (bag ix) (and next (bag ix*)) more)
       (println ix)
       (println ix*)
       (define gens* (sublist gens ix ix*))
       (cons (construct-gens sample-ix gens*) (construct-zones (cons next more)))]))

  (match headers
    [(cons _ '()) '()]
    [(list* (header name ix) (and next (header _ ix*)) more)
     ;; add a dummy so we can get the end of last bag
     (println "constructing")
     (println name)
     (println ix)
     (println ix*)
     (define bags* (sublist bags ix (add1 ix*)))
     (println bags*)
     (define zones (construct-zones bags*))
     (cons (construct name zones) 
           (construct-zoned construct sample-ix (cons next more) bags gens))]))


(define construct-presets (curry construct-zoned preset 41))
(define construct-insts (curry construct-zoned inst 53))

