#lang racket
(provide (struct-out header) (struct-out bag) (struct-out gen)
         (struct-out sample-header)
         (struct-out preset-header) (struct-out preset-bag) (struct-out preset-gen)
         (struct-out inst-header) (struct-out inst-bag) (struct-out inst-gen)
         parse-sample-header parse-preset-header parse-inst-header
         parse-bag parse-gen)

;; bases
(struct header [name ix] #:transparent)
(struct bag [ix] #:transparent)
(struct gen [op data] #:transparent)

;; ignoring some fields
(struct preset-header header [] #:transparent)
;; ix is generator index, ignoring modulators for now
(struct preset-bag bag [] #:transparent)
(struct preset-gen gen [] #:transparent)

;; not ignoring fields, that's all that is in this one
(struct inst-header header [] #:transparent)
;; again, generator index
(struct inst-bag bag [] #:transparent)
;; no type theory pun intended
(struct inst-gen gen [] #:transparent)

(struct sample-header [name start end pitch] #:transparent)

(define (parse-name data)
  (bytes->string/latin-1
   (list->bytes
    (takef (bytes->list (read-bytes 20 data))
           (λ(b) (not (equal? b 0)))))))

(define (parse-preset-header data)
  (define name (parse-name data))
  (read-bytes 4 data)
  (define ix (integer-bytes->integer (read-bytes 2 data) #f))
  (read-bytes 12 data)
  (preset-header name ix))

(define (parse-inst-header data)
  (define name (parse-name data))
  (define ix (integer-bytes->integer (read-bytes 2 data) #f))
  (inst-header name ix))

(define (parse-bag construct data)
  (define ix (integer-bytes->integer (read-bytes 2 data) #f))
  (read-bytes 2 data)
  (construct ix))

(define (parse-gen construct data) 
  (define op (integer-bytes->integer (read-bytes 2 data) #f))
  (define data* (read-bytes 2 data))
  (construct op data*))

(define (parse-sample-header data)
  (println "parsing header")
  (define name (parse-name data))
  (define start (integer-bytes->integer (read-bytes 4 data) #f))
  (define end (integer-bytes->integer (read-bytes 4 data) #f))
  (read-bytes 12 data)
  (define pitch (read-byte data))
  (read-bytes 5 data)
  (println "done")
  (sample-header name start end pitch))

