#lang racket
(require racket/match)
(provide parse-chunk
         get-subchunk
         (struct-out chunk-header)
         (struct-out chunk)
         (struct-out list-chunk)
         (struct-out byte-data))

(struct chunk-header [name len] #:transparent)
(struct chunk [head data] #:transparent)
(struct list-chunk chunk [type] #:transparent)
(struct byte-data [data])

(define (parse-chunk data)
  
  (define (parse-chunk-header data)
    (define name (read-bytes 4 data))
    (println name)
    (define len (integer-bytes->integer (read-bytes 4 data) #f))
    (chunk-header (bytes->string/latin-1 name) len))

  (define (parse-chunk-body head data)
    (match-define (chunk-header name len) head)
    
    (define-values (body len*)
      (cond
        [(or (equal? name "LIST") (equal? name "RIFF"))
         (define typ (read-bytes 4 data))
         (values (list-chunk head (parse-subchunks (- len 4) data) (bytes->string/latin-1 typ))
                 (+ 4 len))]
        [else (values (chunk head (byte-data (read-bytes len data))) len)]))
    (if (odd? len)
        (begin (read-byte data)
               (values body (add1 len*)))
        (values body len*)))
  
  (define (parse-subchunks len data)
    (cond
      [(<= len 0) '()]
      [else
       (define-values (ck bytes) (parse-chunk data))
       (println ck)
       (cons ck (parse-subchunks (- len 8 bytes) data))]))
  
  (define head (parse-chunk-header data))
  (parse-chunk-body head data))

(define (get-subchunk name lch)
  (match lch
    [(list-chunk _ data _)
     (findf
      (match-lambda
        [(list-chunk _ _ type)
         (println "here")
         (println type)
         (equal? name type)]
        [(chunk (chunk-header name* _) _) (equal? name name*)])
      data)]))
     
