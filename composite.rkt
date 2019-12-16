#lang racket

;; Syntax definitions
(define-syntax-rule (-> obj msg args ...)
  ((obj (quote msg)) obj args ...))

(define-syntax-rule (get obj msg)
  (obj (quote msg)))

(define-syntax-rule (new objname [(f v) ...])
  (obj objname (list (list (quote f) v) ...)))

(define (obj superobj fields)
  (λ (msg)
    (let ([field (assoc msg fields)])
      (if field
          (second field)
          (superobj msg)))))

;; Object prototype definitions
(define Object (λ (msg) (void)))

(define File (new Object [(name "")
                          (printName
                           (λ (self depth)
                             (printf "~a~n" (get self name))))]))

(define Dir (new File [(printName
                        (λ (self depth)
                          (begin
                            (printf "~a/~n" (get self name))
                            (map (λ (child)
                                   (begin
                                     (printf "~a" (string-append* (make-list (+ depth 1) "    ")))
                                     (-> child printName (+ depth 1))))
                                   (get self children))
                            (void))))
                       (children empty)]))

;; Concrete object definitions
(define file1 (new File ([name "file1"])))
(define file2 (new File ([name "file2"])))
(define file3 (new File ([name "file3"])))

(define dir1 (new Dir ([name "dir1"]
                       [children (list file1 file2)])))
(define dir3 (new Dir ([name "dir3"]
                       [children (list file3)])))
(define dir2 (new Dir ([name "dir2"]
                       [children (list dir1 dir3)])))

(-> dir2 printName 0)