#lang racket

(define (obj superobj fields)
  (λ (msg)
    (let ([field (assoc msg fields)])
      (if field
          (second field)
          (superobj msg)))))

;; Object prototype definitions
(define Object (λ (msg) (void)))

(define File (obj Object (list (list 'name "")
                               (list 'printName
                                     (λ (self depth)
                                       (printf "~a~n" (self 'name)))))))

(define Dir (obj File (list (list 'printName
                                  (λ (self depth)
                                    (begin
                                      (printf "~a/~n" (self 'name))
                                      (map (λ (child)
                                             (begin
                                               (printf "~a" (string-append* (make-list (+ depth 1) "    ")))
                                               ((child 'printName) child (+ depth 1))))
                                           (self 'children))
                                      (void))))
                            (list 'children empty))))

;; Concrete object definitions
(define file1 (obj File (list (list 'name "file1"))))
(define file2 (obj File (list (list 'name "file2"))))
(define file3 (obj File (list (list 'name "file3"))))

(define dir1 (obj Dir (list (list 'name "dir1")
                            (list 'children (list file1 file2)))))
(define dir3 (obj Dir (list (list 'name "dir3")
                            (list 'children (list file3)))))
(define dir2 (obj Dir (list (list 'name "dir2")
                            (list 'children (list dir1 dir3)))))

((dir2 'printName) dir2 0)