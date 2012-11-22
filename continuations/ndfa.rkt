#lang racket

; ndfa using explicit continuation passing style
(define ndfa-c
  (lambda (r l k)
    (if (atom? r)
        (if (and (not (null? l)) (equal? (car l) r))
            (k (cdr l))
            "no")
        (case (car r)
          [(&) (ndfa-c (cadr r) l
                       (lambda (l1) (ndfa-c (caddr r) l1 k)))]
          [(/) (begin (ndfa-c (cadr r) l k)
                      (ndfa-c (caddr r) l k)
                      "no")]
          [(*) (begin (k l)
                      (ndfa-c (cadr r) l
                              (lambda (l1) (ndfa-c r l1 k)))
                      "no")]))))

(define accept-c
  (lambda (r l k)
    (ndfa-c r l
            (lambda (l1) (if (null? l1) (k "accepted") "no")))))

(define (atom? x)
  (not (list? x)))

(define (id x) x)
(define (print x) (begin (display x)
                             (newline)))

;(accept-c 'a '(a) print)
;(accept-c '(& a b) '(a b) print)
(accept-c '(/ a b) '(a) print)
;(accept-c '(* a) '() print)
;(accept-c '(/ a b) '(b) id)
;(accept-c '(* a) '() print)
;(accept-c '(* a) '(a) print)
;(accept-c '(* a) '(a a a) print)

