;;
;; ndfac.ss - implementation of an ndfa-based regexp recognizer, as
;; translated to continuation-composing style from an original
;; direct-style implementation written using shift.
;;

;; r ::= <symbol>
;;     | (& r1 r2) (sequencing)
;;     | (/ r1 r2) (alternation)
;;     | (* r) (repeating)

(define ndfa-c
  (lambda (r l k)
    (if (symbol? r)
        (if (and (not (null? l))
                 (eq? (car l) r))
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
            (lambda (l1)
              (if (null? l1) (k "accepted") "no")))))
