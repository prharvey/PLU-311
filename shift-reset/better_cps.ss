;;
;; better_cps.ss - Taken from the paper "Abstracting Control" by
;; Olivier Danvy and Andrjez Filinski.
;;

(load "match.ss")


;; helper binding form
(define-syntax with-gensyms
  (syntax-rules ()
    [(with-gensyms (var* ...) expr)
     (let ([var* (gensym)] ...) expr)]))


;; Language:
;; <exp> ::= <var>
;;         | <num>
;;         | bool
;;         | (zero? <exp>)
;;         | (sub1 <exp>)
;;         | (add1 <exp>)
;;         | (<if> <exp> <exp> <exp>)
;;         | (lambda (<var>) <exp>)
;;         | (let-cc <var> <exp>)
;;         | (let-shift <var> <exp>)
;;         | (reset <exp>)
;;         | (<exp> <exp>)


;; The following is based on naive_cps, but "represents continuations
;; in the converted terms as semantic functions operating on pieces of
;; abstract syntax, rather than as syntactic terms."

(define bcps
  (lambda (exp)
    (let cps ([exp exp] [env (lambda (x) x)])
      (match exp
        [,var (guard (symbol? var))
              (lambda (k) (k (env var)))]
        [,num (guard (number? num))
              (lambda (k) (k num))]
        [,bool (guard (boolean? bool))
               (lambda (k) (k bool))]        
        [(zero? ,[exp])
         (lambda (k) (exp (lambda (a) (k `(zero? ,a)))))]
        [(sub1 ,[exp])
         (lambda (k) (exp (lambda (a) (k `(sub1 ,a)))))]
        [(add1 ,[exp])
         (lambda (k) (exp (lambda (a) (k `(add1 ,a)))))]
        [(if ,[pred] ,[conseq] ,[altern])
         (lambda (k) (pred (lambda (b)
                             `(if ,b ,(conseq k) ,(altern k)))))]
        [(lambda (,var) ,[exp])
         (lambda (k) (k `(lambda (,var)
                           (lambda (k)
                             ,(exp (lambda (a) `(k ,a)))))))]
        [(let-cc ,var ,exp)
         (lambda (k)
           (let ([exp (cps exp (lambda (v)
                               (if (eq? v var)
                                   `(lambda  (a)
                                      (lambda (k^) ,(k `a)))
                                   (env v))))])
              (exp k)))]
        [(let-shift ,var ,exp)
         (lambda (k)
           (let ([exp (cps exp (lambda (v)
                               (if (eq? v var)
                                   `(lambda  (a)
                                      (lambda (k^) (k^ ,(k `a))))
                                   (env v))))])
             (exp (lambda (x) x))))]
        [(reset ,[exp])
           (lambda (k) (k (exp (lambda (x) x))))]
        [(,[rator] ,[rand])
            (lambda (k)
               (rator (lambda (f)
                         (rand (lambda (a)
                                  `((,f ,a) (lambda (t) ,(k `t))))))))]))))

(define cps
  (lambda (expr)
    ((bcps expr) (lambda (x) x))))


;; Still produce a cps-term (NOTE!  k55 should really be a gensym)
(define k-cps
  (lambda (expr)
  `(lambda (k55) ,((bcps expr) (lambda (x) `(k55 ,x))))))


(define ee
  (lambda (expr)
    (eval (cps expr))))

#!eof

> (ee '(add1 (reset (add1 (add1 (let-shift c (c (c 100))))))))
105

> (cps #f)
(lambda (g35) (g35 #f))

> (ee #f)
#f

> (ee '(let-cc k (zero? (k 5))))
5

;; proof that shift/reset is not a lexicall context catcher
> (ee '((lambda (x) (add1 (reset (x 100))))
        (lambda (n) (add1 (add1 (let-shift c (c (c n))))))))
105


;; properties of shift/reset.  the reset (prompt) is not removed from the
;; stack during capture (see Dybvig, Peyton Jones, Sabry IU Tech Report 615)

;; (2 + (reset (3 + (ls c (1 + (ls d 7))))))
> (ee '(add1 (add1
              (reset (add1 (add1 (add1
                                  (let-shift c (add1
                                                (let-shift d 7))))))))))
9

;; (2 + (reset (3 + (ls c (1 + (ls d (d 7)))))))
> (ee '(add1 (add1
              (reset
               (add1 (add1 (add1
                            (let-shift c (add1
                                          (let-shift d (d 7)))))))))))
10
;; (2 + (reset (3 + (ls c (1 + (ls d (c 7)))))))
> (ee '(add1 (add1
              (reset
               (add1 (add1 (add1
                            (let-shift c (add1
                                          (let-shift d (c 7)))))))))))
12

;; (2 + (reset (3 + (ls c (1 + (ls d (c (d 7))))))))
> (ee '(add1 (add1
              (reset
               (add1 (add1 (add1
                            (let-shift c (add1
                                          (let-shift d (c (d 7))))))))))))
13

