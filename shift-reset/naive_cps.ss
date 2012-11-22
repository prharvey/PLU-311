;;
;; naive_cps.ss - Taken from the paper "Abstracting Control" by
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

(define cps
  (lambda (exp)
    (let cps ([exp exp] [env (lambda (x) x)])
      (match exp
        [,var (guard (symbol? var))
              (with-gensyms (k)
                `(lambda (,k) (,k ,(env var))))]
        [,num (guard (number? num))
              (with-gensyms (k)
                `(lambda (,k) (,k ,num)))]
        [,bool (guard (boolean? bool))
              (with-gensyms (k)
                `(lambda (,k) (,k ,bool)))]        
        [(zero? ,[exp])
         (with-gensyms (k a)
           `(lambda (,k) (,exp (lambda (,a) (,k (zero? ,a))))))]
        [(sub1 ,[exp])
         (with-gensyms (k a)
           `(lambda (,k) (,exp (lambda (,a) (,k (sub1 ,a))))))]
        [(add1 ,[exp])
         (with-gensyms (k a)
           `(lambda (,k) (,exp (lambda (,a) (,k (add1 ,a))))))]
        [(if ,[pred] ,[conseq] ,[altern])
         (with-gensyms (k b)
           `(lambda (,k) (,pred (lambda (,b)
                                  (if b (,conseq k) (,altern k))))))]
        [(lambda (,var) ,[exp])
         (with-gensyms (k)
           `(lambda (,k) (,k (lambda (,var) ,exp))))]
        [(let-cc ,var ,exp)
         (with-gensyms (k)
           (let ([exp (cps exp (lambda (v)
                               (if (eq? v var)
                                   `(lambda  (a)
                                      (lambda (k^) (,k a)))
                                   (env v))))])
             `(lambda (,k) (,exp ,k))))]
        [(let-shift ,var ,exp)
         (with-gensyms (k)
           (let ([exp (cps exp (lambda (v)
                               (if (eq? v var)
                                   `(lambda  (a)
                                      (lambda (k^) (k^ (,k a))))
                                   (env v))))])
             `(lambda (,k) (,exp (lambda (x) x)))))]
        [(reset ,[exp])
         (with-gensyms (k)
           `(lambda (,k) (,k (,exp (lambda (x) x)))))]
        [(,[rator] ,[rand])
         (with-gensyms (k f a)
            `(lambda (,k)
               (,rator (lambda (,f)
                         (,rand (lambda (,a)
                                  ((,f ,a) ,k)))))))]))))

(define ee
  (lambda (expr)
    ((eval (cps expr)) (lambda (x) x))))

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
