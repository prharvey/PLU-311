#lang plai

(require rackunit)
(require rackunit/text-ui)

;(require (for-syntax "struct.rkt"))
(require "struct.rkt")

;;
;; Environments
;;
;; Env is satom -> struct

;; empty-env : Env
(define empty-env
  (lambda (x)
    (error (format "free identifier ~a" x))))

;; extend-env : srail srail Env -> Env
(define (extend-env p v env)
  (let ([alist (map cons (srail-r p) (srail-r v))])	    
    (lambda (a)
      (cond
       [(assoc a alist) => cdr]
       [else (env a)]))))

(define (lookup x env) (env x))

;; Primitive/native operators
  
(define-struct primentry (name sig impl))
;; PrimEntry is (list Symbol (Number or #f) Boolean(Struct* ... Env -> Struct)) 
;; interp. An entry for a primitive interpreter operation
;; (list name arg norm proc) gives the name of the operator, the number of args (#f for arbitrary),
;; a boolean for whether the arguments should be pre-normalised and
;; a native function that implements it. 

(define (unimplemented . a*)  (error 'reduce "primitive not implemented yet"))


(define PRIMOP-TABLE
  `((lambda 2 #f ,(lambda (params body env) (sclosure params body env)))
    (if 3 #f ,(lambda (pred conseq altern env)
               (let ([answ (normalize pred env)])
                 (if (sboolean-b answ)
                     (normalize conseq env)
                     (normalize altern env)))))
    (zero? 1 #t ,(lambda (s env)
                   (sboolean (and (snumeral? s) (zero? (snumeral-n s))))))
    (number? 1 #t ,(lambda (s env) (sboolean (snumeral? s))))
    (truth-value? 1 #t ,(lambda (s env) (sboolean (sboolean? s))))
    (function? 1 #t ,(lambda (s env) 
                       (sboolean (or (sclosure? s) (snative? s)))))
    (sequence? 1 #t ,(lambda (s env) (sboolean (srail? s))))
    (empty? 1 #t ,(lambda (s env) (sboolean (and (srail? s) (zero? (length (srail-r s)))))))
    
    (add1 1 #t ,(lambda (s env) (snumeral (add1 (snumeral-n s)))))
    (sub1 1 #t ,(lambda (s env) (snumeral (sub1 (snumeral-n s)))))
    (not 1 #t ,(lambda (s env) (sboolean (not (sboolean-b s)))))
    (sequence-first 1 #t ,unimplemented)
    (sequence-rest 1 #t ,unimplemented)
    
    (make-pair 2 #t ,(lambda (srator srand env) (shandle 
                                                 (spair (shandle-h srator) (shandle-h srand)))))
    (make-rail false #t ,(lambda (sr env)
                           (shandle (srail (map shandle-h (srail-r sr))))))
    (make-closure 3 #t ,(lambda (shparams shbody senv env)
                          (shandle (sclosure (shandle-h shparams)
                                             (shandle-h shbody) 
                                             (reflect-env senv env)))))
    (make-atom 1 #t ,unimplemented)
    (quote 1 #f ,(lambda (s env) (shandle s))) ;;quote does not evalute its argument!
    
    (numeral? 1 #t ,unimplemented)
    (boolean? 1 #t ,unimplemented)
    (closure? 1 #t ,unimplemented)
    (rail? 1 #t ,unimplemented)
    (handle? 1 #t ,unimplemented)
    (pair? 1 #t ,(lambda (s env) (sboolean (and (shandle? s) (spair? (shandle-h s))))))
    (atom? 1 #t ,unimplemented)
    
    (pair-fst 1 #t ,unimplemented)
    (pair-snd 1 #t ,unimplemented)
    (rail-fst 1 #t ,unimplemented)
    (rail-rst 1 #t ,unimplemented)
    (closure-params 1 #t ,(lambda (sh env) (shandle (sclosure-params (shandle-h sh)))))
    (closure-body 1 #t ,(lambda (sh env) (shandle (sclosure-body (shandle-h sh)))))
    (closure-env 1 #t ,(lambda (sh env) (reify-env (sclosure-env (shandle-h sh)))))
    
    (up 1 #t ,(lambda (s env) (shandle s)))
    (dn 1 #t ,(lambda  (s env) (if (normal? (shandle-h s))
                                   (shandle-h s)
                                   (error 'normalize "Cannot dn a non-normal structure"))))
    (normalise 1 #t ,unimplemented)
    (reduce 2 #t ,unimplemented)))

(define (make-native-fn name args norm? proc)
  (snative name
           (lambda (rail env)
             (let ([rail (if norm? (normalize/rail rail env) rail)])
               (if (number? args)
                   ;; could check args here!
                   (apply proc (append (srail-r rail) (list env)))
                   (proc rail env))))))

;; the toplevel environment, with all the primitive operations
(define top-env
  (extend-env (srail (map (lambda (e) (satom (car e))) PRIMOP-TABLE))
              (srail (map (lambda (e) (apply make-native-fn e)) PRIMOP-TABLE))
              empty-env))


;;
;; The Interpreter
;;

;; interp : struct -> struct
(define (interp-struct s)
  (normalize s top-env))


;; normalize : struct Env -> struct
(define (normalize s env)
  (type-case struct s
    [snumeral (n) s]
    [sboolean (b) s]
    [sclosure (v b e) s]
    [srail (s*) (normalize/rail s env)]
    [shandle (h) s]
    [spair (rator rand) (reduce rator rand env)]
    [satom (x) (lookup (satom x) env)]
    [snative (name proc) s]))

;; normalize/rail : srail Env -> srail
(define (normalize/rail s env)
  (srail (map (lambda (s) (normalize s env)) (srail-r s))))

;; reduce: struct struct Env -> struct
(define (reduce rator rand env)
  (let ([ratorv (normalize rator env)])
    (cond 
      [(sclosure? ratorv)
       (let ([randv (normalize/rail rand env)])
         (normalize (sclosure-body ratorv)
                    (extend-env (sclosure-params ratorv) randv (sclosure-env ratorv))))]
      [(snative? ratorv) ((snative-proc ratorv) rand env)]
      [else (error "Attempted to apply a parameter to a non-function expression.")])))

;; Is the given structure normal?
(define (normal? s)
  (type-case struct s
    [snumeral (n) #t]
    [sboolean (b) #t]
    [sclosure (v b e) #t]
    [srail (s*) (andmap normal? s*)]
    [shandle (h) #t]
    [spair (rator rand) #f]
    [satom (x) #f]
    [snative (name proc) #t]))

;; Turn an environment into a native 2Lisp procedure
;; Note that the environment takes a quoted atom name
;; and returns a quoted version of its underlying structure
(define (reify-env env)
  (make-native-fn 'an-env 1 #t
                  (lambda (s env^) (shandle (env (shandle-h s))))))

;; Turn a 2Lisp procedure into an environment.  The procedure had better accept
;; quoted atoms and return quoted normal structures (which we can unquote).
;; reflect-env : (sclosure or snative) -> (satom -> struct)
(define (reflect-env senv env)
  (lambda (s) 
    (shandle-h
     (reduce senv (srail (list (shandle s))) env))))


(define (2lisp->struct stx)
  (eval (syntax->datum (parse-syntax stx))))

(define-syntax (parse x)
  (syntax-case x ()
    [(_ exp) #'(2lisp->struct #'exp)]))


(define-syntax interp
  (syntax-rules () 
    [(interp e) (interp-struct (parse e))]))

(define (interp-2lisp s)
  (interp-struct (2lisp->struct s)))



;; unload : struct -> struct
;; turn closures into substituted lambdas
(define (unload s env)
  (type-case struct s
    [satom (x) (lookup (satom x) env)]
    [spair (rator rand) (unload/pair rator rand env)]
    [srail (s*) (srail (map (lambda (s) (unload s env)) (srail-r s*)))]
    [shandle (h) s]
    [sclosure (p b e) (spair (satom 'lambda)
                             (srail (list
                                     p 
                                     (unload b (extend-env p p e)))))]
    [snumeral (n) s]
    [sboolean (b) s]
    [snative (name proc) s]))

(define (unload/pair rator rand env)
  (cond
    [(and (satom? rator) (eq? (satom-a rator) 'lambda))
     ;;(lambda . [[x ...] body])
     ;; RG - add more checks later
     ;; rand = [[x ...] body]
     (let* ([parts (srail-r rand)]
            [params (car parts)]
            [body (cadr parts)])
       ;; perhaps a bit too clever
       (unload (sclosure params body env) env))]
    [else
     (spair (unload rator env) (unload rand env))]))


;-----------------
;===== TESTS =====
;-----------------

(define id (spair (satom 'lambda)
                  (srail (list 
                          (srail (list (satom 'x))) 
                          (satom 'x)))))
(define id2 #'(lambda [x] x))

(define k (spair (satom 'lambda)
                  (srail (list 
                          (srail (list (satom 'x))) 
                          (spair (satom 'lambda)
                                 (srail (list 
                                         (srail (list (satom 'y))) 
                                         (satom 'x))))))))
  
(define k2 #'(lambda [x] (lambda [y] x)))
                          

;;; interp tests
(define interp-tests 
  (test-suite "interp tests"

(check-equal? (interp 1) (snumeral 1))

(check-equal? (unload (interp-struct id) empty-env)
              id)

(check-equal? (unload (interp-2lisp id2) empty-env)
              (2lisp->struct id2))


(check-equal? (unload (interp-struct k) empty-env)
              k)

(check-equal? (interp-struct (spair id (srail (list (snumeral 2)))))
              (snumeral 2))
(check-equal? (interp-struct (spair (spair k (srail (list (snumeral 2)))) (srail (list (snumeral 3)))))
              (snumeral 2))
(check-equal? (interp-struct (spair (satom 'if)
                             (srail (list (sboolean #t) (snumeral 5) (snumeral 6)))))
              (snumeral 5))

(check-equal? (interp-struct (spair (satom 'if)
                             (srail (list (sboolean #f) (snumeral 5) (snumeral 6)))))
              (snumeral 6))

(check-equal? (interp-struct (spair (satom 'zero?) (srail (list (snumeral 0)))))
              (sboolean #t))
(check-equal? (interp-struct (spair (satom 'zero?) (srail (list (snumeral 7)))))
              (sboolean #f))
(check-equal? (interp-struct (spair (satom 'zero?) (srail (list (sboolean #t)))))
              (sboolean #f))
(check-equal? (interp-struct (spair (satom 'number?) (srail (list (snumeral 7)))))
              (sboolean #t))
(check-equal? (interp-struct (spair (satom 'number?) (srail (list (sboolean #t)))))
              (sboolean #f))
(check-equal? (interp-struct (srail (list (spair id (srail (list (snumeral 2))))
                                   (spair (spair k (srail (list (snumeral 2))))
                                          (srail (list (snumeral 3))))
                                   (sboolean #t))))
              (srail (list (snumeral 2) (snumeral 2) (sboolean #t))))

(check-equal? (interp-struct (spair (satom 'function?) (srail (list id))))
              (sboolean #t))
(check-equal? (interp-struct (spair (satom 'function?) (srail (list (satom 'zero?)))))
              (sboolean #t))
(check-equal? (interp-struct (spair (satom 'function?) (srail (list (sboolean #t)))))
              (sboolean #f))
(check-equal? (interp-struct (spair (satom 'up) 
                             (srail (list (spair (satom 'zero?)
                                                  (srail (list (snumeral 0))))))))
              (shandle (sboolean #t)))
(check-exn exn:fail? 
           ;; "Cannot dn a non-normal structure"
           (lambda () 
             (interp-struct (spair (satom 'dn) (srail (list (shandle (satom 'x))))))))
(check-equal? (interp-struct (spair (satom 'dn) (srail (list (shandle (snumeral 6))))))
              (snumeral 6))

(check-equal? (interp-struct (spair (satom 'make-pair)
                             (srail (list (shandle (satom 'zero))
                                          (shandle (srail (list (snumeral 5))))))))
              (shandle (spair (satom 'zero) (srail (list (snumeral 5))))))

(check-equal? (interp-struct (spair (satom 'pair?) (srail (list (snumeral 7)))))
              (sboolean #f))
(check-equal? (interp-struct (spair (satom 'pair?) 
                             (srail (list (shandle (spair (satom 'f)
                                                          (srail (list (snumeral 7)))))))))
              (sboolean #t))

(check-equal? (interp-struct (spair (satom 'closure-params)
                             (srail (list (spair (satom 'up) (srail (list id)))))))
              (shandle (srail (list (satom 'x)))))
(check-equal? (interp-struct (spair (satom 'quote) (srail (list (snumeral 5)))))
              (shandle (snumeral 5)))

(check-equal? (interp-struct (spair (satom 'add1) (srail (list (snumeral 5)))))
              (snumeral 6))
(check-equal? (interp-struct (spair (satom 'sub1) (srail (list (snumeral 5)))))
              (snumeral 4))
(check-equal? (interp-struct (spair (satom 'not) (srail (list (sboolean #f)))))
              (sboolean #t))
(check-equal? (interp-struct (spair (satom 'make-rail)
                             (srail (list (shandle (sboolean #f))
                                          (shandle (snumeral 6))))))
              (shandle (srail (list (sboolean #f) (snumeral 6)))))

;; test code: ((dn (make-closure '[y] '(add1 x) (closure-env (up (k 5))))) 30)
;; transforms function (k 5) up into a closure, steals its environment, builds a new closure, and 
;; transforms that closure down into a function and calls it with 30.
(check-equal? 
 (local [(define (app s1 . s*) (spair s1 (srail s*)))]
   (interp-struct 
    (app
     (app (satom 'dn)
          (app (satom 'make-closure)
               (shandle (srail (list (satom 'y))))
               (shandle (app (satom 'add1) (satom 'x))) ;; '(add1 x)
               (app (satom 'closure-env) 
                    (app (satom 'up)
                         (app k (snumeral 5))))))
     (snumeral 30))))
   (snumeral 6))

(check-exn exn:fail? (lambda () (interp-struct (satom 'x))));;  "free identifier"



; More complicated tests
;(check-equal? (interp (fun 'x (num 5))) (fun 'x (num 5)))
;(check-equal? (interp (app (fun 'x (num 5)) (num 2))) (num 5))
;(chk-exn (interp (app (fun 'x (id 'y)) (num 2))) "free identifier")
;(check-equal? (interp (app (fun 'x (id 'x)) (add-1 (num 2)))) (num 3))

)); define tests

;(run-tests interp-tests)