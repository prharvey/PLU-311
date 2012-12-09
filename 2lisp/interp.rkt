#lang plai

(require rackunit)
(require rackunit/text-ui)

(require "struct.rkt")

;;
;; Primitive/native operators
;;

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


;;
;; Interfaces to the 2Lisp Parser
;;

(define (2lisp->struct stx)
  (eval-syntax (parse-syntax stx)))

(define-syntax parse
  (syntax-rules ()
    [(_ e) (2lisp->struct #'e)]))

(define (interp-2lisp s)
  (interp-struct (2lisp->struct s)))

(define-syntax interp
  (syntax-rules () 
    [(interp e) (interp-2lisp #'e)]))





;; unload : struct -> struct
;; turn closures into substituted lambdas
(define (unload s env)
  (type-case struct s
    [satom (x) (lookup (satom x) env)]
    [spair (rator rand) (unload/pair rator rand env)]
    [srail (s*) (srail (map (lambda (s) (unload s env)) s*))]
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

(define id #'(lambda [x] x))

(define k #'(lambda [x] (lambda [y] x)))
                          

(define (ieq? l r)
  (equal? (unload l empty-env)
          (2lisp->struct r)))

;;; interp tests
(define interp-tests 
  (test-suite "interp tests"
              
(check ieq? (interp 1) #'1)
(check ieq? (interp-2lisp id) id)

(check ieq? (interp-2lisp k) k)

(check ieq? (interp ((lambda [x] x) 2)) #'2)

(check ieq? (interp (((lambda [x] (lambda [y] x)) 2) 3)) #'2)

(check ieq? (interp (if $f 5 6)) #'6)

(check ieq? (interp (zero? 0)) #'$t)

(check ieq? (interp (zero? 7)) #'$f)

(check ieq? (interp (zero? $t)) #'$f)

(check ieq? (interp (number? 7)) #'$t)

(check ieq? (interp (number? $t)) #'$f)

(check ieq? (interp [((lambda [x] x) 2) (((lambda [x] (lambda [y] x)) 2) 3) $t]) #'[2 2 $t])
       
(check ieq? (interp (function? (lambda [x] x))) #'$t)

(check ieq? (interp (function? zero?)) #'$t)

(check ieq? (interp (function? $t)) #'$f)

(check ieq? (interp (up (zero? 0))) #''$t)

(check-exn exn:fail? 
           ;; "Cannot dn a non-normal structure"
           (lambda () (interp (dn 'x))))

(check ieq? (interp (dn '6)) #'6)


(check ieq? (interp (make-pair 'zero '[5])) #''(zero 5))

(check ieq? (interp (pair? 7)) #'$f)

(check ieq? (interp (pair? '(f 7))) #'$t)

(check ieq? (interp (closure-params (up (lambda [x] x)))) #''[x])

(check ieq? (interp '5) #''5)

(check ieq? (interp (add1 5)) #'6)

(check ieq? (interp (sub1 5)) #'4)

(check ieq? (interp (not $f)) #'$t)

(check ieq? (interp (make-rail '$f '6)) #''[$f 6])


;; Fancy metastructural test:
;; transforms function (k 5) up into a closure, steals its environment, builds a new closure, and 
;; transforms that closure down into a function and calls it with 30.
(check ieq? (interp ((dn (make-closure '[y] '(add1 x)
                          (closure-env (up ((lambda [x] (lambda [y] x)) 5))))) 30))
       #'6)

(check-exn exn:fail? ;;  "free identifier"
           (lambda () (interp x)))


(check ieq? (interp (lambda [x] 5)) #'(lambda [x] 5))

(check ieq? (interp ((lambda [x] 5) 2)) #'5)

(check-exn exn:fail? '' "free identifier"
           (lambda () (interp ((lambda [x] y) 2))))

(check ieq? (interp ((lambda [x] x) (add1 2))) #'3)

)); define tests

;(run-tests interp-tests)