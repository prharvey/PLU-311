#lang plai
(require rackunit)
(require rackunit/text-ui)

;; 2lisp.rkt : An interpreter for the 2LISP Language
;; of Brian Cantwell Smith


;; Internal Structures:
;; Numerals
;; Booleans
;; Closures
;; Rails
;; Handles

;; Pairs
;; Atoms

;; Designated Objects:
;; Numbers (by Numerals)
;; Truth Values (by Booleans)
;; Functions (by Closures)
;; Sequences (by Rails)
;; All of the Internal Structures (by Handles)

;; Pairs designate the designant of the result of applying the function
;; designated by the first element to the object designated by the second
;; element

;; Atoms designate the object that results from looking up the name in the
;; environment.  Note that there is a difference between an atom and a quoted
;; atom.  

;; A simple environment-passing interpreter


;; Expressions:


;; The Basics:

;; 2-Lisp treats lambda as an application though.

;; This is the primitive structure of 2-Lisp
;; e ::= ...
;;    | n            ;; numerals
;;    | #t | #f      ;; booleans
;;    | #<closure>   ;; closures : not sure how to represent them yet
;;    | [ ... ]      ;; rails
;;    | (e e)        ;; pair
;;    | (quote e)    ;; handles 
;;    | x            ;; atoms

;; Some value predicates:
;; e ::= ...
;;    | (zero? e) 
;;    | (number? e)
;;    | (truth-value? e)
;;    | (function? e)
;;    | (sequence? e)
;;    | (empty? e)

;; Some value operators and selectors
;; e ::= ...
;;    | (add1 e) | (sub1 e) | (not e)
;;    | (sequence-first e)
;;    | (sequence-rest e)


;; Handles:

;; The following operators produce handles (i.e. expressions that designate
;; structures).  We'll refer to the ones named "make-*" as "constructors" since
;; they use standard interpretation on their arguments.  quote, on the other
;; hand, does not evaluate its argument (perhaps it checks its arguments for
;; well-formedness, but it may not need to: it may be impossible to write an
;; ill-formed expression there).

;; e ::= ...
;;    | (make-pair e e)
;;    | (make-rail e ...)
;;    | (make-closure e ...)
;;    | (make-atom e)
;;    | (quote e)
;; Note there is no:
;;  - make-boolean : we'll have (up #t) and (up #f) for those
;;  - make-numeral : we'll have (up n) for those.
;;  - make-handle : we quote and we'll have up for that. 

;; e.g. (make-pair (quote a) (quote b)) ==> (quote (a . b))
;; I have no idea how make-atom works, i.e. what it takes.
;; (make-rail (quote x) (quote y) (quote z)) ==> (quote [x y z])


;; The following predicates will only be true for handles (expressions that
;; designate structures):

;; e ::= ...
;;    | (numeral? e)
;;    | (boolean? e)
;;    | (closure? e)
;;    | (rail? e)
;;    | (handle? e)
;;    | (pair? e)
;;    | (atom? e)

;; e.g.: (numeral? '3) => #t
;; (handle? ''3) => #t


;; The following are selectors on handles
;; e ::= ...
;;    | (pair-fst e)
;;    | (pair-snd e)
;;    | (rail-fst e)  ;; Note Smith unifies rail and sequence selectors
;;    | (rail-rst e)

;; Metastructural operations
;; e ::= ...
;;   | (up e)
;;   | (dn e)
;;   | (normalise e)
;;   | (reduce e e)   


;; curried andmap helper function
(define (candmap p)
  (lambda (x*) (andmap p x*)))



;; The internal structures, which are also the internal representation of the
;; language: struct plays the role that expr typically takes in an interpreter
;; Added one new struct: snative for native code, which is a function that takes the current
;; arguments and the environment.  As per des Rivieres,
;; this is essentially level shifting, which is related to reflection.
(define-type struct
  [snumeral (n number?)]
  [sboolean (b boolean?)]
  [sclosure (params (lambda (x) 
                 (and (srail? x) (andmap satom? (srail-r x)))))
            (body struct?)
            (env procedure?)] 
  [srail (r (candmap struct?))]
  [shandle (h struct?)]
  [spair (l struct?) (r struct?)]
  [satom (a symbol?)]
  [snative (name symbol?) (proc procedure?)])


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
(define (interp s)
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

;-----------------
;===== TESTS =====
;-----------------

(define id (spair (satom 'lambda)
                  (srail (list 
                          (srail (list (satom 'x))) 
                          (satom 'x)))))

(define k (spair (satom 'lambda)
                  (srail (list 
                          (srail (list (satom 'x))) 
                          (spair (satom 'lambda)
                                 (srail (list 
                                         (srail (list (satom 'y))) 
                                         (satom 'x))))))))
  
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

;;; interp tests
(define tests 
  (test-suite "interp tests"

(check-equal? (interp (snumeral 1)) (snumeral 1))

(check-equal? (unload (interp id) empty-env)
              id)

(check-equal? (unload (interp k) empty-env)
              k)

(check-equal? (interp (spair id (srail (list (snumeral 2)))))
              (snumeral 2))
(check-equal? (interp (spair (spair k (srail (list (snumeral 2)))) (srail (list (snumeral 3)))))
              (snumeral 2))
(check-equal? (interp (spair (satom 'if)
                             (srail (list (sboolean #t) (snumeral 5) (snumeral 6)))))
              (snumeral 5))

(check-equal? (interp (spair (satom 'if)
                             (srail (list (sboolean #f) (snumeral 5) (snumeral 6)))))
              (snumeral 6))

(check-equal? (interp (spair (satom 'zero?) (srail (list (snumeral 0)))))
              (sboolean #t))
(check-equal? (interp (spair (satom 'zero?) (srail (list (snumeral 7)))))
              (sboolean #f))
(check-equal? (interp (spair (satom 'zero?) (srail (list (sboolean #t)))))
              (sboolean #f))
(check-equal? (interp (spair (satom 'number?) (srail (list (snumeral 7)))))
              (sboolean #t))
(check-equal? (interp (spair (satom 'number?) (srail (list (sboolean #t)))))
              (sboolean #f))
(check-equal? (interp (srail (list (spair id (srail (list (snumeral 2))))
                                   (spair (spair k (srail (list (snumeral 2))))
                                          (srail (list (snumeral 3))))
                                   (sboolean #t))))
              (srail (list (snumeral 2) (snumeral 2) (sboolean #t))))

(check-equal? (interp (spair (satom 'function?) (srail (list id))))
              (sboolean #t))
(check-equal? (interp (spair (satom 'function?) (srail (list (satom 'zero?)))))
              (sboolean #t))
(check-equal? (interp (spair (satom 'function?) (srail (list (sboolean #t)))))
              (sboolean #f))
(check-equal? (interp (spair (satom 'up) 
                             (srail (list (spair (satom 'zero?)
                                                  (srail (list (snumeral 0))))))))
              (shandle (sboolean #t)))
(check-exn exn:fail? 
           ;; "Cannot dn a non-normal structure"
           (lambda () 
             (interp (spair (satom 'dn) (srail (list (shandle (satom 'x))))))))
(check-equal? (interp (spair (satom 'dn) (srail (list (shandle (snumeral 6))))))
              (snumeral 6))

(check-equal? (interp (spair (satom 'make-pair)
                             (srail (list (shandle (satom 'zero))
                                          (shandle (srail (list (snumeral 5))))))))
              (shandle (spair (satom 'zero) (srail (list (snumeral 5))))))

(check-equal? (interp (spair (satom 'pair?) (srail (list (snumeral 7)))))
              (sboolean #f))
(check-equal? (interp (spair (satom 'pair?) 
                             (srail (list (shandle (spair (satom 'f)
                                                          (srail (list (snumeral 7)))))))))
              (sboolean #t))

(check-equal? (interp (spair (satom 'closure-params)
                             (srail (list (spair (satom 'up) (srail (list id)))))))
              (shandle (srail (list (satom 'x)))))
(check-equal? (interp (spair (satom 'quote) (srail (list (snumeral 5)))))
              (shandle (snumeral 5)))

(check-equal? (interp (spair (satom 'add1) (srail (list (snumeral 5)))))
              (snumeral 6))
(check-equal? (interp (spair (satom 'sub1) (srail (list (snumeral 5)))))
              (snumeral 4))
(check-equal? (interp (spair (satom 'not) (srail (list (sboolean #f)))))
              (sboolean #t))
(check-equal? (interp (spair (satom 'make-rail)
                             (srail (list (shandle (sboolean #f))
                                          (shandle (snumeral 6))))))
              (shandle (srail (list (sboolean #f) (snumeral 6)))))

;; test code: ((dn (make-closure '[y] '(add1 x) (closure-env (up (k 5))))) 30)
;; transforms function (k 5) up into a closure, steals its environment, builds a new closure, and 
;; transforms that closure down into a function and calls it with 30.
(check-equal? 
 (local [(define (app s1 . s*) (spair s1 (srail s*)))]
   (interp 
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

(check-exn exn:fail? (lambda () (interp (satom 'x))));;  "free identifier"



; More complicated tests
;(check-equal? (interp (fun 'x (num 5))) (fun 'x (num 5)))
;(check-equal? (interp (app (fun 'x (num 5)) (num 2))) (num 5))
;(chk-exn (interp (app (fun 'x (id 'y)) (num 2))) "free identifier")
;(check-equal? (interp (app (fun 'x (id 'x)) (add-1 (num 2)))) (num 3))

)); define tests

(run-tests tests)