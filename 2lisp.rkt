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
(define-type struct
  [snumeral (n number?)]
  [sboolean (b boolean?)]
  [sclosure (v (lambda (x) 
                 (and (srail? x) (andmap satom? (srail-r x)))))
            (b struct?)
            (e procedure?)] 
  [srail (r (candmap struct?))]
  [shandle (s struct?)]
  [spair (l struct?) (r struct?)]
  [satom (a symbol?)])


;; Primitive operators
  
(define-struct primentry (name sig impl))
;; PrimEntry is (make-primentry Sig Impl) where Sig is one of:
;; - #f
;; - Number
;; interp. signature for a primitive operator
;; where false indicates arbitrary number of arguments and a number is a strict
;; number of arguments.

(define (unimplemented . a*)  (error 'reduce "primitive not implemented yet"))

(define PRIMOP-TABLE
  (map
   (lambda (e) (cons (car e) (apply make-primentry e)))
   `((lambda 2 ,unimplemented)
     (zero? 1 ,unimplemented)
     (number? 1 ,unimplemented)
     (truth-value? 1 ,unimplemented)
     (function? 1 ,unimplemented)
     (sequence? 1 ,unimplemented)
     (empty? 1 ,unimplemented)

     (add1 1 ,unimplemented)
     (sub1 1 ,unimplemented)
     (not 1 ,unimplemented)
     (sequence-first 1 ,unimplemented)
     (sequence-rest 1 ,unimplemented)

     (make-pair 2 ,unimplemented)
     (make-rail false ,unimplemented)
     (make-closure 3 ,unimplemented);; To be finished: I don't know how closures are rep'd.
     (make-atom 1 ,unimplemented)
     (quote 1 ,unimplemented) ;; but quote does not evalute its argument!

     (numeral? 1 ,unimplemented)
     (boolean? 1 ,unimplemented)
     (closure? 1 ,unimplemented)
     (rail? 1 ,unimplemented)
     (handle? 1 ,unimplemented)
     (pair? 1 ,unimplemented)
     (atom? 1 ,unimplemented)

     (pair-fst 1 ,unimplemented)
     (pair-snd 1 ,unimplemented)
     (rail-fst 1 ,unimplemented)
     (rail-rst 1 ,unimplemented)

     (up 1 ,unimplemented)
     (dn 1 ,unimplemented)
     (normalise 1 ,unimplemented)
     (reduce 2 ,unimplemented))))

(define (primop? x) (assq x PRIMOP-TABLE)) 


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

;; for now
(define (primop-lookup x) #f)

(define (lookup x env)
  (or (primop-lookup x)
      (env x)))

;; interp : struct -> struct
(define (interp s)
  (normalize s empty-env))


;; normalize : struct Env -> struct
(define (normalize s env)
  (type-case struct s
    [satom (x) (lookup (satom x) env)]
    [spair (rator rand) (reduce rator rand env)]
    [srail (s*) (normalize/rail s env)]
    [shandle (h) s]
    [sclosure (v b e) s]
    [snumeral (n) s]
    [sboolean (b) s]))

;; normalize/rail : srail Env -> srail
(define (normalize/rail s env)
  (srail (map (lambda (s) (normalize s env)) (srail-r s))))

;; reduce: struct struct Env -> struct
(define (reduce rator rand env)
  (cond
   [(and (satom? rator) (eq? (satom-a rator) 'lambda))
    ;;(lambda . [[x ...] body])
    ;; RG - add more checks later
    ;; rand = [[x ...] body]
    (let* ([parts (srail-r rand)]
           [params (car parts)]
           [body (cadr parts)])
      (sclosure params body env))]
   [(primop? rator) (error 'reduce "go away")]
   [else
    (let ([ratorv (normalize rator env)]
          [randv (normalize/rail rand env)])
      (if (sclosure? ratorv)
          (normalize (sclosure-b ratorv)
                     (extend-env (sclosure-v ratorv) randv env))
          (error "Attempted to apply a parameter to a non-function expression.")))]))

;-----------------
;===== TESTS =====
;-----------------

(define id (spair (satom 'lambda)
                  (srail (list 
                          (srail (list (satom 'x))) 
                          (satom 'x)))))

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
    [sboolean (b) s]))

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
    [(primop? rator) (error 'reduce "go away")]
    [else
     (spair (unload rator env) (unload rand env))]))

;;; interp tests
(define-syntax chk-exn
  (syntax-rules ()
    [(_ exp msg) (check-exn exn:fail? (lambda () exp) msg)]))

(define tests 
  (test-suite "interp tests"
; A few basic ones
(check-equal? (interp (snumeral 1)) (snumeral 1))


#;(check-equal? (interp id) #f)
;(check-equal? (interp (add-1 (num 1))) (num 2))
;(chk-exn (interp (id 'x)) "free identifier")
;(check-equal? (interp (fun 'x (id 'x))) (fun 'x (id 'x)))
;(check-equal? (interp (app (fun 'x (id 'x)) (num 2))) (num 2))

; More complicated tests
;(check-equal? (interp (fun 'x (num 5))) (fun 'x (num 5)))
;(check-equal? (interp (app (fun 'x (num 5)) (num 2))) (num 5))
;(chk-exn (interp (app (fun 'x (id 'y)) (num 2))) "free identifier")
;(check-equal? (interp (app (fun 'x (id 'x)) (add-1 (num 2)))) (num 3))

)); define tests

(run-tests tests)
