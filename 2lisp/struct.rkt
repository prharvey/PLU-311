#lang plai

(require rackunit)
(require rackunit/text-ui)


;; struct.rkt :  2LISP: The structural field
;; Part of an interpreter for the 2LISP Language of Brian Cantwell Smith

;; Ron Garcia
;; Zach Drudi


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
;; structures).  We'll refer to the ones named make-*" as "constructors" since
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

(define environment? procedure?)

(define-type struct
  [snumeral (n number?)]
  [sboolean (b boolean?)]
  [sclosure (params (lambda (x) 
                 (and (srail? x) (andmap satom? (srail-r x)))))
            (body struct?)
            (env environment?)] 
  [srail (r (candmap struct?))]
  [shandle (h struct?)]
  [spair (l struct?) (r struct?)]
  [satom (a symbol?)]
  [snative (name symbol?) (proc procedure?)])


; query syntax object to determine paren type
; rail-syn : syn-object -> boolean
(define (rail-syn? syn-obj)
  (eq? #\[ (syntax-property syn-obj 'paren-shape)))

(define (list-syn? syn-obj)
  (and (list? (syntax-e syn-obj))
       (not (rail-syn? syn-obj))))

;; parser : syn-obj -> syn-object
(define (parse-syntax syn-obj)
  
  (let ((e (syntax-e syn-obj)))
   (cond [(or (eq? e '$t) (eq? e '$T))
          #'(sboolean #t)]
         [(or (eq? e '$f) (eq? e '$F))
          #'(sboolean #f)]
         [(boolean? e) ;; support Scheme Booleans as well as 2-Lisp Booleans
          #`(sboolean #,syn-obj)]
         [(number? e)
          #`(snumeral #,syn-obj)]
         [(symbol? e)
          #`(satom (quote #,syn-obj))]
         [(list? e) 
          (cond 
            [(rail-syn? syn-obj)
             #`(srail (list #,@(map parse-syntax e)))]
            [(equal? (syntax-e (first e)) 'quote)
             (if (eq? (length (rest e)) 1)
                 #`(shandle #,(parse-syntax (first (rest e))))
                 (error "parse error" e))]
            ; otherwise we deal with pairs
            ; pairs have the structure: (a . b)
            ;    (a b1 b2 b3 ... bn) is an abbreviation for:
            ;    (a [b1 b2 b3 ... bn])
            [else
             #`(spair #,(parse-syntax (first e))
                    (srail (list #,@(map parse-syntax (rest e)))))])]
         [(and (pair? e) 
               (not (eq? (syntax-e (car e)) 'quote))
               (not (rail-syn? syn-obj)))
          ; This case is to handle things like:
          ; (+ . [1])
          ; A square-bracketed, like [1 . 2] is an error
          #`(spair #,(parse-syntax (car e))
                   #,(parse-syntax (cdr e)))]
         [else
          (error "parse error" e)])))


;; Test Suite

(define (syn-eq? a b) (equal? (syntax->datum a) (syntax->datum b)))
  
(define tests
  (test-suite "parser tests"
    (check syn-eq? (parse-syntax #'1) #'(snumeral 1))
  
    (check syn-eq? (parse-syntax #'x) #'(satom 'x))
    (check syn-eq? (parse-syntax #'$t) #'(sboolean #t))
    (check syn-eq? (parse-syntax #'$T) #'(sboolean #t))
    (check syn-eq? (parse-syntax #'$F) #'(sboolean #f))
    (check syn-eq? (parse-syntax #'$f) #'(sboolean #f))
    (check syn-eq? (parse-syntax #'[1 $t x]) #'(srail (list (snumeral 1)
                                                         (sboolean #t)
                                                         (satom 'x))))
    (check syn-eq? (parse-syntax #'(1)) #'(spair (snumeral 1)
                                              (srail (list))))
    (check syn-eq? (parse-syntax #'(foo bar)) #'(spair (satom 'foo)
                                                    (srail (list (satom 'bar)))))
    (check syn-eq? (parse-syntax #'(foo 1 2 3 4)) #'(spair (satom 'foo)
                                                        (srail (list (snumeral 1)
                                                                     (snumeral 2)
                                                                     (snumeral 3)
                                                                     (snumeral 4)))))
    (check syn-eq? (parse-syntax #''q) #'(shandle (satom 'q)))
    (check syn-eq? (parse-syntax #''1) #'(shandle (snumeral 1)))
    (check syn-eq? (parse-syntax #''[1 2]) #'(shandle (srail (list (snumeral 1)
                                                                (snumeral 2)))))
    (check syn-eq? (parse-syntax #'(a . [b])) #'(spair (satom 'a)
                                                    (srail (list (satom 'b)))))
    
    (check-exn exn:fail? (lambda () (parse-syntax #'(quote . 2))))
    (check-exn exn:fail? (lambda () (parse-syntax #'(quote 2 3))))
    
)) ; define tests

;(run-tests tests)