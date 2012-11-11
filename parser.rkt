#lang plai

; Notes:

; I've done nothing about closures.
; On page 29 of Smith, in the middle of the left column, we have 
; "Closures (notated '(CLOSURE: ...}')",
; but I'm not certain how the rest of the closure is supposed to be notated.
; Indeed, if one is to specify an environment in the body of a closure, that raises the
; question of how an environment is supposed to be notated. 
; If not, why is it called a closure?

; We need to be able to distinguish between [] and ()
; The default racket reader spits out a list regardless of which
; parens are used. So instead we use the syntax-read function,
; which returns a syntax object. We then query this syntax object
; to determine which flavour of parens were used.
; This is a fair bit of additional complexity to detect something
; quite minor, but I don't know how else to do this

(require "2lisp.rkt")

; query syntax object to determine paren type
; rail-syn : syn-object -> boolean
(define (rail-syn? syn-obj)
  (eq? #\[ (syntax-property syn-obj 'paren-shape)))

(define (list-syn? syn-obj)
  (and (list? (syntax-e syn-obj))
       (not (rail-syn? syn-obj))))

;; parser :: syn-obj -> struct
(define (parser syn-obj)
  
  (let ((e (syntax-e syn-obj)))
   (cond [(or (eq? e '$t) (eq? e '$T))
         (sboolean #t)]
         [(or (eq? e '$f) (eq? e '$F))
          (sboolean #f)]
         [(number? e)
          (snumeral e)]
         [(symbol? e)
          (satom e)]
         [(list? e)
          (cond 
            [(rail-syn? syn-obj)
             (srail (map parser e))]
            [(and (equal? (syntax-e (first e)) 'quote)
                  (eq? (length (rest e)) 1))
             (shandle (parser (first (rest e))))]
            ; currently (quote 1 2) gets turned into a pair. this is
            ; perhaps not what we want...
            
            ; otherwise we deal with pairs
            ; pairs have the structure: (a . b)
            ;    (a b1 b2 b3 ... bn) is an abbreviation for:
            ;    (a [b1 b2 b3 ... bn])
            [else
             (spair (parser (first e))
                    (srail (map parser (rest e))))])]
         [(pair? e)
          ;this case is to handle things like:
          ; (+ . [1])
          ; for now assume we're dealing with a 2-Lisp pair, and not a rail
          ; I'm not sure what [1 . 2] means, if anything
          (spair (parser (car e))
                 (parser (cdr e)))]
         [else
          (error "parse error" e)])))

(define (run-parser bytes)
  (parser (read-syntax "test" (open-input-bytes bytes))))

(define (test-parser input expected)
  (equal? (run-parser input) expected))

; TESTS

(test-parser #"1" (snumeral 1))
(test-parser #"x" (satom 'x))
(test-parser #"$t" (sboolean #t))
(test-parser #"$T" (sboolean #t))
(test-parser #"$F" (sboolean #f))
(test-parser #"$f" (sboolean #f))
(test-parser #"[1 $t x]" (srail (list (snumeral 1)
                                      (sboolean #t)
                                      (satom 'x))))
(test-parser #"(1)" (spair (snumeral 1)
                           (srail '())))
(test-parser #"(foo bar)" (spair (satom 'foo)
                                 (srail (list (satom 'bar)))))
(test-parser #"(foo 1 2 3 4)" (spair (satom 'foo)
                                     (srail (list (snumeral 1)
                                                  (snumeral 2)
                                                  (snumeral 3)
                                                  (snumeral 4)))))
(test-parser #"'q" (shandle (satom 'q)))
(test-parser #"'1" (shandle (snumeral 1)))
(test-parser #"'[1 2]" (shandle (srail (list (snumeral 1)
                                             (snumeral 2)))))
(test-parser #"(a . [b])" (spair (satom 'a)
                                 (srail (list (satom 'b)))))