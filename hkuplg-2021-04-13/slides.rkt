#lang slideshow

;; Title
(slide
 (t "A Taste of Racket and PLT Redex"))

;; TOC
(slide
 #:title "Table of Contents"
 (item "A quick intro to Racket")
 (item "PLT Redex: play with languages"))


(require slideshow/code)
(slide
 #:title "Scheme Basics"
 (para "Function Abstraction")
 'next
 (para (code (λ (x y)
               (+ x y))))
 'next
 (para "Function Application")
 'next
 (para (code ((λ (x)
                (+ x 1)) 2)))
 'next
 (para "Naming")
 'next
 (para (code (define foo 1)
             (define add1
               (λ (x)
                 (+ x 1)))))
 )

(slide
 #:title "Macro System"
 (para (code
             (define-syntax (time-it stx)
               (syntax-parse stx
                 [(_ task)
                  #'(thunk-time-it (λ () task))]))
             
             (define (thunk-time-it task)
               (define before (cim))
               (define answer (task))
               (define delta  (- (cim) before))
               (printf "time: ~a ms\n" delta)
               answer)
             (define cim current-inexact-milliseconds))
       )
 'next
 (para "Run it")
 (code (time-it (add1 2)))
 )

(slide
 #:title "What is PLT Redex"
 (item "A tool to explore and experiment with languages")
 (item "It can formalize a language and more powerful than prolog")
 (item "It can test whether your judgment holds or not")
 (item "It can draw the derivation tree for your examples")
 (item "It can generate random terms according to your specified constraints")
 (item "Thus it can test the property of your language instead of proving it")
 )

(slide
 #:title "STLC Syntax"
 (para
  (code
   (define-language L
     (x ::= variable-not-otherwise-mentioned)
     (e ::= x (λ (x : τ) e) (e e)
        false true (if e then e else e))
     (τ ::= bool (τ -> τ))
     (Γ ::= ((x τ) ...))
     (v ::= true false (λ (x : τ) e))
     (E ::= hole (E e) (v E) (if E then e else e))
     #:binding-forms
     (λ (x : τ) e #:refers-to x))
   )))

(current-font-size 18)

(slide
 #:title "STLC Typing Judgment"
 (para
  (code
   (define-judgment-form L
     #:mode (typeof I I I O)
     #:contract (typeof Γ e : τ)
     [(lookup Γ x τ)
      -------------- "t-var"
      (typeof Γ x : τ)]
     [(typeof (ext Γ (x_1 τ_1)) e : τ)
      ---------------------------- "t-abs"
      (typeof Γ (λ (x_1 τ_1) e) : (τ_1 -> τ))]
     [(typeof Γ e_1 : (τ_1 -> τ_2))
      (typeof Γ e_2 : τ_1)
      ------------------------- "t-app"
      (typeof Γ (e_1 e_2) : τ_2)]
     [------------------------- "t-true"
      (typeof Γ true : bool)]
     [------------------------- "t-false"
      (typeof Γ false : bool)]
     [(typeof Γ e_1 : bool)
      (typeof Γ e_2 : τ)
      (typeof Γ e_3 : τ)
      ------------------------- "t-if"
      (typeof Γ (if e_1 then e_2 else e_3) : τ)]
     ))))

(current-font-size 32)

(slide
 #:title "Test whether a judgment holds or not"
 (para
  (code
   (test-equal (judgment-holds
                (typeof () true : τ) τ)
               (list (term bool)))
   (test-equal (judgment-holds
                (typeof ((y bool)) y : τ) τ)
               (list (term bool)))
   )))

(slide
 #:title "Draw a derivation tree"
 (para
  (code
   (show-derivations
    (build-derivations
     (typeof ((y bool)) y : bool))))))


(slide
 #:title "Generate terms"
 (para
  (code
   (redex-check
    L v
    (redex-match? L e (term v))
    #:attempts 1000)
   )))
 
 