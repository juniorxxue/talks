#lang racket
(require redex)
(define-language L
  (x ::= variable-not-otherwise-mentioned)
  (e ::= x (λ (x : τ) e) (e e) false true (if e then e else e))
  (τ ::= bool (τ -> τ))
  (Γ ::= ((x τ) ...))
  (v ::= true false (λ (x : τ) e))
  (E ::= hole (E e) (v E) (if E then e else e))
  #:binding-forms
  (λ (x : τ) e #:refers-to x)
  )

(default-language L)

(define-judgment-form L
  #:mode (lookup I I O)
  #:contract (lookup ((any any) ...) any any)
  [(lookup (_ ... (any_1 any_2) _ ...) any_1 any_2)])

(test-equal (judgment-holds (lookup ((x 1) (y 1) (x 3)) x 1)) #t)

(define-judgment-form L
  #:mode (unique I)
  #:contract (unique (any ...))
  [(unique (any_!_1 ...))]
  )
(test-equal (judgment-holds (unique ((x 1) (y 1)))) #t)
(test-equal (judgment-holds (unique ((x 1) (x 1)))) #f)

(define-metafunction L
  ext1 : ((any any) ...) (any any) -> ((any any) ...)
  [(ext1 (any_1 ... (any_2 any_3) any_4 ...) (any_2 any_5)) (any_1 ... (any_2 any_5) any_4 ...)]
  [(ext1 (any_1 ...) (any_2 any_3) ) ((any_2 any_3) any_1 ...)])

(define-metafunction L
  ext : ((any any) ..) (any any) ... -> ((any any) ...)
  [(ext any) any]
  [(ext any any_1 any_2 ...) (ext1 (ext any any_2 ...) any_1)])

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
  )
(test-equal (judgment-holds (typeof () true : τ) τ) (list (term bool)))
(test-equal (judgment-holds (typeof ((y bool)) y : τ) τ) (list (term bool)))

;; (show-derivations (build-derivations (typeof ((y bool)) y : bool)))

(define r
  (reduction-relation
   L
   (--> ((λ (x : τ) e_1) v) (substitute e_1 x v) "beta")
   (--> (if true then e_1 else e_2) e_1 "if-t")
   (--> (if false then e_1 else e_2) e_2 "if-f")))

;; reducation relation can be thought as a judgment with #:mode (r I O)

(define -->r (compatible-closure r L e))

;; (traces -->r (term ((λ (x : bool) x) (if true then true else false))))

;; (traces -->r (term ((λ (x : bool) x)
;;                     (if (if true then true else false) then
;;                         (if true then true else false) else
;;                         (if true then true else false)))))

;; right here problems has happened, weneed to determine which side should be evaluted first
(define -->n (context-closure r L E)) ;; evaluation context rocks

;; (traces -->n (term ((λ (x : bool) x)
;;                     (if (if true then true else false) then
;;                         (if true then true else false) else
;;                         (if true then true else false)))))

;; all value is expression
(redex-check L v (redex-match? L e (term v)) #:attempts 1000)

;; (redex-match L (in-hole E e) (term ((λ (x : bool) x)
;;                                     (if (if true then true else false) then
;;                                         (if true then true else false) else
;;                                         (if true then true else false)))))