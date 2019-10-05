#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/runtime-path)

(provide poly-first-or/c)

;; ---------------------------------------------------------------------------------------------------

(define-namespace-anchor this-anchor)
(define-runtime-module-path-index parametric-mpi 'racket/contract/private/parametric)
(define-runtime-module-path-index exists-mpi 'racket/contract/private/exists)

(define this-ns (namespace-anchor->namespace this-anchor))
(define parametric-ns (module->namespace parametric-mpi this-ns))
(define exists-ns (module->namespace exists-mpi this-ns))

(define-values [barrier-contract? barrier-contract-pred barrier-contract-positive?]
  (eval '(values barrier-contract? barrier-contract-pred barrier-contract-positive?) parametric-ns))
(define-values [∀∃/c? ∀∃/c-pred? ∀∃/c-neg?]
  (eval '(values ∀∃/c? ∀∃/c-pred? ∀∃/c-neg?) exists-ns))

(define (poly/c? v)
  (or (barrier-contract? v) (∀∃/c? v)))
(define (poly/c-pred v)
  (if (barrier-contract? v) (barrier-contract-pred v) (∀∃/c-pred? v)))
(define (poly/c-positive? v)
  (if (barrier-contract? v) (barrier-contract-positive? v) (not (∀∃/c-neg? v))))

(define (contract-first-order/blame ctc)
  (if (poly/c? ctc)
      (let ([pred (poly/c-pred ctc)]
            [pos? (poly/c-positive? ctc)])
        (λ (v blame)
          (or (eq? (blame-original? blame) pos?)
              (pred blame))))
      (let ([ctc? (contract-first-order ctc)])
        (λ (v blame) (ctc? v)))))

;; ---------------------------------------------------------------------------------------------------

(struct poly-first-or/c (ctcs)
  #:constructor-name make-poly-first-or/c
  #:omit-define-syntaxes
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:contract
  (build-contract-property
   #:name
   (λ (self)
     (apply build-compound-type-name 'poly-first-or/c (poly-first-or/c-ctcs self)))
   #:first-order (λ (self) #t)
   #:late-neg-projection
   (λ (self)
     (define ctcs (poly-first-or/c-ctcs self))
     (define ctc?s (map contract-first-order/blame ctcs))
     (define ctc-projs (map get/build-late-neg-projection ctcs))
     (λ (blame)
       (define ctc-projs/blame (map (λ (proj) (proj blame)) ctc-projs))
       (λ (val missing-party)
         (define ctc-proj/blame (for/first ([ctc? (in-list ctc?s)]
                                            [ctc-proj/blame (in-list ctc-projs/blame)]
                                            #:when (ctc? val blame))
                                  ctc-proj/blame))
         (unless ctc-proj/blame
           (raise-blame-error blame #:missing-party missing-party val "none of the cases matched"))
         (ctc-proj/blame val missing-party))))))

(define/subexpression-pos-prop (poly-first-or/c . raw-ctcs)
  (define ctcs (coerce-contracts 'poly-first-or/c raw-ctcs))
  (if (ormap poly/c? ctcs)
      (make-poly-first-or/c ctcs)
      (apply first-or/c ctcs)))
