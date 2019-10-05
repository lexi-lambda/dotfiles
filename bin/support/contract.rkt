#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     racket/list)
         racket/contract
         racket/stxparam
         syntax/parse/define)

(provide current-contract-precision-level
         configurable-precision/c
         (contract-out
          [property/c (->* [(-> any/c any/c) flat-contract?] [#:name any/c] flat-contract?)]))

;; ---------------------------------------------------------------------------------------------------
;; configurable-precision/c

(define-syntax-parameter current-contract-precision-level '#:high)

(begin-for-syntax
  (define contract-precision-levels '(#:none #:low #:mid #:high))
  (define (precision-level->integer l)
    (index-of contract-precision-levels l))

  (define-syntax-class precision-level
    #:description "precision level"
    #:attributes []
    #:commit
    [pattern {~or #:low #:mid #:high}]))

(define-syntax-parser configurable-precision/c
  [(_ [precision-level:keyword ctc] ...)
   #:declare ctc (expr/c #'contract? #:name (~a (syntax-e #'precision-level) " contract"))
   #:do [(define current-level (syntax-local-value #'current-contract-precision-level))]
   (cond
     [(or (eq? current-level '#:none)
          (empty? (attribute precision-level)))
      #'any/c]
     [else
      (define specs (map cons (map syntax-e (attribute precision-level)) (attribute ctc.c)))
      (cdr (cond
             [(assq current-level specs)]
             [else
              ; pick the closest spec to the wanted level, preferring stronger
              (define current-level-index (precision-level->integer current-level))
              (define (spec-level-index spec) (precision-level->integer (car spec)))
              (define sorted-specs (sort specs > #:key spec-level-index))
              (argmin (λ (spec) (abs (- current-level-index (spec-level-index spec)))) specs)]))])])

;; ---------------------------------------------------------------------------------------------------
;; property/c

(struct property/c (accessor val-ctc prop-name)
  #:constructor-name make-property/c
  #:omit-define-syntaxes
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name
   (λ (ctc)
     `(property/c ,(property/c-prop-name ctc)
                  ,(contract-name (property/c-val-ctc ctc))))
   #:first-order
   (λ (ctc)
     (define accessor (property/c-accessor ctc))
     (define val-ctc-first-order (contract-first-order (property/c-val-ctc ctc)))
     (λ (val)
       (val-ctc-first-order (accessor val))))
   #:late-neg-projection
   (λ (ctc)
     (define accessor (property/c-accessor ctc))
     (define val-ctc-proj (contract-late-neg-projection (property/c-val-ctc ctc)))
     (define prop-name (property/c-prop-name ctc))
     (define ctx-str (format "the ~a property of" prop-name))
     (λ (orig-blame)
       (define blame (blame-add-context orig-blame ctx-str))
       (define val-ctc-proj/blame (val-ctc-proj blame))
       (λ (val neg-party)
         (val-ctc-proj/blame (accessor val) neg-party))))
   #:stronger
   (λ (ctc-a ctc-b)
     (contract-stronger? (property/c-val-ctc ctc-a)
                         (property/c-val-ctc ctc-b)))
   #:equivalent
   (λ (ctc-a ctc-b)
     (contract-equivalent? (property/c-val-ctc ctc-a)
                           (property/c-val-ctc ctc-b)))))

(define (property/c accessor ctc-v #:name [name (object-name accessor)])
  (make-property/c accessor ctc-v name))
