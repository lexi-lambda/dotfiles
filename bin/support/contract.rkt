#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     racket/list)
         racket/contract
         racket/stxparam
         syntax/parse/define)

(provide current-contract-precision-level
         configurable-precision/c)

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
