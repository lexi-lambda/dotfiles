#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/provide-transform
                     racket/require-transform)
         syntax/parse/define)

(provide require/reprovide)

(begin-for-syntax
  (struct opaque-box ([value #:mutable]))

  (define (localize-import)
    (define freshen (make-syntax-introducer))
    (λ (i) (struct-copy import i [local-id (freshen (import-local-id i) 'add)])))

  (define (import-local-ids-by-phase imports)
    (for/fold ([acc (hash)])
              ([import (in-list imports)])
      (define local-id (import-local-id import))
      (define local-phase (import-mode import))
      (hash-update acc local-phase (λ (ids) (cons local-id ids)) '()))))

(define-syntax record-in
  (make-require-transformer
   (syntax-parser
     [(_ b-stx require-spec ...)
      #:do [(define b (syntax-e #'b-stx))]
      #:when (opaque-box? b)
      #:do [(define-values [imports import-sources]
              (expand-import (syntax/loc this-syntax (combine-in require-spec ...))))
            (define localized-imports (map (localize-import) imports))
            (set-opaque-box-value! b (import-local-ids-by-phase localized-imports))]
      (values localized-imports import-sources)])))

(define-syntax recorded-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (syntax-parse stx
       [(_ b-stx)
        #:do [(define b (syntax-e #'b-stx))]
        #:when (opaque-box? b)
        (pre-expand-export
         (quasisyntax/loc this-syntax
           (combine-out #,@(for/list ([(phase ids) (in-hash (opaque-box-value b))])
                             (quasisyntax/loc this-syntax
                               (for-meta #,phase #,@ids)))))
         modes)]))))

(define-syntax-parser require/reprovide
  [(_ require-spec ...)
   #:do [(define b (opaque-box #f))]
   (quasisyntax/loc this-syntax
     (begin
       #,(quasisyntax/loc this-syntax
           (require (record-in #,b require-spec ...)))
       #,(quasisyntax/loc this-syntax
           (provide (recorded-out #,b)))))])

(module* reader syntax/module-reader (submod ".." lang) (begin))

(module* lang racket/base
  (require (for-syntax racket/base)
           racket/require
           syntax/parse/define
           (submod ".."))

  (provide (rename-out [module-begin #%module-begin])
           require only-in except-in prefix-in rename-in combine-in relative-in only-meta-in
           for-syntax for-template for-meta submod quote lib file planet
           (all-from-out racket/require))

  (begin-for-syntax
    (define-syntax-class top-level-form
      #:description "require form or reprovide spec"
      #:commit
      #:attributes [require-form reprovide-spec]
      #:literals [require]
      [pattern (require ~! . _)
               #:attr require-form this-syntax
               #:attr reprovide-spec #f]
      [pattern _
               #:attr require-form #f
               #:attr reprovide-spec this-syntax]))

  (define-syntax-parser module-begin
    [(_ form:top-level-form ...)
     (syntax/loc this-syntax
       (#%plain-module-begin
        {~? form.require-form} ...
        (require/reprovide {~? form.reprovide-spec} ...)))]))
