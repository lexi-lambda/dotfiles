#lang racket/base

(require (for-syntax racket/base
                     racket/list)
         racket/match
         syntax/parse/define)

(provide match? match*? match/values?
         bind bind-values)

(begin-for-syntax
  (define-syntax-class match-pattern
    #:description "pattern"
    #:attributes []
    [pattern _]))

(define-syntax-parser match?
  [(_ val:expr pat:match-pattern)
   (quasisyntax/loc this-syntax
     (match/derived val #,this-syntax [pat #t] [_ #f]))])
(define-syntax-parser match*?
  [(_ [val:expr ...] pat:match-pattern ...)
   #:with _s (make-list (length (attribute val)) #'_)
   (quasisyntax/loc this-syntax
     (match*/derived [val ...] #,this-syntax [[pat ...] #t] [_s #f]))])
(define-syntax-parser match/values?
  [(_ vals:expr pat:match-pattern ...)
   #:with [tmp ...] (generate-temporaries (attribute pat))
   #:with _s (make-list (length (attribute pat)) #'_)
   (quasisyntax/loc this-syntax
     (let-values ([(tmp ...) vals])
       (match*/derived [tmp ...] #,this-syntax [[pat ...] #t] [_s #f])))])

(define-match-expander bind
  (syntax-parser
    [(_ [pat:match-pattern e:expr] ...)
     (syntax/loc this-syntax
       (bind-values [(pat) e] ...))]))
(define-match-expander bind-values
  (syntax-parser
    [(_ [(pat:match-pattern ...) e:expr] ...)
     #'(and (app (λ (v) e) pat ...) ...)]))
