#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/set
                     syntax/name)
         syntax/parse/define)

(provide (rename-out [keyword-case-lambda case-lambda]))

;; ---------------------------------------------------------------------------------------------------
;; keyword `case-lambda`

(begin-for-syntax
  (define-syntax-class formals
    #:attributes [[pos-req.id 1]
                  [pos-opt.id 1]
                  [pos-opt.expr 1]
                  [kw.kw 1]
                  [kw.id 1]
                  [kw.expr 1]
                  [kw-req.kw 1]
                  rest-id]
    #:commit
    [pattern ({~alt {~and kw/pos:kw-formal ~!}
                    {~and pos-req.id:id ~!}}
              ...
              {~alt {~and kw/opt:kw-formal ~!}
                    {~and pos-opt:formal-opt-spec ~!}
                    {~and {~describe #:opaque #f bad-pos-req:id}
                          ~! {~fail #:when #'bad-pos-req
                                    (string-append "required positional argument cannot come"
                                                   " after optional positional argument")}}}
              ...
              . {~and {~or* rest-id:id ()} ~!})
             #:attr [kw.kw 1] (append (attribute kw/pos.kw) (attribute kw/opt.kw))
             #:attr [kw.id 1] (append (attribute kw/pos.id) (attribute kw/opt.id))
             #:attr [kw.expr 1] (append (attribute kw/pos.expr) (attribute kw/opt.expr))
             #:attr [kw-req.kw 1] (for/list ([kw (in-list (attribute kw.kw))]
                                             [expr (in-list (attribute kw.expr))]
                                             #:unless expr)
                                    kw)
             #:fail-when (check-duplicate-identifier (append (attribute pos-req.id)
                                                             (attribute pos-opt.id)
                                                             (attribute kw.id)
                                                             (if (attribute rest-id)
                                                                 (list #'rest-id)
                                                                 '())))
             "duplicate argument name"
             #:fail-when (check-duplicates (attribute kw.kw) eq? #:key syntax-e)
             "duplicate argument keyword"])

  (define-splicing-syntax-class kw-formal
    #:description #f
    #:attributes [kw id expr]
    #:no-delimit-cut
    [pattern {~seq kw:keyword ~! {~or* id:id :formal-opt-spec}}])

  (define-syntax-class formal-opt-spec
    #:description "optional formal spec"
    #:attributes [id expr]
    #:no-delimit-cut
    [pattern {~and [_ . _] ~! [id:id expr:expr]}]))

(define-syntax-parser keyword-case-lambda
  [(_)
   (syntax/loc this-syntax
     (case-lambda))]
  [(_ [formals:formals body ...+])
   (syntax/loc this-syntax
     (lambda formals body ...))]
  [(_ [formals:formals body ...+] ...+)
   ; calculate arity masks for each clause
   #:do [(define pos-masks (for/list ([req-ids (in-list (attribute formals.pos-req.id))]
                                      [opt-ids (in-list (attribute formals.pos-opt.id))]
                                      [rest-id (in-list (attribute formals.rest-id))])
                             (define req-mask (arithmetic-shift -1 (length req-ids)))
                             (if rest-id
                                 req-mask
                                 (bitwise-xor req-mask
                                              (arithmetic-shift req-mask
                                                                (add1 (length opt-ids)))))))]
   #:with [formals.pos-mask ...] pos-masks
   ; calculate arity mask for the whole `case-lambda`
   #:with pos-mask (foldl bitwise-ior 0 pos-masks)
   ; calculate which keywords are required and which are allowed for the whole `case-lambda`
   #:do [(define (stxs-e stxs) (map syntax-e stxs))]
   #:with [kw-req ...] (sort (apply set-intersect (map stxs-e (attribute formals.kw-req.kw)))
                             keyword<?)
   #:with [kw-all ...] (sort (apply set-union (map stxs-e (attribute formals.kw.kw))) keyword<?)

   ; build `lambda` expressions for each clause
   #:do [(define inferred-name (syntax-local-infer-name this-syntax))
         (define (add-inferred-name stx)
           (syntax-property stx 'inferred-name inferred-name))]
   #:with [clause-lambda ...] (for/list ([formals (in-list (attribute formals))]
                                         [bodies (in-list (attribute body))])
                                (add-inferred-name
                                 (quasisyntax/loc this-syntax
                                   (lambda #,formals #,@bodies))))

   (if (empty? (attribute kw-all))
       ; if no keyword arguments are allowed, expand directly to `lambda`
       #`(procedure-reduce-arity-mask
          #,(add-inferred-name
             (syntax/loc this-syntax
               (lambda pos-args
                 (define num-pos-args (length pos-args))
                 (cond
                   [(bitwise-bit-set? 'formals.pos-mask num-pos-args)
                    (apply clause-lambda pos-args)]
                   ...))))
          'pos-mask)

       ; otherwise, expand to `make-keyword-procedure`
       #`(procedure-reduce-keyword-arity-mask
          (make-keyword-procedure
           #,(add-inferred-name
              (quasisyntax/loc this-syntax
                (lambda (kws kw-args . pos-args)
                  (define num-pos-args (length pos-args))
                  (cond
                    [(and (bitwise-bit-set? 'formals.pos-mask num-pos-args)
                          (for/and ([kw (in-list kws)])
                            (memq kw '(formals.kw.kw ...)))
                          (for/and ([kw (in-list '(formals.kw-req.kw ...))])
                            (memq kw kws)))
                     (keyword-apply clause-lambda kws kw-args pos-args)]
                    ...
                    [else
                     (raise
                      (exn:fail:contract:arity
                       (apply string-append (format "~a" '#,inferred-name)
                              ": the given combination of arguments does not match "
                              "any expected combination\n"
                              "  arguments...:"
                              (append (for/list ([pos-arg (in-list pos-args)])
                                        (format "\n   ~v" pos-arg))
                                      (for/list ([kw (in-list kws)]
                                                 [kw-arg (in-list kw-args)])
                                        (format "\n   ~a ~v" kw kw-arg))))
                       (current-continuation-marks)))])))))
          'pos-mask
          '(kw-req ...)
          '(kw-all ...)))])
