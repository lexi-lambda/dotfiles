#lang racket/base

(require (for-syntax racket/base
                     racket/format
                     "syntax.rkt")
         racket/contract
         racket/list
         syntax/parse/define)

(provide
 ;; define-struct-type-property
 (contract-out
  [make-standard-struct-type-property
   (->* [symbol?]
        [#:wrap-guard wrap-guard/c
         #:value-contract contract?
         #:allow-procedure? any/c
         #:allow-plain-value? any/c
         #:allow-field-index? any/c
         #:supers (listof (cons/c struct-type-property? (-> any/c any/c)))
         #:can-impersonate? any/c]
        (values struct-type-property?
                procedure?
                procedure?))])
 define-struct-type-property

 ;; object-name+custom-write
 (contract-out
  [make-object-name-custom-write (-> symbol? (-> any/c output-port? (or/c #t #f 0 1) void?))])
 prop:object-name+custom-write)

;; ---------------------------------------------------------------------------------------------------
;; define-struct-type-property

(define struct-type-info/c
  (list/c symbol?
          exact-nonnegative-integer?
          exact-nonnegative-integer?
          struct-accessor-procedure?
          struct-mutator-procedure?
          (listof exact-nonnegative-integer?)
          (or/c struct-type? #f)
          boolean?))

(define wrap-guard/c
  (-> any/c struct-type-info/c (-> any/c struct-type-info/c (-> any/c any/c)) any/c))

(define (make-standard-struct-type-property
         name
         #:wrap-guard [wrap-guard (λ (v info guard) (guard v info))]
         #:value-contract [value/c any/c]
         #:allow-procedure? [allow-procedure? #t]
         #:allow-plain-value? [allow-plain-value? #t]
         #:allow-field-index? [allow-field-index? #t]
         #:supers [supers '()]
         #:can-impersonate? [can-impersonate? #f])
  (define-values [prop:name name? name-ref]
    (make-struct-type-property
     name
     (λ (v info)
       (wrap-guard
        v info
        (λ (v info)
          (cond
            [(and allow-procedure? (procedure? v))
             v]
            [(and allow-field-index? (exact-integer? v))
             (define self-ref (fourth info))
             (λ (self) (self-ref self v))]
            [else
             (λ (self) v)]))))
     supers
     can-impersonate?))
  (values prop:name name? (procedure-rename (λ (self) ((name-ref self) self)) name)))

(define-syntax-parser define-struct-type-property
  [(_ {~optional name:id}
      {~alt {~optional {~seq #:property-id property-id:id}}
            {~optional {~seq #:predicate-id predicate-id:id}}
            {~optional {~seq #:accessor-id accessor-id:id}}
            {~optional {~seq #:reflection-name reflection-name}}
            {~optional {~seq #:wrap-guard wrap-guard}}
            {~optional {~seq #:value-contract value-contract}}
            {~optional {~or* {~and #:disallow-procedure {~bind [allow-procedure? #'#f]}}
                             {~seq #:allow-procedure? allow-procedure?:expr}}}
            {~optional {~or* {~and #:disallow-plain-value {~bind [allow-plain-value? #'#f]}}
                             {~seq #:allow-plain-value? allow-plain-value?:expr}}}
            {~optional {~or* {~and #:disallow-field-index {~bind [allow-field-index? #'#f]}}
                             {~seq #:allow-field-index? allow-field-index?:expr}}}
            {~seq #:super super-prop super-proc}
            {~optional {~or* {~and #:can-impersonate {~bind [can-impersonate? #'#t]}}
                             {~seq #:can-impersonate? can-impersonate?:expr}}}}
      ...)
   #:declare reflection-name (expr/c #'symbol? #:name "#:reflection-name argument")
   #:declare wrap-guard (expr/c #'wrap-guard/c #:name "#:wrap-guard argument")
   #:declare value-contract (expr/c #'contract? #:name "#:value-contract argument")
   #:declare super-prop (expr/c #'struct-type-property? #:name "#:super argument")
   #:declare super-proc (expr/c #'(-> any/c any/c) #:name "#:super argument")
   #:fail-unless (or (attribute name)
                     (and (attribute property-id)
                          (attribute predicate-id)
                          (attribute accessor-id)
                          (attribute reflection-name)))
   (~a "Either a name identifier must be specified, or #:property-id, #:predicate-id, "
       "#:accessor-id, and #:reflection-name options must all be provided")
   #:with {~var prop:name} (or (attribute property-id) (~id/1 "prop:" #'name))
   #:with name? (or (attribute predicate-id) (~id/1 #'name "?"))
   #:with name-ref (or (attribute accessor-id) (~id/1 #'name "-ref"))
   (syntax/loc this-syntax
     (define-values [prop:name name? name-ref]
       (make-standard-struct-type-property {~? reflection-name.c 'name}
                                           {~? {~@ #:wrap-guard wrap-guard.c}}
                                           {~? {~@ #:value-contract value-contract.c}}
                                           {~? {~@ #:allow-procedure? allow-procedure?}}
                                           {~? {~@ #:allow-plain-value? allow-plain-value?}}
                                           {~? {~@ #:allow-field-index? allow-field-index?}}
                                           #:supers (list (cons super-prop.c super-proc.c) ...)
                                           {~? {~@ #:can-impersonate? can-impersonate?}})))])

;; ---------------------------------------------------------------------------------------------------
;; object-name+custom-write

(define ((make-object-name-custom-write type-name) self out mode)
  (define name (object-name self))
  (if name
      (fprintf out "#<~a:~a>" type-name name)
      (fprintf out "#<~a>" type-name)))

(define-struct-type-property object-name+custom-write
  #:wrap-guard (λ (v info guard) (cons (if (exact-integer? v) v (guard v info)) info))
  #:super prop:object-name car
  #:super prop:custom-write (λ (v+info) (make-object-name-custom-write (first (cdr v+info)))))
