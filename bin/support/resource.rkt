#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/match
         racket/syntax
         syntax/parse/define
         "struct.rkt")

(provide (contract-out
          [resource? (-> any/c boolean?)]
          [make-resource (->* [] [#:name symbol?
                                  #:constructor (or/c procedure? #f)
                                  #:destructor (or/c procedure? #f)
                                  #:wrapper (or/c procedure? #f)
                                  #:one-shot? any/c]
                              resource?)]
          [call-with-resource (-> resource? procedure? any)])
         with-resources

         (contract-out
          [make-custodian-resource (->* [] [custodian?] resource?)]
          [make-pipe-resource (->* [] [#:limit (or/c exact-positive-integer? #f)
                                       #:name any/c
                                       #:input-name any/c
                                       #:output-name any/c]
                                   resource?)]))

;; ---------------------------------------------------------------------------------------------------
;; core

(struct resource (name constructor destructor wrapper one-shot?)
  #:property prop:object-name+custom-write (struct-field-index name))

(define (make-resource #:name [name #f]
                       #:constructor [constructor #f]
                       #:destructor [destructor #f]
                       #:wrapper [wrapper #f]
                       #:one-shot? [one-shot? #t])
  (resource name
            (or constructor (λ () (values)))
            (or destructor void)
            (or wrapper (λ (proc . args) (apply proc args)))
            one-shot?))

(define (call-with-resource resource proc)
  (define constructor (resource-constructor resource))
  (define destructor (resource-destructor resource))
  (define wrapper (resource-wrapper resource))

  (define (build+invoke)
    (call-with-values
     constructor
     (case-lambda
       [()
        (dynamic-wind void (λ () (wrapper proc)) destructor)]
       [(val)
        (dynamic-wind
         void
         (λ () (wrapper proc val))
         (λ () (destructor val)))]
       [vals
        (dynamic-wind
         void
         (λ () (apply wrapper proc vals))
         (λ () (apply destructor vals)))])))

  (if (resource-one-shot? resource)
      (call-with-continuation-barrier build+invoke)
      (build+invoke)))

(define-syntax-parser with-resources
  [(_ ([{~or* {~and single:id {~bind [[x 1] (list #'single)]}}
              (x:id ...)}
        resource:expr]
       ...)
      body ...+)
   #:with [[tmp ...] ...] (map generate-temporaries (attribute x))
   (for/fold ([body (syntax/loc this-syntax (let () body ...))])
             ([xs (in-list (reverse (attribute x)))]
              [tmps (in-list (reverse (attribute x)))]
              [resource (in-list (reverse (attribute resource)))])
     (quasisyntax/loc this-syntax
       (call-with-resource #,resource
                           #,(quasisyntax/loc this-syntax
                               (λ #,tmps #,(quasisyntax/loc this-syntax
                                             (match-let-values ([#,xs (values #,@tmps)])
                                               #,body)))))))])

;; ---------------------------------------------------------------------------------------------------
;; resource library

(define (make-custodian-resource [cust (current-custodian)])
  (make-resource #:name 'custodian
                 #:constructor (λ () (make-custodian cust))
                 #:destructor custodian-shutdown-all
                 #:wrapper (λ (thunk new-cust) (parameterize ([current-custodian new-cust])
                                                 (thunk)))))

(define (make-pipe-resource #:limit [limit #f]
                            #:name [name 'pipe]
                            #:input-name [input-name name]
                            #:output-name [output-name name])
  (make-resource #:name name
                 #:constructor (λ () (make-pipe limit input-name output-name))
                 #:destructor (λ (in out) (close-input-port in) (close-output-port out))))
