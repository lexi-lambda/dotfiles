#lang racket/base

(require racket/contract
         racket/format
         racket/list
         racket/match
         racket/string
         syntax/srcloc
         "contract.rkt")

(provide (contract-out
          [~id (->i () (#:context [context (or/c syntax? #f)]
                        #:source [source (props) (if (unsupplied-arg? props)
                                                     (or/c syntax? #f)
                                                     source-location?)]
                        #:props [props (or/c syntax? #f)]
                        #:sub-range-binders [sub-range-binder-introduce
                                             (or/c (-> syntax? syntax?) #f)]
                        #:binder? [binder? any/c])
                    #:rest (listof (or/c identifier? string? symbol? keyword? char? number?))
                    [_ identifier?])]
          [~id/1 (->* [] [#:context (or/c syntax? #f 'infer)
                          #:source (or/c source-location? 'infer)
                          #:props (or/c syntax? #f 'infer)
                          #:sub-range-binders (or/c (-> syntax? syntax?) #f)
                          #:binder? any/c]
                      #:rest (and/c (listof (or/c identifier? string? symbol? keyword? char? number?))
                                    list-contains-exactly-one-identifier?)
                      identifier?)]))

(define (~id #:context [context #f]
             #:source [source context]
             #:props [props source]
             #:sub-range-binders [sub-range-binder-introduce
                                  (if (syntax-transforming?)
                                      syntax-local-introduce
                                      values)]
             #:binder? [binder? #t]
             . args)

  (define-values [pieces ids]
    (for/fold ([pieces '()]
               [pos 0]
               [ids '()]
               #:result (values pieces ids))
              ([arg (in-list args)])
      (define (continue piece [id #f])
        (define piece-length (string-length piece))
        (values
         (cons piece pieces)
         (+ pos piece-length)
         (if (and sub-range-binder-introduce id)
             (cons (vector-immutable (sub-range-binder-introduce id) piece-length pos) ids)
             ids)))
      (match arg
        [(? identifier?)
         (continue (symbol->string (syntax-e arg)) arg)]
        [(or (? string?) (? symbol?) (? char?) (? number?))
         (continue (~a arg))]
        [(? keyword?)
         (continue (keyword->string arg))])))

  (define new-id (datum->syntax context
                                (string->symbol (string-append* (reverse pieces)))
                                (build-source-location-vector source)
                                props))

  (cond
    [(not (empty? ids))
     (define (build-prop-value id)
       (match-define (vector old-id old-id-len new-id-start) id)
       (if binder?
           (vector-immutable new-id new-id-start old-id-len 0.5 0.5
                             old-id 0 old-id-len 0.5 0.5)
           (vector-immutable old-id 0 old-id-len 0.5 0.5
                             new-id new-id-start old-id-len 0.5 0.5)))
     (syntax-property new-id 'sub-range-binders (map build-prop-value ids))]
    [else
     new-id]))

(define (list-contains-exactly-one-identifier? lst)
  (= (count identifier? lst) 1))

(define (~id/1 #:context [context 'infer]
               #:props [props 'infer]
               #:sub-range-binders [sub-range-binder-introduce
                                    (if (syntax-transforming?)
                                        syntax-local-introduce
                                        values)]
               #:source [source (if sub-range-binder-introduce #f 'infer)]
               #:binder? [binder? #t]
               . args)
  (define the-id (first (filter identifier? args)))
  (define (infer-or x) (if (eq? x 'infer) the-id x))
  (apply ~id args
         #:context (infer-or context)
         #:source (infer-or source)
         #:props (infer-or props)
         #:sub-range-binders sub-range-binder-introduce
         #:binder? binder?))
