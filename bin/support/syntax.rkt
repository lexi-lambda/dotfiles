#lang racket/base

(require racket/contract
         racket/format
         racket/list
         racket/match
         racket/string
         syntax/srcloc
         "contract.rkt")

(provide (contract-out
          [~id (->* [] [#:context (or/c syntax? #f)
                        #:source source-location?
                        #:props (or/c syntax? #f 'infer)
                        #:sub-range-binders (or/c #t (-> syntax? syntax?) #f)
                        #:binder? any/c]
                    #:rest (listof (or/c identifier? string? symbol? keyword? char? number?))
                    identifier?)]
          [~id/1 (->* [] [#:context (or/c syntax? #f 'infer)
                          #:source (or/c source-location? 'infer)
                          #:props (or/c syntax? #f 'infer)
                          #:sub-range-binders (or/c #t (-> syntax? syntax?) #f)
                          #:binder? any/c]
                      #:rest (and/c (listof (or/c identifier? string? symbol? keyword? char? number?))
                                    list-contains-exactly-one-identifier?)
                      identifier?)]))

(define (jumble->list v p?)
  (let recur ([v v])
    (cond
      [(p? v) (list v)]
      [(list? v) (append-map recur v)]
      [(pair? v) (append (recur (car v)) (recur (cdr v)))]
      [else '()])))

(define (~id #:context [context #f]
             #:source [source context]
             #:props [props 'infer]
             #:sub-range-binders [sub-range-binders #t]
             #:binder? [binder? #t]
             . args)
  (define sub-range-binder-introduce (and sub-range-binders
                                          (if (procedure? sub-range-binders)
                                              sub-range-binders
                                              (if (syntax-transforming?)
                                                  syntax-local-introduce
                                                  values))))
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
         (if (and sub-range-binders id)
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
                                ; If we’re attaching 'sub-range-binders, then we don’t want to copy
                                ; properties, since 'sub-range-binders doesn’t care about
                                ; originalness, and in fact it will probably do more harm than good.
                                ; But if context is provided and we’re not attaching
                                ; 'sub-range-binders, then we probably want to copy originalness,
                                ; after all.
                                (if (eq? props 'infer)
                                    (if sub-range-binders #f context)
                                    props)))

  (cond
    [(not (empty? ids))
     ; Build a 'sub-range-binders for each given identifier.
     (define (build-prop-value id)
       (define new-id-introduced (sub-range-binder-introduce new-id))
       (match-define (vector old-id old-id-len new-id-start) id)

       ; If the identifier already has a 'sub-range-binders property on it, then in all likelihood
       ; that means it was just created in the dynamic extent of the same macro transformer that
       ; added it in the first place. This means that the identifier may be being built in several
       ; steps, using multiple calls to ~id in sequence. Therefore, we want to copy over old
       ; 'sub-range-binders values that are already present if one side of the binding arrow points
       ; to the old identifier.
       ;
       ; Conservatively, we use bound-identifier=? to check if the identifier is, in fact, the
       ; “same”, since if we’re really in the same macro transformer, it’s unlikely other scopes
       ; have been added in between, anyway.
       (define (relocate-old-prop-value val)
         (match-define (vector binder-id binder-id-start binder-id-range binder-id-x binder-id-y
                               use-id use-id-start use-id-range use-id-x use-id-y)
           val)
         (cond
           [(bound-identifier=? binder-id old-id)
            (vector-immutable new-id-introduced (+ binder-id-start new-id-start) binder-id-range
                              binder-id-x binder-id-y
                              use-id use-id-start use-id-range use-id-x use-id-y)]
           [(bound-identifier=? use-id old-id)
            (vector-immutable binder-id binder-id-start binder-id-range binder-id-x binder-id-y
                              new-id-introduced (+ use-id-start new-id-start) use-id-range
                              use-id-x use-id-y)]
           [else
            #f]))
       (define old-values (jumble->list (syntax-property old-id 'sub-range-binders) vector?))
       (define relocated-old-values (filter-map relocate-old-prop-value old-values))

       (define new-value
         (if binder?
             (vector-immutable new-id-introduced new-id-start old-id-len 0.5 0.5
                               old-id 0 old-id-len 0.5 0.5)
             (vector-immutable old-id 0 old-id-len 0.5 0.5
                               new-id-introduced new-id-start old-id-len 0.5 0.5)))
       (if (empty? relocated-old-values) new-value (cons new-value relocated-old-values)))

     (syntax-property new-id 'sub-range-binders (map build-prop-value ids))]
    [else
     new-id]))

(define (list-contains-exactly-one-identifier? lst)
  (= (count identifier? lst) 1))

(define (~id/1 #:context [context 'infer]
               #:source [source 'infer]
               #:props [props 'infer]
               #:sub-range-binders [sub-range-binder-introduce #t]
               #:binder? [binder? #t]
               . args)
  (define the-id (first (filter identifier? args)))
  (define (infer-or x) (if (eq? x 'infer) the-id x))
  (apply ~id args
         #:context (infer-or context)
         #:source (infer-or source)
         #:props props
         #:sub-range-binders sub-range-binder-introduce
         #:binder? binder?))
