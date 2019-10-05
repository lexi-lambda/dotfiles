#lang racket/base

(require racket/contract
         racket/hash
         racket/list
         racket/match)

(provide (contract-out
          [arguments? predicate/c]
          [make-arguments (-> list? kws-hash/c arguments?)]
          [arguments-positional (-> arguments? list?)]
          [arguments-keywords (-> arguments? kws-hash/c)]
          [arguments-empty? (-> arguments? boolean?)]
          [arguments (unconstrained-domain-> arguments?)]
          [arguments-append (->* [] [#:combine-kws (-> keyword? any/c any/c any/c)]
                                 #:rest (listof arguments?)
                                 arguments?)]
          [apply/arguments procedure?]
          [make-arguments-procedure (-> (-> arguments? any) procedure?)]))

(define kws-hash/c (and/c (hash/c keyword? any/c #:immutable? #t) hash-eq?))

;; ---------------------------------------------------------------------------------------------------

(struct arguments (positional keywords)
  #:transparent
  #:constructor-name internal-make-arguments
  #:omit-define-syntaxes)

(define empty-arguments (internal-make-arguments '() (hasheq)))

(define (make-arguments positional keywords)
  (if (and (empty? positional) (hash-empty? keywords))
      empty-arguments
      (internal-make-arguments positional keywords)))

(define (arguments-empty? args)
  (eq? args empty-arguments))

(define arguments
  (make-keyword-procedure
   (lambda (kws kw-vals . pos-vals)
     (make-arguments pos-vals (make-immutable-hasheq (map cons kws kw-vals))))))

;; ---------------------------------------------------------------------------------------------------

(define ((raise-multiple-keywords-error who) kw val-a val-b)
  (raise-arguments-error who "multiple values for keyword"
                         "keyword" kw
                         "first val" val-a
                         "second val" val-b))

(define (arguments-append #:combine-kws [combine-kws (raise-multiple-keywords-error
                                                      'arguments-append)]
                          . args)
  (make-arguments (append-map arguments-positional args)
                  (apply hash-union (map arguments-keywords args) #:combine/key combine-kws)))

; procedure? any/c ... #:<kw> any/c ... arguments? -> any
(define apply/arguments
  (make-keyword-procedure
   (lambda (kws kw-vals f . all-pos-vals)
     (match-define (list pos-vals ... arg-val) all-pos-vals)
     (define extra-args (keyword-apply arguments kws kw-vals pos-vals))
     (apply/arguments f (arguments-append
                         arg-val extra-args
                         #:combine-kws (raise-multiple-keywords-error 'arguments-apply))))
   (case-lambda
     [(f arg-val)
      (match-define (list (cons kws kw-vals) ...)
        (sort (hash->list (arguments-keywords arg-val))
              keyword<? #:key car))
      (keyword-apply f kws kw-vals (arguments-positional arg-val))]
     [(f . all-pos-vals)
      (match-define (list pos-vals ... arg-val) all-pos-vals)
      (define extra-args (apply arguments pos-vals))
      (apply/arguments f (arguments-append arg-val extra-args))])))

(define (make-arguments-procedure proc)
  (make-keyword-procedure
   (lambda (kws kw-vals . pos-args)
     (proc (keyword-apply arguments kws kw-vals pos-args)))
   (lambda pos-args
     (proc (apply arguments pos-args)))))
