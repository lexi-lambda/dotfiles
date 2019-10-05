#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/lazy-require
         syntax/parse/define)

(lazy-require ["error.rkt" [raise-argument-error]])

(provide alist? alist/c
         (rename-out [*in-alist in-alist]))

(define (alist? v)
  (and (list? v) (andmap pair? v)))

(define (check-alist v)
  (unless (alist? v)
    (raise-argument-error 'in-alist "alist?" v)))

(define (in-alist alst)
  (check-alist alst)
  (make-do-sequence
   (λ () (values (λ (alst)
                   (define elem (car alst))
                   (values (car elem) (cdr elem)))
                 #f
                 cdr
                 alst
                 (λ (alst) (not (null? alst)))
                 #f
                 #f))))

(define-sequence-syntax *in-alist
  (λ () #'in-alist)
  (syntax-parser
    [[(a b) (_ alst-e)]
     #'[(a b)
        (:do-in
         ([(alst) alst-e])
         (check-alist alst)
         ([alst* alst])
         (not (null? alst*))
         ([(a b) (let ([elem (car alst*)])
                   (values (car elem) (cdr elem)))])
         #t
         #t
         [(cdr alst*)])]]
    [_ #f]))

(define (make-alist/c-contract-property builder flat?)
  (define ((stronger/equivalent compare) self other)
    (and (base-alist/c? other)
         (and (compare (base-alist/c-key-ctc self)
                       (base-alist/c-key-ctc other))
              (compare (base-alist/c-val-ctc self)
                       (base-alist/c-val-ctc other)))))
  (builder
   #:name
   (λ (self)
     (build-compound-type-name 'alist/c
                               (base-alist/c-key-ctc self)
                               (base-alist/c-val-ctc self)))
   #:first-order
   (λ (self)
     (define key? (contract-first-order (base-alist/c-key-ctc self)))
     (define val? (contract-first-order (base-alist/c-val-ctc self)))
     (λ (val)
       (and (list? val)
            (for/and ([elem (in-list val)])
              (and (pair? val)
                   (key? (car val))
                   (val? (cdr val)))))))
   #:late-neg-projection
   (λ (self)
     (define key-proj (get/build-late-neg-projection (base-alist/c-key-ctc self)))
     (define val-proj (get/build-late-neg-projection (base-alist/c-val-ctc self)))
     (λ (blame)
       (define elem-blame (blame-add-context blame "an element of"))
       (define key-blame (blame-add-context blame "a key of"))
       (define val-blame (blame-add-context blame "a value of"))
       (define key-proj+blame (key-proj blame))
       (define val-proj+blame (val-proj blame))
       (λ (val missing-party)
         (unless (list? val)
           (raise-blame-error blame #:missing-party missing-party val
                              '(expected "a list" given: "~e") val))
         (define (check-elem elem)
           (unless (pair? elem)
             (raise-blame-error elem-blame #:missing-party missing-party val
                                '(expected "a pair" given: "~e") val))
           (cons (with-contract-continuation-mark (cons key-blame missing-party)
                   (key-proj+blame (car elem) missing-party))
                 (with-contract-continuation-mark (cons val-blame missing-party)
                   (val-proj+blame (cdr elem) missing-party))))
         (if flat?
             (begin
               (for-each check-elem val)
               val)
             (map check-elem val)))))
   #:list-contract? (λ (self) #t)
   #:stronger (stronger/equivalent contract-stronger?)
   #:equivalent (stronger/equivalent contract-equivalent?)))

(struct base-alist/c (key-ctc val-ctc)
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:custom-print-quotable 'never)
(struct flat-alist/c base-alist/c ()
  #:property prop:flat-contract
  (make-alist/c-contract-property build-flat-contract-property #t))
(struct chaperone-alist/c base-alist/c ()
  #:property prop:chaperone-contract
  (make-alist/c-contract-property build-chaperone-contract-property #f))
(struct impersonator-alist/c base-alist/c ()
  #:property prop:contract
  (make-alist/c-contract-property build-contract-property #f))

(define/subexpression-pos-prop (alist/c key-raw-ctc val-raw-ctc)
  (define key-ctc (coerce-contract 'alist/c key-raw-ctc))
  (define val-ctc (coerce-contract 'alist/c val-raw-ctc))
  (cond
    [(and (flat-contract? key-ctc) (flat-contract? val-ctc))
     (flat-alist/c key-ctc val-ctc)]
    [(and (chaperone-contract? key-ctc) (chaperone-contract? val-ctc))
     (chaperone-alist/c key-ctc val-ctc)]
    [else
     (impersonator-alist/c key-ctc val-ctc)]))
