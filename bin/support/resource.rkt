#lang racket/base

(require (for-syntax racket/base)
         net/http-client
         racket/contract
         racket/file
         racket/match
         racket/path
         racket/system
         racket/syntax
         syntax/parse/define
         "error.rkt"
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
                                   resource?)]
          [make-fifo-resource (->* [] [#:filename path-element?] resource?)]
          [make-temporary-file-resource (->* [] [#:filename-template string?
                                                 #:directory? any/c]
                                             resource?)]
          [make-http-conn-resource (-> resource?)]))

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

(define (make-temporary-file-resource #:filename-template [filename-template "rkttmp~a"]
                                      #:directory? [directory? #f])
  (make-resource
   #:name 'temporary-file
   #:constructor (λ () (make-temporary-file filename-template (if directory? 'directory #f)))
   #:destructor (λ (filename) (delete-directory/files filename #:must-exist? #t))))

(define mkfifo-path (find-executable-path "mkfifo"))
(define (make-fifo-resource #:filename [filename "rkttmp_fifo.fifo"])
  (make-resource
   #:name 'fifo
   #:constructor
   (λ ()
     (unless mkfifo-path
       (panic! #:name 'mkfifo "cannot find executable"))
     (define fifo-dir (simple-form-path (make-temporary-file "rkttmp_fifo_~a" 'directory)))
     (define fifo-file (build-path fifo-dir filename))
     (define exit-code (system*/exit-code mkfifo-path fifo-file))
     (unless (zero? exit-code)
       (panic! #:name 'mkfifo "non-zero exit code" #:fields `([exit-code . ,exit-code])))
     (values fifo-dir fifo-file))
   #:destructor (λ (fifo-dir fifo-file) (delete-directory/files fifo-dir #:must-exist? #t))
   #:wrapper (λ (thunk fifo-dir fifo-file) (thunk fifo-file))))

(define (make-http-conn-resource)
  (make-resource
   #:name 'http-conn
   #:constructor http-conn
   #:destructor http-conn-close!))
