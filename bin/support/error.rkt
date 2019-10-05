#lang racket/base

(require racket/contract
         racket/format
         racket/list
         racket/sequence
         racket/string
         "sequence.rkt")

(provide (struct-out exn:fail:panic)
         current-panic-message-prefix
         current-panic-exit-code
         raise-argument-error
         raise-arguments-error
         (contract-out
          [make-uncaught-panic-exception-handler (->* [] [(-> any/c any)] (-> any/c any))]
          [panic! (->* [string?]
                       [string?
                        #:name (or/c symbol? #f)
                        #:fields (listof (cons/c string? any/c))
                        #:exit-code any/c]
                       any)]))

;; ---------------------------------------------------------------------------------------------------
;; `panic!`

(struct exn:fail:panic exn:fail (exit-code) #:transparent)

(define ((make-uncaught-panic-exception-handler [super (uncaught-exception-handler)]) exn)
  (parameterize ([uncaught-exception-handler super])
    (define existing-break-paramz (current-break-parameterization))
    (parameterize-break #f
      (cond
        [(exn:fail:panic? exn)
         ((error-display-handler) (exn-message exn) exn)
         (exit (exn:fail:panic-exit-code exn))
         ((error-escape-handler))
         (abort-current-continuation (default-continuation-prompt-tag) void)]
        [else
         (call-with-break-parameterization existing-break-paramz
                                           (λ () (super exn)))]))))

(define current-panic-message-prefix (make-parameter #f))
(define current-panic-exit-code (make-parameter #f))

(define (panic! #:name [name #f] main-message [continued-message #f]
                #:fields [fields '()]
                #:exit-code [exit-code (current-panic-exit-code)])
  (define prefix (current-panic-message-prefix))
  (define message (build-arguments-error-message
                   (if name
                       (if prefix
                           (~a prefix ": " name)
                           name)
                       (or prefix 'panic!))
                   (~a main-message (if continued-message (~a ";\n " continued-message) ""))
                   fields))
  (raise (exn:fail:panic message (current-continuation-marks) exit-code)))

;; ---------------------------------------------------------------------------------------------------
;; safe error raising

; When raising an error, string representations of values that led to the error are often included in
; the error message, and various error-raising functions like `raise-arguments-error` automatically do
; this using the current value of `error-value->string-handler`. However, it’s possible that printing
; a value will actually raise an exception, which isn’t terrible, but it does lose some context. These
; variations of error-raising functions capture any exceptions raised while printing a value and
; include them in the error message as extra context.

(define (~e/safe v)
  (with-handlers ([exn:fail? values])
    (~e v)))

(define (indent-string n str)
  (define indent-str (make-string n #\space))
  (string-join (for/list ([line (in-list (string-split str "\n" #:trim? #f))])
                 (if (equal? line "")
                     line
                     (string-append indent-str line)))
               "\n"))

(define (raise-argument-error name expected v)
  (define (do-raise-flow-trace-argument-error name expected v)
    (define v-result (~e/safe v))
    (define message
      (if (string? v-result)
          (~a name ": contract violation\n"
              "  expected: " expected "\n"
              "  given: " v-result)
          (~a name ": contract violation;\n"
              " additionally, an exception was raised while trying to print the given value\n"
              "  expected: " expected "\n"
              "  exception:\n" (indent-string 3 (exn-message v-result)))))
    (raise (exn:fail:contract message (current-continuation-marks))))

  (unless (symbol? name)
    (do-raise-flow-trace-argument-error 'raise-flow-trace-argument-error "symbol?" name))
  (unless (string? expected)
    (do-raise-flow-trace-argument-error 'raise-flow-trace-argument-error "string?" expected))
  (do-raise-flow-trace-argument-error name expected v))

(define (build-arguments-error-message name message fields)
  (define-values [all-fields exn-fields]
    (for/fold ([all-fields '()]
               [exn-fields '()]
               #:result (values (reverse all-fields) (reverse exn-fields)))
              ([(name value) (in-alist fields)])
      (unless (string? name)
        (raise-argument-error 'raise-flow-trace-arguments-error "string?" name))
      (define value-result (~e/safe value))
      (define result (cons name (if (string? value-result) value-result (exn-message value-result))))
      (if (string? value-result)
          (values (cons result all-fields) exn-fields)
          (values (cons result all-fields) (cons result exn-fields)))))

  (define num-exns (length exn-fields))
  (define continued-message
    (cond
      [(zero? num-exns)
       ""]
      [(= num-exns 1)
       (~a ";\n additionally, an exception was raised while trying to print the "
           (car (first exn-fields)) " value")]
      [else
       ";\n additionally, an exception was raised while trying to print some values"]))

  (define fields-message
    (string-append*
     (for/list ([(field-name field-value) (in-alist all-fields)])
       (~a "\n  " field-name ":"
           (if (string? field-value)
               (if (string-contains? field-value "\n")
                   (~a "\n" (indent-string 3 field-value))
                   (~a " " field-value))
               " <raised exception>")))))

  (define exceptions-message
    (cond
      [(zero? num-exns)
       ""]
      [(= num-exns 1)
       (~a "\n  exception:\n" (indent-string 3 (cdr (first exn-fields))))]
      [else
       (~a "\n  exceptions...:"
           (string-append*
            (for/list ([(field-name field-value) (in-alist all-fields)])
              (define full-field-name (if (string-suffix? field-name "...")
                                          field-name
                                          (~a field-name "...")))
              (~a "\n   " full-field-name ":\n" (indent-string 4 field-value)))))]))

  (~a name ": " message
      fields-message
      exceptions-message))

(define (raise-arguments-error name message . fields)
  (unless (symbol? name)
    (raise-argument-error 'raise-flow-trace-arguments-error "symbol?" name))
  (unless (string? message)
    (raise-argument-error 'raise-flow-trace-arguments-error "string?" message))
  (unless (even? (length fields))
    (define field-str-result (~e/safe (last fields)))
    (define message
      (if (string? field-str-result)
          (~a "raise-flow-trace-arguments-error: missing value after field string;\n"
              " (i.e. an odd number of arguments were provided)\n"
              "  field string: " field-str-result)
          (~a "raise-flow-trace-arguments-error: missing value after field string;\n"
              " (i.e. an odd number of arguments were provided)\n"
              " additionally, an exception was raised while trying to print the field string\n"
              "  exception:\n" (indent-string 3 (exn-message field-str-result)))))
    (raise (exn:fail:contract:arity message (current-continuation-marks))))

  (define full-message (build-arguments-error-message
                        name
                        message
                        (for/list ([field+value (in-slice 2 fields)])
                          (cons (first field+value) (second field+value)))))

  (raise (exn:fail:contract full-message (current-continuation-marks))))
