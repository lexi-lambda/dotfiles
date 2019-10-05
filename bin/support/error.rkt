#lang racket/base

(require racket/contract
         racket/format
         racket/string)

(provide (struct-out exn:fail:panic)
         current-panic-message-prefix
         current-panic-exit-code
         (contract-out
          [make-uncaught-panic-exception-handler (->* [] [(-> any/c any)] (-> any/c any))]
          [panic! (->* [string?]
                       [string?
                        #:fields (listof (cons/c string? any/c))
                        #:exit-code any/c]
                       any)]))

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

(define current-panic-message-prefix (make-parameter 'panic!))
(define current-panic-exit-code (make-parameter #f))

(define (panic! main-message [continued-message #f]
                #:fields [fields '()]
                #:exit-code [exit-code (current-panic-exit-code)])
  (define message
    (~a (current-panic-message-prefix) ": "
        main-message (if continued-message (~a ";\n " continued-message) "")
        (string-append* (for/list ([field (in-list fields)])
                          (~a "\n  " (car field) ": " (~e (cdr field)))))))
  (raise (exn:fail:panic message (current-continuation-marks) exit-code)))
