#lang racket/base

(require racket/contract
         racket/format
         racket/path
         racket/string
         syntax/location
         "../error.rkt")

(provide panic!
         (contract-out
          [require-os! (-> (listof symbol?) void?)]))

;; ---------------------------------------------------------------------------------------------------

(define run-file-path (find-system-path 'run-file))
(define run-file (file-name-from-path run-file-path))
(define run-file-sym (string->symbol (path->string run-file)))

(define (string-prefix-lines str prefix #:prefix-empty? [prefix-empty? #f])
  (string-join (map (λ (line) (if (zero? (string-length line))
                                  line
                                  (string-append prefix line)))
                    (string-split str "\n" #:trim? #f))
               "\n"))

(define ((make-script-error-display-handler [super (error-display-handler)]) message exn)
  (cond
    [(exn:break? exn)
     (newline (current-error-port))]
    [(exn:fail:panic? exn)
     (displayln message (current-error-port))]
    [(exn:fail:user? exn)
     (super message exn)]
    [else
     (super (~a run-file-sym ": uncaught exception;\n" (string-prefix-lines message " ")) exn)]))

(define current-os (system-type 'os))
(define (require-os! os-syms)
  (unless (memq current-os os-syms)
    (panic! "operating system not supported"
            #:fields `(["supported systems" . ,(unquoted-printing-string
                                                (string-join (map ~v os-syms) ", "))]
                       ["current system" . ,current-os]))))

;; ---------------------------------------------------------------------------------------------------
;; lang

(module reader syntax/module-reader
  #:language (quote-module-path ".." lang)
  (require syntax/location))

(module* lang racket/base
  (require (for-syntax racket/base
                       syntax/location)
           syntax/parse/define
           (submod ".."))

  (provide (except-out (all-from-out racket/base) #%module-begin)
           (rename-out [script-module-begin #%module-begin])
           (all-from-out (submod "..")))

  (define-syntax-parser script-module-begin
    [(_ form ...)
     (quasisyntax/loc this-syntax
       (#%module-begin
        (module configure-runtime racket/base
          (require #,(quote-module-path ".." for-configure-runtime))
          (configure-runtime!))
        form ...))]))

(module* for-configure-runtime #f
  (provide configure-runtime!)
  (define (configure-runtime!)
    (dynamic-require (quote-module-path ".." configure-runtime) #f)
    (current-panic-message-prefix run-file-sym)
    (uncaught-exception-handler (make-uncaught-panic-exception-handler))
    (error-display-handler (make-script-error-display-handler))))
