#lang racket/base

(require "../../error.rkt"
         "../script.rkt")

(provide (all-from-out "../script.rkt"))

; TODO: Provide wrapper around execv.

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
  (require (prefix-in script: (submod "../script.rkt" for-configure-runtime)))
  (provide configure-runtime!)
  (define (configure-runtime!)
    (script:configure-runtime!)
    (current-panic-exit-code 125)))
