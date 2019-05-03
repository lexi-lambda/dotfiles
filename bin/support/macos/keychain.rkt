#lang racket/base

(require (rename-in ffi/unsafe [-> _->] [_pointer _pointer/null])
         ffi/unsafe/alloc
         ffi/unsafe/define
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/splicing
         "../contract.rkt"
         "../sequence.rkt")

;; ---------------------------------------------------------------------------------------------------
;; foreign libraries

; These are loaded via absolute path to ensure we’re really getting the system frameworks, rather than
; something that might be in the search path trying to steal our passwords.

(define foundation-framework
  (ffi-lib "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"))
(define security-framework
  (ffi-lib "/System/Library/Frameworks/Security.framework/Security"))

(define-ffi-definer define-core-foundation security-framework)
(define-ffi-definer define-security security-framework)

;; ---------------------------------------------------------------------------------------------------
;; general FFI utils

(define int-layout/c (or/c 'int8 'uint8 'int16 'uint16 'int32 'uint32 'int64 'uint64))
(define pointer-layout/c (or/c 'pointer 'gcpointer 'fpointer))

(define (ctype->base-type type)
  (match (ctype->layout type)
    ['int8 _int8]
    ['uint8 _uint8]
    ['int16 _int16]
    ['uint16 _uint16]
    ['int32 _int32]
    ['uint32 _uint32]
    ['int64 _int64]
    ['uint64 _uint64]
    ['float _float]
    ['double _double]
    ['bool _stdbool]
    ['void _void]
    ['pointer _pointer/null]
    ['gcpointer _gcpointer]
    ['fpointer _fpointer]
    ['bytes _bytes]
    ['string/ucs-4 _string/ucs-4]
    ['string/utf-16 _string/utf-16]))

(define ((make-ctype/c-compare compare) self other)
  (and (ctype/c? other)
       (compare (ctype/c-layout-ctc self)
                (ctype/c-layout-ctc other))
       (compare (ctype/c-in-ctc self)
                (ctype/c-in-ctc other))
       (compare (ctype/c-out-ctc self)
                (ctype/c-out-ctc other))))

(struct ctype/c (layout-ctc in-ctc out-ctc)
  #:constructor-name make-ctype/c
  #:omit-define-syntaxes
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:custom-print-quotable 'never
  #:property prop:contract
  (build-contract-property
   #:name
   (λ (self)
     (define layout-ctc (ctype/c-layout-ctc self))
     (define in-ctc (ctype/c-in-ctc self))
     (define out-ctc (ctype/c-out-ctc self))
     (define in-and-out-same? (or (eq? in-ctc out-ctc)
                                  (contract-equivalent? in-ctc out-ctc)))
     `(ctype/c ,@(if (and in-and-out-same?
                          (equal? in-ctc any/c))
                     '()
                     (list (contract-name in-ctc)))
               ,@(if in-and-out-same?
                     '()
                     (list (contract-name out-ctc)))
               ,@(if (equal? layout-ctc any/c)
                     '()
                     (list '#:layout (contract-name layout-ctc)))))
   #:first-order
   (λ (self)
     (define layout? (contract-first-order (ctype/c-layout-ctc self)))
     (λ (val)
       (and (ctype? val)
            (layout? (ctype->layout val)))))
   #:late-neg-projection
   (λ (self)
     (define layout-proj (get/build-late-neg-projection (ctype/c-layout-ctc self)))
     (define in-proj (get/build-late-neg-projection (ctype/c-in-ctc self)))
     (define out-proj (get/build-late-neg-projection (ctype/c-out-ctc self)))
     (λ (blame)
       (define layout-blame (blame-add-context blame "the layout of"))
       (define in-blame (blame-add-context blame "a Racket value given to" #:swap? #t))
       (define out-blame (blame-add-context blame "a Racket value produced by"))
       (define layout-proj+blame (layout-proj layout-blame))
       (define out-proj+blame (out-proj out-blame))
       (define in-proj+blame (in-proj in-blame))
       (λ (val missing-party)
         (unless (ctype? val)
           (raise-blame-error blame #:missing-party missing-party val
                              '(expected: "ctype?" given: "~e") val))
         (with-contract-continuation-mark (cons layout-blame missing-party)
           (layout-proj+blame (ctype->layout val) missing-party))
         (define out-blame+missing-party (cons out-blame missing-party))
         (define in-blame+missing-party (cons in-blame missing-party))
         (make-ctype val
                     (λ (rkt-val)
                       (with-contract-continuation-mark in-blame+missing-party
                         (in-proj+blame rkt-val missing-party)))
                     (λ (c-val)
                       (with-contract-continuation-mark out-blame+missing-party
                         (out-proj+blame c-val missing-party)))))))
   #:stronger (make-ctype/c-compare contract-stronger?)
   #:equivalent (make-ctype/c-compare contract-equivalent?)))

(define/subexpression-pos-prop (ctype/c [in-ctc any/c] [out-ctc in-ctc] #:layout [layout-ctc any/c])
  (make-ctype/c (coerce-flat-contract 'ctype/c layout-ctc)
                (coerce-contract 'ctype/c in-ctc)
                (coerce-contract 'ctype/c out-ctc)))

(define/contract (_or-false type)
  (-> (ctype/c #:layout int-layout/c) ctype?)
  (define base-type (ctype->base-type type))
  (make-ctype base-type
              (λ (v) (if v (cast v type base-type) 0))
              (λ (v) (if (zero? v) #f (cast v base-type type)))))

(define/contract (_not-null type #:name [who '_not-null])
  (->* [(ctype/c (not/c #f) #:layout pointer-layout/c)]
       [#:name symbol?]
       ctype?)
  (define (check-not-null v)
    (unless v
      (raise-argument-error who "(not/c #f)" v))
    v)
  (make-ctype type check-not-null check-not-null))

(define _pointer (_not-null _pointer/null #:name '_pointer))

; like `_enum`, but allows arbitrary values of the base type, and optionally allows unknown values to
; be passed through unchanged
(splicing-let ([RKT (new-∀/c 'RKT)]
               [C (new-∀/c 'C)])
  (define/contract (_enum* #:name who
                           #:base-type [base-type _ufixint]
                           #:unknown [unknown 'error]
                           mappings)
    (->* [#:name symbol?
          (alist/c symbol? RKT)]
         [#:base-type (ctype/c RKT C)
          #:unknown (or/c 'error 'pass)]
         (ctype/c symbol? C))
    (define rkt-to-c (make-hasheq mappings))
    (define c-to-rkt (for/hash ([(rkt c) (in-alist mappings)])
                       (values c rkt)))
    (make-ctype
     base-type
     (λ (v)
       (hash-ref rkt-to-c v (λ () (match unknown
                                    ['error
                                     (raise-argument-error who "a known symbol" v)]
                                    ['pass
                                     v]))))
     (λ (v)
       (hash-ref c-to-rkt v (λ () (match unknown
                                    ['error
                                     (raise-argument-error who "a known value of the base type" v)]
                                    ['pass
                                     v])))))))

;; ---------------------------------------------------------------------------------------------------
;; CoreFoundation and Security types and constants

(define _CFIndex _long)
(define _CFTypeRef _pointer)
(define _OSStatus _sint32)

(define-cstruct _CFRange ([location _CFIndex] [length _CFIndex]))

(define-cpointer-type _CFStringRef)
(define-cpointer-type _SecKeychainRef)
(define-cpointer-type _SecKeychainAttributeListRef)
(define-cpointer-type _SecKeychainItemRef)
(define-cpointer-type _SecPasswordRef)

; Several macOS APIs use a `FourCharCode` type that uses four byte character literals represented by
; 32-bit, unsigned integers to make enum constants more meaningful. This type provides nice conversion
; to and from such values, representing them as four-byte byte strings on the Racket side.
(define (make-_four-char-code who big-endian?)
  (make-ctype
   _uint32
   (λ (v)
     (unless (and (bytes? v) (= (bytes-length v) 4))
       (raise-argument-error who "(and/c bytes? (property/c bytes-length 4))" v))
     (integer-bytes->integer v #f big-endian?))
   (λ (v)
     (integer->integer-bytes v 4 #f big-endian?))))

(define _four-char-code (make-_four-char-code '_four-char-code (system-big-endian?)))
; Strangely, `SecAuthenticationType` explicitly ensures its values always use big endian, even though
; other constants that use `FourCharCode` don’t do that. I’m not sure why.
(define _four-char-code/big-endian (make-_four-char-code '_four-char-code/big-endian #t))

(define _SecProtocolType
  (_or-false (_enum* #:name '_SecProtocolType
                     #:base-type _four-char-code
                     '([afp         . #"afp "]
                       [apple-talk  . #"atlk"]
                       [cifs        . #"cifs"]
                       [cvs-pserver . #"cvsp"]
                       [daap        . #"daap"]
                       [eppc        . #"eppc"]
                       [ftp         . #"ftp "]
                       [ftp-account . #"ftpa"]
                       [ftp-proxy   . #"ftpx"]
                       [ftps        . #"ftps"]
                       [http        . #"http"]
                       [http-proxy  . #"htpx"]
                       [https       . #"htps"]
                       [https-proxy . #"htsx"]
                       [imap        . #"imap"]
                       [imaps       . #"imps"]
                       [ipp         . #"ipp "]
                       [irc         . #"irc "]
                       [ircs        . #"ircs"]
                       [ldap        . #"ldap"]
                       [ldaps       . #"ldps"]
                       [nntp        . #"nntp"]
                       [nntps       . #"ntps"]
                       [pop3        . #"pop3"]
                       [pop3s       . #"pops"]
                       [rtsp        . #"rtsp"]
                       [rtsp-proxy  . #"rtsx"]
                       [smb         . #"smb "]
                       [smtp        . #"smtp"]
                       [socks       . #"sox "]
                       [ssh         . #"ssh "]
                       [svn         . #"svn "]
                       [telnet      . #"teln"]
                       [telnets     . #"tels"]))))

(define _SecAuthenticationType
  (_or-false (_enum* #:name '_SecAuthenticationType
                     #:base-type _four-char-code/big-endian
                     '([default     . #"dflt"]
                       [dpa         . #"dpaa"]
                       [html-form   . #"form"]
                       [http-basic  . #"http"]
                       [http-digest . #"httd"]
                       [msn         . #"msna"]
                       [ntlm        . #"ntlm"]
                       [rpa         . #"rpaa"]))))

;; ---------------------------------------------------------------------------------------------------
;; Security exception types

(define OSStatus:security:item-not-found -25300)

(struct exn:fail:contract:security exn:fail:contract ())
(struct exn:fail:contract:security:not-found exn:fail:contract:security ())
(struct exn:fail:contract:security:user-canceled exn:fail:contract:security ())

;; ---------------------------------------------------------------------------------------------------
;; CoreFoundation wrappers

(define-core-foundation CFRelease
  (_fun _CFTypeRef _-> _void)
  #:wrap (releaser))

(define utf-16-to-utf-8-converter (bytes-open-converter "platform-UTF-16" "platform-UTF-8"))

(define-core-foundation CFStringGetLength
  (_fun _CFStringRef _-> _CFIndex))
(define-core-foundation CFStringGetCharacters
  (_fun [str-ref : _CFStringRef]
        [range : _CFRange]
        [buffer : (_bytes o (* 2 (CFRange-length range)))]
        _-> _void
        _-> (match/values (bytes-convert utf-16-to-utf-8-converter buffer)
              [[utf8-buffer _ 'complete]
               (begin0
                 (bytes->string/utf-8 utf8-buffer)
                 (void/reference-sink str-ref))])))
(define (CFString->string str-ref)
  (CFStringGetCharacters str-ref (make-CFRange 0 (CFStringGetLength str-ref))))

;; ---------------------------------------------------------------------------------------------------
;; Security wrappers

(define-security SecCopyErrorMessageString
  (_fun _OSStatus [_pointer/null = #f] _-> _CFStringRef/null)
  #:wrap (allocator CFRelease))

(define (check-security-status-code! who status)
  (unless (zero? status)
    (define message (SecCopyErrorMessageString status))
    (raise-arguments-error who (if message
                                   (CFString->string message)
                                   "foreign procedure returned with non-zero exit code")
                           "exit code" status)))

(define-security SecKeychainItemFreeContent
  (_fun _SecKeychainAttributeListRef/null
        _pointer/null
        _-> [status : _OSStatus]
        _-> (check-security-status-code! 'SecKeychainItemFreeContent status))
  #:wrap (compose1 (deallocator second) (deallocator first)))

(define fail-tag (make-continuation-prompt-tag))
(define (failable proc)
  (make-keyword-procedure
   (λ (kws kw-args . args)
     (call-with-continuation-prompt
      (λ () (keyword-apply proc kws kw-args args))
      fail-tag
      (λ (v) (if (procedure? v) (v) v))))))
(define (fail v)
  (abort-current-continuation fail-tag v))

(define-security keychain-find-internet-password
  (let ()
    (define (string-length/zero str)
      (if str (string-length str) 0))
    (define (maybe-attr name v)
      (if v (list name v) '()))
    (_fun (#:keychain [keychain #f]
           #:host [host #f]
           #:domain [domain #f]
           #:username [username #f]
           #:path [path #f]
           #:port [port #f]
           #:protocol [protocol #f]
           #:authentication [authentication #f]
           #:failure [failure-result
                      (λ () (apply raise-arguments-error
                                   'keychain-find-internet-password "no password with attributes"
                                   (append (maybe-attr "keychain" keychain)
                                           (maybe-attr "host" host)
                                           (maybe-attr "domain" domain)
                                           (maybe-attr "username" username)
                                           (maybe-attr "path" path)
                                           (maybe-attr "port" port)
                                           (maybe-attr "protocol" protocol)
                                           (maybe-attr "authentication" authentication))))])
          ::
          [keychain : _SecKeychainRef/null]
          [_uint32 = (string-length/zero host)]
          [host : _string/utf-8]
          [_uint32 = (string-length/zero domain)]
          [domain : _string/utf-8]
          [_uint32 = (string-length/zero username)]
          [username : _string/utf-8]
          [_uint32 = (string-length/zero path)]
          [path : _string/utf-8]
          [_uint16 = (or port 0)]
          [protocol : _SecProtocolType]
          [authentication : _SecAuthenticationType]
          [password-length : (_ptr o _uint32)]
          [password : (_ptr o (_bytes o password-length))]
          [_SecKeychainItemRef/null = #f]
          _-> [status : _OSStatus]
          _-> (cond
                [(= status OSStatus:security:item-not-found)
                 (fail failure-result)]
                [else
                 (check-security-status-code! 'SecKeychainFindInternetPassword status)
                 password])))
  #:c-id SecKeychainFindInternetPassword
  #:wrap (compose1 failable (allocator (λ (v) (SecKeychainItemFreeContent #f v)))))

#;(keychain-find-internet-password ;#:host "smtp.gmail.com"
                                 ;#:username "lexi.lambda@gmail.com"
                                 #:protocol 'smtp
                                 ;#:authentication 'default
                                 )
