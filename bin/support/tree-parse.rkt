#lang racket/base

(require (for-syntax racket/base)
         data/order
         racket/contract
         (only-in racket/contract/collapsible get/build-collapsible-late-neg-projection)
         racket/format
         racket/list
         racket/match
         racket/pretty
         racket/set
         racket/string
         (only-in racket/unsafe/ops unsafe-struct*-cas!)
         syntax/parse/define)

(provide (contract-out
          [parser? predicate/c]
          [parser/c (-> contract? contract? contract?)]
          [parse (parametric->/c [I O]
                   (->* [(parser/c I O) I] [#:who symbol?] O))]
          (struct exn:fail:parse ([message string?] [continuation-marks continuation-mark-set?]))

          ; basic constructors
          [succeed/p (parametric->/c [I O]
                       (-> O (parser/c I O)))]
          [fail/p (parametric->/c [I O]
                    (-> string? (parser/c I O)))]
          [satisfy/p (parametric->/c [A]
                       (-> (-> A any/c) (parser/c A A)))]
          [equal?/p (parametric->/c [A]
                      (->* [A] [(-> A A any/c)] (parser/c A A)))]

          ; controlling the parse context
          [mask/p (parametric->/c [I O]
                    (-> (-> (parametric->/c [J P]
                              (-> (parser/c J P)
                                  (parser/c J P)))
                            (parser/c I O))
                         (parser/c I O)))]
          [opaque/p (parametric->/c [I O]
                      (-> (parser/c I O) (parser/c I O)))]
          [describe/p (parametric->/c [I O]
                        (->* [(parser/c I O)]
                             [#:name (or/c string? #f)
                              #:opaque? any/c]
                             (parser/c I O)))]
          [post/p (parametric->/c [I O]
                    (-> (parser/c I O) (parser/c I O)))]

          ; cuts and commits
          [cut/p (parser/c any/c void?)]
          [delimit-cut/p (parametric->/c [I O]
                           (-> (parser/c I O) (parser/c I O)))]
          [commit/p (parametric->/c [I O]
                      (-> (parser/c I O) (parser/c I O)))]

          ; mapping
          [map/p (parametric->/c [I J O P]
                   (->* [(parser/c I O)]
                        [#:in (-> J I)
                         #:out (-> O P)]
                        (parser/c J P)))]
          [observe/p (parametric->/c [I O]
                       (-> (-> I (parser/c I O)) (parser/c I O)))]

          ; choice
          [or/p (parametric->/c [I O]
                  (-> (parser/c I O) ... (parser/c I O)))]

          ; sequencing
          [and/p (parametric->/c [I O]
                   (-> (parser/c I O) (-> O (parser/c I O)) ... (parser/c I O)))]
          [and/fold/p (parametric->/c [I O A]
                        (-> (-> O A A) A (parser/c I O) ... (parser/c I A)))]
          [and/fold1/p (parametric->/c [I O A]
                         (-> (-> O A A) (parser/c I A) (parser/c I O) ... (parser/c I A)))]
          [and/list/p (parametric->/c [I O]
                        (-> (parser/c I O) ... (parser/c I (listof O))))]
          [and/first/p (parametric->/c [I O]
                         (-> (parser/c I O) (parser/c I O) ... (parser/c I O)))]
          [and/last/p (parametric->/c [I O]
                        (-> (parser/c I O) (parser/c I O) ... (parser/c I O)))]

          ; atoms
          [symbol/p (->* [symbol?] [#:name (or/c string? #f)] (parser/c any/c symbol?))]
          [string/p (->* [symbol?] [#:name (or/c string? #f)] (parser/c any/c string?))]

          ; structures
          [cons/p (parametric->/c [I O J P]
                    (->* [(parser/c I O)
                          (parser/c J P)]
                         [#:name (or/c string? #f)]
                         (parser/c (cons/c I J) (cons/c O P))))]
          [list/p (->* [] [#:name (or/c string? #f)] #:rest (listof parser?) parser?)]
          [listof/p (parametric->/c [I O]
                      (-> (parser/c I O) (parser/c (listof I) (listof O))))]))

;; ---------------------------------------------------------------------------------------------------
;; fake-parametric->/c

; Parsers don’t actually really work with parametric contracts, since it’s hard to ensure terms get
; unwrapped before they end up in failure messages, but it’s nice to still be able to notationally
; express the parametricity they ought to support, so this fake version of `parametric->/c` just
; treats the “variable” contracts as `any/c`.
(struct named-any/c (name)
  #:property prop:object-name (struct-field-index name)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write contract-custom-write-property-proc
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name (λ (self) (named-any/c-name self))
   #:first-order (λ (self) (λ (val) #t))
   #:stronger (λ (self ctc) (contract-stronger? ctc any/c))
   #:equivalent (λ (self ctc) (contract-equivalent? ctc any/c))))

(struct fake-parametric->/c (var-names ctc)
  #:reflection-name 'parametric->/c
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write contract-custom-write-property-proc)

(define (fake-parametric->/c-ctc* v)
  (if (fake-parametric->/c? v)
      (fake-parametric->/c-ctc* (fake-parametric->/c-ctc v))
      v))

(define (make-fake-parametric->/c-property build-property)
  (build-property
   #:name (λ (self)
            `(parametric->/c ,(fake-parametric->/c-var-names self)
               ,(contract-name (fake-parametric->/c-ctc self))))
   #:first-order (λ (self) (contract-first-order (fake-parametric->/c-ctc self)))
   #:late-neg-projection (λ (self) (get/build-late-neg-projection (fake-parametric->/c-ctc self)))
   #:collapsible-late-neg-projection (λ (self) (get/build-collapsible-late-neg-projection
                                                (fake-parametric->/c-ctc self)))
   #:stronger (λ (self ctc) (contract-stronger? (fake-parametric->/c-ctc self)
                                                (fake-parametric->/c-ctc* ctc)))
   #:equivalent (λ (self ctc) (contract-equivalent? (fake-parametric->/c-ctc self)
                                                    (fake-parametric->/c-ctc* ctc)))))

(struct flat-fake-parametric->/c fake-parametric->/c ()
  #:property prop:flat-contract (make-fake-parametric->/c-property build-flat-contract-property))
(struct chaperone-fake-parametric->/c fake-parametric->/c ()
  #:property prop:chaperone-contract (make-fake-parametric->/c-property
                                      build-chaperone-contract-property))
(struct impersonator-fake-parametric->/c fake-parametric->/c ()
  #:property prop:contract (make-fake-parametric->/c-property build-contract-property))

(define (make-fake-parametric->/c var-names raw-ctc)
  (define ctc (coerce-contract 'parametric->/c raw-ctc))
  (cond
    [(flat-contract? ctc)
     (flat-fake-parametric->/c var-names ctc)]
    [(chaperone-contract? ctc)
     (chaperone-fake-parametric->/c var-names ctc)]
    [else
     (impersonator-fake-parametric->/c var-names ctc)]))

(define-simple-macro (parametric->/c [x:id ...] ctc:expr)
  (make-fake-parametric->/c '[x ...] (let ([x (named-any/c 'x)] ...) ctc)))

;; ---------------------------------------------------------------------------------------------------
;; persistent queues

(struct promise ([thunk #:mutable]) #:authentic)
(define (force v)
  (if (promise? v)
      ((promise-thunk v))
      v))
(define (delay/proc maker)
  (define (pr-thunk)
    (define v (maker))
    (if (unsafe-struct*-cas! pr 0 pr-thunk (λ () v))
        v
        ((promise-thunk pr))))
  (define pr (promise pr-thunk))
  pr)
(define-syntax-parser delay
  [(_ body ...+)
   (quasisyntax/loc this-syntax
     (delay/proc #,(syntax/loc this-syntax
                     (λ () body ...))))])

; (stream*of a b) = (promise/c (cons/c a (streamof a))) | b
; (streamof a) = (stream*of a '())

(define (queue=?/recur q1 q2 recur)
  (and (eqv? (queue-length q1) (queue-length q2))
       (for/and ([e1 (in-queue q1)]
                 [e2 (in-queue q2)])
         (recur e1 e2))))

(define (queue-hash-code/recur q recur)
  (recur (cons 'queue (for/list ([e (in-queue q)]) e))))

; queue : (queueof a)
;   front : (stream*of a (listof a))
;   rear : (listof a)
;   front-len, rear-len : exact-nonnegative-integer?
;     front-len = (length front)
;     rear-len = (length rear)
;     front-len >= rear-len
(struct queue (front front-len rear rear-len)
  #:property prop:equal+hash (list queue=?/recur queue-hash-code/recur queue-hash-code/recur))

(define empty-queue (queue '() 0 '() 0))

; (stream*of a (listof a)) (promise/c (listof a)) -> (stream*of a (listof a))
(define (queue-do-rotate front rear)
  (delay (let promise-loop ([front front])
           (if (promise? front)
               (let* ([front (force front)]
                      [front-rest (cdr front)])
                 (cons (car front) (delay (promise-loop front-rest))))
               (let list-loop ([front front])
                 (if (empty? front)
                     (force rear)
                     (let ([front-rest (cdr front)])
                       (cons (car front) (delay (list-loop front-rest))))))))))

(define (queue-rotate-if-needed front front-len rear rear-len)
  (if (>= front-len rear-len)
      (queue front front-len rear rear-len)
      (queue (queue-do-rotate front (delay (reverse rear))) (+ front-len rear-len) '() 0)))

(define (queue-empty? q)
  (zero? (queue-front-len q)))
(define (queue-length q)
  (+ (queue-front-len q) (queue-rear-len q)))
(define (enqueue q v)
  (queue-rotate-if-needed (queue-front q)
                          (queue-front-len q)
                          (cons v (queue-rear q))
                          (add1 (queue-rear-len q))))
(define (dequeue q)
  (define front (force (queue-front q)))
  (values (car front)
          (queue-rotate-if-needed (cdr front)
                                  (sub1 (queue-front-len q))
                                  (queue-rear q)
                                  (queue-rear-len q))))
(define (queue-first q)
  (car (force (queue-front q))))
(define (queue-rest q)
  (queue-rotate-if-needed (cdr (force (queue-front q)))
                          (sub1 (queue-front-len q))
                          (queue-rear q)
                          (queue-rear-len q)))

(define (in-queue q)
  (define front (queue-front q))
  (define rear (queue-rear q))
  (make-do-sequence
   (λ ()
     (define (do-next/front-promise front)
       (if (promise? front)
           (cons do-next/front-promise (force front))
           (cons do-next/front-list front)))
     (define (do-next/front-list front)
       (if (empty? front)
           (cons do-next/rear (force rear))
           (cons do-next/front-list front)))
     (define (do-next/rear rear) rear)
     (values (λ (s) (car (cdr s)))
             (λ (s) (cons (car s) (cdr (cdr s))))
             (λ (s) ((car s) (cdr s)))
             (cons do-next/front-promise (force front))
             (λ (pos) (not (empty? (cdr pos))))
             #f
             #f))))

(define (queue-foldr f v q)
  (let loop ([q q])
    (if (queue-empty? q)
        v
        (let-values ([(e q) (dequeue q)])
          (f e (loop q))))))

; XXX: Currently not very fast, could be lazier
(define (queue-append q1 q2)
  (for/fold ([q q1])
            ([e (in-queue q2)])
    (enqueue q e)))

; XXX: Currently not very fast, only handles flat contracts, not very good error reporting
(define (queueof raw-ctc)
  (define ctc (coerce-flat-contract 'queueof raw-ctc))
  (define ctc? (contract-first-order ctc))
  (make-flat-contract
   #:name (build-compound-type-name 'queueof raw-ctc)
   #:first-order (λ (v) (and (queue? v) (for/and ([e (in-queue v)]) (ctc? e))))))

;; ---------------------------------------------------------------------------------------------------
;; progress

; Progress frames track parse progress for the purpose of error reporting: when multiple branches of a
; parser fail, the ones that made the “most progress” are used to report an error.
;
;   - `(progress-frame:recur _i)` represents a step of parse progress that recursively walked into a
;     data structure at index `_i`. This is the most common kind of progress frame.
;
;   - `progress-frame:prefer` expresses a strong preference for the failure over other failures. When
;     two failures share a common progress frame prefix, but the next frame is
;     `progress-frame:prefer` and the other is not, the failure with `progress-frame:prefer` is always
;     chosen, regardless of the other frames in the other failure.
;
;   - `(progress-frame:parse _term)` represents a nested parse of `_term` initiated from within
;     the enclosing parser, where `_term` is possibly unrelated to the original term. Progress inside
;     two different terms is uncorrelatable.
;
; The precision afforded by `progress-frame:recur` frames also allows progress frames to serve an
; extra purpose: they effectively provide a “path” into the structure being parsed. Two lists of
; progress frames can be assumed to refer to the same subpiece of the structure if they consist of
; exactly the same `progress-frame:recur` frames, in the same order.

(struct progress-frame () #:transparent)
(struct progress-frame:recur progress-frame (index) #:transparent
  #:guard (struct-guard/c exact-nonnegative-integer?))
(define-values [progress-frame:prefer? progress-frame:prefer]
  (let ()
    (struct progress-frame:prefer progress-frame ()
      #:property prop:custom-print-quotable 'never
      #:property prop:custom-write
      (λ (self out mode) (fprintf out (if (eq? mode 0) "~a" "#<~a>") 'progress-frame:prefer)))
    (values progress-frame:prefer? (progress-frame:prefer))))
(struct progress-frame:parse progress-frame (term) #:transparent)

(define (progress-frame-order pf1 pf2)
  (match pf1
    [(progress-frame:recur i1)
     (match pf2
       [(progress-frame:recur i2)
        (real-order i1 i2)]
       [(? progress-frame:prefer?)
        '<]
       [(? progress-frame:parse?)
        #f])]
    [(? progress-frame:prefer?)
     (match pf2
       [(? progress-frame:recur?)
        '>]
       [(? progress-frame:prefer?)
        '=]
       [(? progress-frame:parse?)
        #f])]
    [(progress-frame:parse e)
     (match pf2
       [(progress-frame:parse (== e eq?))
        '=]
       [_
        #f])]))
(define (progress-frame=? pf1 pf2)
  (eq? (progress-order pf1 pf2) '=))

(define progress/c (queueof progress-frame?))
(define (progress-order p1 p2)
  (if (queue-empty? p1)
      (if (queue-empty? p2) '= '<)
      (cond
        [(queue-empty? p2) '>]
        [else
         (define-values [pf1 rest-p1] (dequeue p1))
         (define-values [pf2 rest-p2] (dequeue p2))
         (match (progress-frame-order pf1 pf2)
           ['= (progress-order rest-p1 rest-p2)]
           [other other])])))
(define (progress=? p1 p2)
  (eq? (progress-order p1 p2) '=))

; Merges two progress queues by taking the common prefix, then adding an incomparable progress frame
; at the point of divergence. This is used by `failure-merge` to preserve information about
; incomparable failures.
(define (progress-merge p1 p2)
  (define (diverged p)
    ; add a parse frame with a unique value to ensure the result is incomparable with all other frames
    ; at this point
    (enqueue p (progress-frame:parse (gensym 'diverged))))
  (let loop ([acc empty-queue]
             [p1 p1]
             [p2 p2])
    (if (queue-empty? p1)
        (if (queue-empty? p2) acc (diverged acc))
        (cond
          [(queue-empty? p2) (diverged acc)]
          [else
           (define-values [pf1 rest-p1] (dequeue p1))
           (define-values [pf2 rest-p2] (dequeue p2))
           (if (progress-frame=? pf1 pf2)
               (loop (enqueue acc pf1) rest-p1 rest-p2)
               (diverged))]))))

;; ---------------------------------------------------------------------------------------------------
;; expect frames and expect trees

; Expect frames track information about the terms currently being parsed for the purposes of
; constructing error messages.
;
;   - `(expect-frame:named _name _term _progress)` uses `_name` as a name to describe `_term`, which
;     is the term being parsed. The `_progress` field is used to track whether or not two frames name
;     the same term.
;
;   - `(expect-frame:parse _term)` indicates that a nested parse of `_term` was initiated from within
;     the enclosing parser, where `_term` is possibly unrelated to the original term.

(struct expect-frame () #:transparent)
(struct expect-frame:named expect-frame (name term progress) #:transparent
  #:guard (struct-guard/c string? any/c progress/c))
(struct expect-frame:parse expect-frame (term) #:transparent)

(define (expect-frame=? ef1 ef2)
  (or (eq? ef1 ef2)
      (match* [ef1 ef2]
        [[(struct* expect-frame:named ([name name-1] [progress progress-1]))
          (struct* expect-frame:named ([name name-2] [progress progress-2]))]
         (and (string=? name-1 name-2) (progress=? progress-1 progress-2))]
        [[(struct* expect-frame:parse ([term term-1]))
          (struct* expect-frame:parse ([term term-2]))]
         (eq? term-1 term-2)])))


; context
;   progress-frames : progress/c
;   progress-favor : exact-nonnegative-integer?
;   expect-frames : (listof expect-frame?)
;   masks : (or/c (listof symbol?) 'opaque)
(struct context (progress-frames progress-favor expect-frames masks) #:transparent)

(define (context-opaque? ctx)
  (eq? (context-masks ctx) 'opaque))
(define (context-masked? ctx)
  (or (context-opaque? ctx)
      (not (empty? (context-masks ctx)))))

(define empty-context (context empty-queue 0 '() '()))

(define (make-child-context ctx)
  (if (context-masked? ctx)
      (struct-copy context empty-context [masks (context-masks ctx)])
      empty-context))

(define (context-add-progress-frame ctx frame)
  (if (context-opaque? ctx)
      ctx
      (struct-copy context ctx [progress-frames (enqueue (context-progress-frames ctx) frame)])))
(define (context-add-progress-favor ctx favor)
  (if (context-opaque? ctx)
      ctx
      (struct-copy context ctx [progress-favor (+ favor (context-progress-favor ctx))])))
(define (context-add-expect-frame ctx frame)
  (if (context-masked? ctx)
      ctx
      (struct-copy context ctx [expect-frames (cons frame (context-expect-frames ctx))])))

(define (context-make-opaque ctx)
  (if (context-opaque? ctx)
      ctx
      (struct-copy context ctx [masks 'opaque])))
(define (context-mask ctx m)
  (if (context-opaque? ctx)
      ctx
      (struct-copy context ctx [masks (cons m (context-masks ctx))])))
(define (context-unmask ctx m)
  (if (context-opaque? ctx)
      ctx
      (struct-copy context ctx [masks (remq m (context-masks ctx))])))


; Expect trees combine information from multiple expect queues.

(struct expect-tree () #:transparent)
(define-values [expect-tree:empty? expect-tree:empty]
  (let ()
    (struct expect-tree:empty expect-tree ())
    (values expect-tree:empty? (expect-tree:empty))))
(struct expect-tree:frame expect-tree (frame rest) #:transparent
  #:guard (struct-guard/c expect-frame? expect-tree?))
(struct expect-tree:branch expect-tree (branches) #:transparent
  #:guard (struct-guard/c (non-empty-listof (or/c expect-tree:empty? expect-tree:frame?))))

(define (make-expect-tree frames)
  (expect-tree-prepend expect-tree:empty frames))
(define (expect-tree-prepend et frames)
  (foldl expect-tree:frame et frames))

(define (expect-tree-last-frames et)
  (match et
    [(== expect-tree:empty eq?)
     '()]
    [(expect-tree:frame ef rest-et)
     (match rest-et
       [(== expect-tree:empty eq?)
        (list ef)]
       [(expect-tree:frame _ _)
        (expect-tree-last-frames rest-et)]
       [(expect-tree:branch ets)
        (define-values [pre-ets post-ets] (splitf-at ets (λ (et) (not (eq? et expect-tree:empty)))))
        (define pre-frames (append-map expect-tree-last-frames pre-ets))
        (if (empty? post-ets)
            pre-frames
            (append pre-frames (cons ef (append-map expect-tree-last-frames (rest post-ets)))))])]
    [(expect-tree:branch ets)
     (append-map expect-tree-last-frames ets)]))

(define (expect-tree-merge et1 et2)
  (define (do-merge-frame+branch ef1 rest-et1 ets2 not-found)
    (let loop ([ets2 ets2])
      (match ets2
        ['()
         (not-found)]
        [(cons et2 rest-ets2)
         (match et2
           [(expect-tree:frame (== ef1 expect-frame=?) rest-et2)
            (expect-tree:frame ef1 (expect-tree-merge rest-et1 rest-et2))]
           [_
            (cons et2 (loop rest-ets2))])])))

  (match* [et1 et2]
    ; empty cases
    [[(? expect-tree:empty?) (? expect-tree:empty?)]
     expect-tree:empty]
    [[(? expect-tree:empty?) (expect-tree:frame _ _)]
     (expect-tree:branch (list expect-tree:empty et2))]
    [[(? expect-tree:empty?) (expect-tree:branch ets2)]
     (expect-tree:branch (cons expect-tree:empty (remq expect-tree:empty ets2)))]
    [[(expect-tree:frame _ _) (? expect-tree:empty?)]
     (expect-tree:branch (list expect-tree:empty et2))]
    [[(expect-tree:branch ets1) (? expect-tree:empty?)]
     (expect-tree:branch (if (memq expect-tree:empty ets1)
                             ets1
                             (append ets1 (list expect-tree:empty))))]
    ; frame + frame
    [[(expect-tree:frame ef1 rest-et1) (expect-tree:frame ef2 rest-et2)]
     (if (expect-frame=? ef1 ef2)
         (expect-tree:frame ef1 (expect-tree-merge rest-et1 rest-et2))
         (expect-tree:branch (list et1 et2)))]
    ; frame + branch
    [[(expect-tree:frame ef1 rest-et1) (expect-tree:branch ets2)]
     (expect-tree:branch
      (let/ec escape
        (do-merge-frame+branch ef1 rest-et1 ets2 (λ () (escape (cons ef1 ets2))))))]
    [[(expect-tree:branch ets1) (expect-tree:frame ef2 rest-et2)]
     (expect-tree:branch (do-merge-frame+branch ef2 rest-et2 ets1 (λ () (list ef2))))]
    ; branch + branch
    [[(expect-tree:branch ets1) (expect-tree:branch ets2)]
     (expect-tree:branch
      (for/fold ([ets ets1])
                ([et2 (in-list ets2)])
        (let loop ([ets ets])
          (match ets
            ['()
             (list et2)]
            [(cons et1 rest-ets)
             (match* [et1 et2]
               [[(? expect-tree:empty?) (? expect-tree:empty?)]
                (cons expect-tree:empty rest-ets)]
               [[(expect-tree:frame ef1 rest-et1) (expect-tree:frame ef2 rest-et2)]
                (if (expect-frame=? ef1 ef2)
                    (cons (expect-tree:frame ef1 (expect-tree-merge rest-et1 rest-et2)) rest-ets)
                    (cons et1 (loop rest-ets)))])]))))]))

;; ---------------------------------------------------------------------------------------------------
;; failures

; Failures encapsulate information about parse failures, which may combine information from multiple
; contexts. Expect trees are created lazily, since they aren’t needed until reporting errors, and
; computing them eagerly would be a lot of wasted work if the parse is going to succeed.

(struct failure (message expect-tree-maker progress-frames progress-favor) #:transparent
  #:guard (struct-guard/c (or/c string? #f)
                          (-> expect-tree?)
                          (queueof progress-frame?)
                          exact-nonnegative-integer?))
(define (failure-expect-tree f)
  ((failure-expect-tree-maker f)))

(define (failure-order f1 f2)
  (match (progress-order (failure-progress-frames f1) (failure-progress-frames f2))
    ['= (real-order (failure-progress-favor f1) (failure-progress-favor f2))]
    [#f #f]
    [other other]))

(define (make-failure ctx #:message [message #f])
  (failure message
           (let ([frames (context-expect-frames ctx)])
             (λ () (make-expect-tree frames)))
           (context-progress-frames ctx)
           (context-progress-favor ctx)))

(define ((expect-tree-maker-merge maker-1 maker-2))
  (expect-tree-merge (maker-1) (maker-2)))

(define (failure-merge f1 f2)
  (match (failure-order f1 f2)
    ['> f1]
    ['< f2]
    ['= (failure #f
                 (expect-tree-maker-merge (failure-expect-tree-maker f1)
                                          (failure-expect-tree-maker f2))
                 (failure-progress-frames f1)
                 (failure-progress-favor f1))]
    [#f (failure #f
                 (expect-tree-maker-merge (failure-expect-tree-maker f1)
                                          (failure-expect-tree-maker f2))
                 (progress-merge (failure-progress-frames f1)
                                 (failure-progress-frames f2))
                 0)]))

(define (failure-reroot parent-ctx child-f)
  (failure (failure-message child-f)
           (let ([frames (context-expect-frames parent-ctx)]
                 [maker (failure-expect-tree-maker child-f)])
             (λ () (expect-tree-prepend (maker) frames)))
           (queue-append (context-progress-frames parent-ctx) (failure-progress-frames child-f))
           (+ (context-progress-favor parent-ctx) (failure-progress-favor child-f))))

(struct exn:fail:parse exn:fail ())

(define (open-pretty-string-port)
  (define out (open-output-string))
  (port-count-lines! out)
  (port-display-handler out (λ (v out) (pretty-display v out #:newline? #f)))
  (port-write-handler out (λ (v out) (pretty-write v out #:newline? #f)))
  (port-print-handler out (λ (v out [depth 0]) (pretty-print v out depth #:newline? #f)))
  out)

(define (call-with-pretty-output-string proc)
  (define out (open-pretty-string-port))
  (proc out)
  (get-output-string out))

(define (expect-tree->context-string indent tree)
  (define (pretty-print-term indent term out)
    (define overflow-prompt-tag (make-continuation-prompt-tag))
    (call-with-continuation-prompt
     overflow-prompt-tag
     (λ ()
       (define term-out (make-tentative-pretty-print-output-port
                         out
                         (pretty-print-columns)
                         (λ ()
                           (tentative-pretty-print-port-cancel term-out)
                           (abort-current-continuation overflow-prompt-tag))))
       (write-string " " term-out)
       (parameterize ([pretty-print-depth 2])
         (print term term-out))
       (tentative-pretty-print-port-transfer term-out out))
     (λ ()
       (fprintf out "\n~a " indent)
       (parameterize ([pretty-print-depth 2])
         (print term out)))))

  (string-join
   (let loop ([lines '()]
              [tree tree])
     (match tree
       [(? expect-tree:empty?)
        lines]
       [(expect-tree:frame frame rest-tree)
        (loop (cons (call-with-pretty-output-string
                     (λ (out)
                       (match frame
                         [(struct* expect-frame:named ([name name] [term term]))
                          (fprintf out "~a~a:" indent name)
                          (pretty-print-term indent term out)]
                         [(struct* expect-frame:parse ([term term]))
                          (fprintf out "~athe subterm" indent)
                          (pretty-print-term indent term out)])))
                    lines)
              rest-tree)]
       [(expect-tree:branch subtrees)
        (define indent* (~a indent " "))
        (cons (string-join
               (cons (~a indent "different things...:")
                     (map (λ (subtree) (expect-tree->context-string indent* subtree)) subtrees))
               "\n")
              lines)]))
   "\n"))

(define (raise-parse-error who f term)
  (define expect-tree (failure-expect-tree f))
  (define last-frames (expect-tree-last-frames expect-tree))
  (define-values [names subterm] (if (empty? last-frames)
                                     (values '() term)
                                     (values (for/set ([frame (in-list last-frames)]
                                                       #:when (expect-frame:named? frame))
                                               (expect-frame:named-name frame))
                                             (match (first last-frames)
                                               [(struct* expect-frame:named ([term term]))
                                                term]
                                               [(struct* expect-frame:parse ([term term]))
                                                term]))))
  (define message (or (failure-message f)
                      (if (set-empty? names)
                          "parse failure"
                          (string-join (sort (set->list names) string<?) ", "
                                       #:before-first "expected "
                                       #:before-last (if (= (set-count names) 2)
                                                         " or "
                                                         ", or ")))))
  (raise (exn:fail:parse (~a who ": " message
                             "\n  at: " (~e subterm)
                             "\n  in: " (~e term)
                             "\n  while parsing...:"
                             "\n" (expect-tree->context-string "   " expect-tree))
                         (current-continuation-marks))))

;; ---------------------------------------------------------------------------------------------------
;; parsers and parser contracts

(struct parser (name proc wrapped-parser maybe-orig-parser)
  #:guard (let ([parser? (λ (v) (parser? v))])
            (struct-guard/c (or/c string? #f)
                            (let ([fail/c (-> failure? any/c)])
                              (-> any/c
                                  context?
                                  (-> any/c fail/c any/c)
                                  fail/c
                                  fail/c
                                  any/c))
                            (or/c parser? #f)
                            (or/c parser? #f)))
  #:property prop:impersonator-of (λ (self) (parser-wrapped-parser self))
  ; racket/racket#2644
  #:property prop:equal+hash
  (list (λ (a b recur) (eq? (parser-orig-parser a) (parser-orig-parser b)))
        (λ (self recur) (eq-hash-code (parser-orig-parser self)))
        (λ (self recur) (eq-hash-code (parser-orig-parser self))))
  #:property prop:object-name (struct-field-index name)
  #:property prop:custom-write
  (λ (self out mode)
    (define name (parser-name self))
    (if name
        (fprintf out "#<parser:~a>" name)
        (write-string "#<parser>" out))))

(define (parser-orig-parser self)
  (or (parser-maybe-orig-parser self) self))

(define (make-parser proc #:name [name #f])
  (parser name proc #f #f))
(define (redirect-parser p wrap-proc)
  (struct-copy parser p
               [proc (wrap-proc (parser-proc p))]
               [wrapped-parser p]
               [maybe-orig-parser (parser-orig-parser p)]))

(struct base-parser/c (in-ctc out-ctc)
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write contract-custom-write-property-proc)

(define (parser/c-name self)
  (build-compound-type-name 'parser/c (base-parser/c-in-ctc self) (base-parser/c-out-ctc self)))

(struct any-parser/c base-parser/c ()
  #:property prop:flat-contract
  (build-flat-contract-property
   #:name parser/c-name
   #:first-order (λ (self) parser?)
   #:stronger (λ (self ctc) (contract-stronger? ctc parser?))
   #:equivalent (λ (self ctc) (contract-equivalent? ctc parser?))))

(define (make-parser/c-contract-property build-property redirect-procedure)
  (build-property
   #:name parser/c-name
   #:first-order (λ (self) parser?)
   #:late-neg-projection
   (λ (self)
     (define in-proj (get/build-late-neg-projection (base-parser/c-in-ctc self)))
     (define out-proj (get/build-late-neg-projection (base-parser/c-out-ctc self)))
     (λ (blame)
       (define in-blame (blame-add-context blame "an input to" #:swap? #t))
       (define out-blame (blame-add-context blame "an output from"))
       (define in-proj/blame (in-proj in-blame))
       (define out-proj/blame (out-proj out-blame))
       (λ (val missing-party)
         (unless (parser? val)
           (raise-blame-error blame #:missing-party missing-party val
                              `(expected: "parser?" given: "~e") val))
         (define in-blame+missing-party (cons in-blame missing-party))
         (define out-blame+missing-party (cons out-blame missing-party))
         (redirect-parser
          val
          (λ (proc)
            (redirect-procedure
             proc
             (λ (e ctx ok fail abort)
               (values (with-contract-continuation-mark in-blame+missing-party
                         (in-proj/blame e missing-party))
                       ctx
                       (redirect-procedure
                        ok
                        (λ (v fail)
                          (values (with-contract-continuation-mark out-blame+missing-party
                                    (out-proj/blame v missing-party))
                                  fail)))
                       fail
                       abort))))))))))

(struct chaperone-parser/c base-parser/c ()
  #:property prop:chaperone-contract
  (make-parser/c-contract-property build-chaperone-contract-property chaperone-procedure))
(struct impersonator-parser/c base-parser/c ()
  #:property prop:contract
  (make-parser/c-contract-property build-contract-property impersonate-procedure))

(define (parser/c raw-in-ctc raw-out-ctc)
  (define in-ctc (coerce-contract 'parser/c raw-in-ctc))
  (define out-ctc (coerce-contract 'parser/c raw-out-ctc))
  (cond
    [(and (flat-contract? in-ctc)
          (flat-contract? out-ctc)
          (contract-equivalent? in-ctc any/c)
          (contract-equivalent? out-ctc any/c))
     (any-parser/c in-ctc out-ctc)]
    [(and (chaperone-contract? in-ctc) (chaperone-contract? out-ctc))
     (chaperone-parser/c in-ctc out-ctc)]
    [else
     (impersonator-parser/c in-ctc out-ctc)]))

;; ---------------------------------------------------------------------------------------------------
;; primitive combinators

(define (parse p e #:who [who 'parse])
  (define (fail f)
    (raise-parse-error who f e))
  ((parser-proc p) e empty-context (λ (v fail) v) fail fail))

(define (succeed/p v)
  (make-parser
   (λ (e ctx ok fail abort)
     (ok v fail))))

(define (fail/p message)
  (make-parser
   (λ (e ctx ok fail abort)
     (fail (make-failure ctx #:message message)))))

(define (satisfy/p pred)
  (make-parser
   (λ (e ctx ok fail abort)
     (if (pred e)
         (ok e fail)
         (fail (make-failure ctx))))))

(define (equal?/p v1 [=? equal?])
  (satisfy/p (λ (v2) (=? v1 v2))))

(define (mask/p f)
  (define (make-unmask/p m)
    (procedure-rename
     (λ (p)
       (define proc (parser-proc p))
       (make-parser
        (λ (e ctx ok fail abort)
          (proc e (context-unmask ctx m) ok fail abort))))
     (string->symbol (~a "un" m "/p"))))
  (make-parser
   (λ (e ctx ok fail abort)
     (define m (gensym 'mask))
     (define proc (parser-proc (f (make-unmask/p m))))
     (proc e (context-mask ctx m) ok fail abort))))

(define (opaque/p p)
  (define proc (parser-proc p))
  (make-parser
   (λ (e ctx ok fail abort)
     (proc e (context-make-opaque ctx) ok fail abort))))

(define (describe/p p #:name [name #f] #:opaque? [opaque? #f])
  (if (or name opaque?)
      (let ([proc (parser-proc p)])
        (make-parser
         #:name name
         (λ (e ctx ok fail abort)
           (define named-ctx (if name
                                 (context-add-expect-frame
                                  ctx
                                  (expect-frame:named name e (context-progress-frames ctx)))
                                 ctx))
           (define opaque-ctx (if opaque? (context-make-opaque named-ctx) named-ctx))
           (proc e opaque-ctx ok fail abort))))
      p))

(define (post/p p)
  (define proc (parser-proc p))
  (make-parser
   (λ (e ctx ok fail abort)
     (proc e (context-add-progress-frame ctx progress-frame:prefer) ok fail abort))))

(define (parse/p p e2 #:hidden? [hidden? #f])
  (define proc (parser-proc p))
  (make-parser
   (λ (e1 ctx ok fail abort)
     (define progress-ctx (context-add-progress-frame ctx (progress-frame:parse e2)))
     (proc e2
           (if hidden?
               progress-ctx
               (context-add-expect-frame progress-ctx (expect-frame:parse e2)))
           ok
           fail
           abort))))

(define cut/p
  (make-parser
   #:name "cut"
   (λ (e ctx ok fail abort)
     (ok (void) abort))))

(define (delimit-cut/p p)
  (define proc (parser-proc p))
  (make-parser
   (λ (e ctx ok fail abort)
     (proc e ctx ok fail fail))))

(define (commit/p p)
  (define proc (parser-proc p))
  (make-parser
   (λ (e1 ctx ok fail1 abort)
     (proc e1 ctx (λ (e2 fail2) (ok e2 fail1)) fail1 fail1))))

(define (map/p p #:in [f values] #:out [g values])
  (define proc (parser-proc p))
  (make-parser
   (λ (e1 ctx ok fail1 abort)
     (proc (f e1) ctx (λ (e2 fail2) (ok (g e2) fail2)) fail1 abort))))

(define (observe/p f)
  (make-parser
   (λ (e ctx ok fail abort)
     ((parser-proc (f e)) e ctx ok fail abort))))

(define (or/p . ps)
  (define procs (map parser-proc ps))
  (make-parser
   (λ (e ctx ok fail abort)
     (define child-ctx (make-child-context ctx))
     (let try ([procs procs]
               [f #f])
       (match procs
         ['()
          (fail (if f
                    (failure-reroot ctx f)
                    (make-failure ctx)))]
         [(cons proc procs)
          (proc e
                child-ctx
                ok
                (λ (new-f) (try procs (if f (failure-merge f new-f) new-f)))
                abort)])))))

(define (and/p p . fs)
  (define proc (parser-proc p))
  (define (next fs e ctx ok abort)
    (match fs
      ['()
       ok]
      [(cons f fs)
       (λ (v fail)
         ((parser-proc (f v)) e
                              ctx
                              (next fs)
                              fail
                              abort))]))
  (make-parser
   (λ (e ctx ok fail abort)
     (proc e
           ctx
           (next fs e ctx ok abort)
           fail
           abort))))

(define (and/fold/p f init . ps)
  (define procs (map parser-proc ps))
  (make-parser
   (λ (e ctx ok fail abort)
     (let loop ([fail fail]
                [acc init]
                [procs procs])
       (match procs
         ['()
          (ok acc fail)]
         [(cons proc procs)
          (proc e
                ctx
                (λ (v fail)
                  (loop fail (f v acc) procs))
                fail
                abort)])))))

(define (and/fold1/p f p . ps)
  (define proc (parser-proc p))
  (make-parser
   (λ (e ctx ok fail abort)
     (proc e
           ctx
           (λ (v fail)
             (define ps-proc (parser-proc (apply and/fold/p f v ps)))
             (ps-proc e ctx ok fail abort))
           fail
           abort))))

(define (and/list/p . ps)
  (map/p #:out reverse (apply and/fold/p cons '() ps)))
(define (and/first/p p . ps)
  (apply and/fold1/p (λ (new old) old) p ps))
(define (and/last/p p . ps)
  (apply and/fold1/p (λ (new old) new) p ps))

;; ---------------------------------------------------------------------------------------------------

(define (symbol/p sym #:name [name (~a "the literal symbol " (~v sym))])
  (describe/p #:name name (satisfy/p (λ (v) (eq? v sym)))))

(define (string/p str #:name [name (~a "the literal string " (~v str))])
  (describe/p #:name name (satisfy/p (λ (v) (equal? v str)))))

;; ---------------------------------------------------------------------------------------------------

(define (cons/p p1 p2 #:name [name "a pair"])
  (define proc1 (parser-proc p1))
  (define proc2 (parser-proc p2))
  (describe/p
   #:name name
   (make-parser
    (λ (e ctx ok fail0 abort)
      (if (pair? e)
          (proc1 (car e)
                 (context-add-progress-frame ctx (progress-frame:recur 0))
                 (λ (v1 fail1)
                   (proc2 (cdr e)
                          (context-add-progress-frame ctx (progress-frame:recur 1))
                          (λ (v2 fail2) (ok (cons v1 v2) fail2))
                          fail1
                          abort))
                 fail0
                 abort)
          (fail0 (make-failure ctx)))))))

(define-values [not-given? not-given]
  (let ()
    (struct not-given ())
    (values not-given? (not-given))))

(define (list/p #:name [name not-given] . ps)
  (define num-ps (length ps))
  (define procs (map parser-proc ps))
  (describe/p
   #:name (if (not-given? name) (~a "a list of " num-ps " element" (if (= num-ps 1) "" "s")) name)
   (make-parser
    (λ (e ctx ok fail abort)
      (if (and (list? e) (= (length e) num-ps))
          (let next ([es e]
                     [i 0]
                     [procs procs]
                     [vs '()]
                     [fail fail])
            (match* [es procs]
              [['() _]
               (ok (reverse vs) fail)]
              [[(cons e es) (cons proc procs)]
               (proc e
                     (context-add-progress-frame ctx (progress-frame:recur i))
                     (λ (v fail) (next es (add1 i) procs (cons v vs) fail))
                     fail
                     abort)]))
          (fail (make-failure ctx)))))))

(define (listof/p p #:name [name "a list"])
  (define proc (parser-proc p))
  (make-parser
   #:name name
   (λ (e ctx ok fail abort)
     (if (list? e)
         (let next ([es e]
                    [i 0]
                    [vs '()]
                    [fail fail])
           (match es
             ['()
              (ok (reverse vs) fail)]
             [(cons e es)
              (proc e
                    (context-add-progress-frame ctx (progress-frame:recur i))
                    (λ (v fail) (next es (add1 i) (cons v vs) fail))
                    fail
                    abort)]))
         (fail (make-failure ctx))))))
