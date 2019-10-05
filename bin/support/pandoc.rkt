#lang racket/base

(require net/url-structs
         racket/contract
         "tree-parse.rkt")

(provide
 (contract-out
  [version? flat-contract?]
  [pandoc-supported-version? flat-contract?]

  (struct document ([metadata metadata?] [content (listof block?)]))
  [metadata? flat-contract?]

  [block? predicate/c]
  (struct block:plain ([content (listof inline?)]))
  (struct block:paragraph ([content (listof inline?)]))
  (struct block:line ([content (listof (listof inline?))]))
  (struct block:code ([attributes attributes?] [content string?]))
  (struct block:raw ([format string?] [content string?]))
  (struct block:quote ([content (listof block?)]))
  (struct block:list ([items list?]) #:omit-constructor)
  (struct block:list:unordered ([items (listof (listof block?))]))
  (struct block:list:ordered ([items (listof (listof block?))]
                              [start exact-integer?]
                              [style ordered-list-style?]
                              [delimiter ordered-list-delimiter?]))
  (struct block:list:definition ([items (listof (cons/c (listof inline?) (listof (listof block?))))]))
  (struct block:header ([level exact-positive-integer?]
                        [attributes attributes?]
                        [content (listof inline?)]))
  [block:horizontal-rule? predicate/c]
  [block:horizontal-rule block?]
  (struct block:table ([caption (listof inline?)]
                       [alignments (listof table-column-alignment?)]
                       [widths (listof real?)]
                       [headers (listof (listof block?))]
                       [rows (listof (listof (listof block?)))]))
  (struct block:div ([attributes attributes?] [content (listof block?)]))
  [block:null? predicate/c]
  [block:null block?]

  (struct attributes ([id string?]
                      [classes (listof string?)]
                      [others (and/c (hash/c symbol? string? #:immutable #t) hash-equal?)]))

  [ordered-list-style? flat-contract?]
  [ordered-list-delimiter? flat-contract?]
  [table-column-alignment? flat-contract?]

  [inline? predicate/c]
  (struct inline:string ([content string?]))
  (struct inline:emph ([content (listof inline?)]))
  (struct inline:strong ([content (listof inline?)]))
  (struct inline:strikeout ([content (listof inline?)]))
  (struct inline:superscript ([content (listof inline?)]))
  (struct inline:subscript ([content (listof inline?)]))
  (struct inline:small-caps ([content (listof inline?)]))
  (struct inline:quoted ([type quote-type?] [content (listof inline?)]))
  (struct inline:cite ([citations (listof citation?)] [content (listof inline?)]))
  (struct inline:code ([attributes attributes?] [content string?]))
  [inline:space? predicate/c]
  [inline:space inline?]
  [inline:line-break? predicate/c]
  [inline:line-break:soft? predicate/c]
  [inline:line-break:soft inline?]
  [inline:line-break:hard? predicate/c]
  [inline:line-break:hard inline?]
  (struct inline:math ([type math-type?] [content string?]))
  (struct inline:raw ([format string?] [content string?]))
  (struct inline:link ([attributes attributes?] [content (listof inline?)] [target link-target?]))
  (struct inline:image ([attributes attributes?] [content (listof inline?)] [target link-target?]))
  (struct inline:note ([content (listof block?)]))
  (struct inline:span ([attributes attributes?] [content (listof inline?)]))

  (struct citation ([id string?]
                    [prefix (listof inline?)]
                    [suffix (listof inline?)]
                    [mode citation-mode?]
                    [note-num exact-nonnegative-integer?]
                    [hash fixnum?]))
  [citation-mode? flat-contract?]

  [quote-type? flat-contract?]
  [math-type? flat-contract?]

  (struct link-target ([url url?] [title string?]))))

;; ---------------------------------------------------------------------------------------------------
;; structs and predicates

(define version? (listof exact-nonnegative-integer?))
(define pandoc-supported-version? (cons/c 1 (cons/c 17 any/c)))

(struct document (metadata content) #:transparent)

(define (make-singleton name #:super [super #f])
  (struct s ()
    #:reflection-name name
    #:super super
    #:property prop:custom-print-quotable 'never
    #:property prop:custom-write
    (λ (self out mode)
      (fprintf out (if (eq? mode 0) "~a" "#<~a>") name)))
  (values s? (s)))

(struct block () #:transparent)
(struct block:plain block (content) #:transparent)
(struct block:paragraph block (content) #:transparent)
(struct block:line block (content) #:transparent)
(struct block:code block (attributes content) #:transparent)
(struct block:raw block (format content) #:transparent)
(struct block:quote block (content) #:transparent)
(struct block:list block (items) #:transparent)
(struct block:list:unordered block:list () #:transparent)
(struct block:list:ordered block:list (start style delimiter) #:transparent)
(struct block:list:definition block:list () #:transparent)
(struct block:header block (level attributes content) #:transparent)
(define-values [block:horizontal-rule? block:horizontal-rule]
  (make-singleton 'block:horizontal-rule #:super struct:block))
(struct block:table block (caption alignments widths headers rows) #:transparent)
(struct block:div block (attributes content) #:transparent)
(define-values [block:null? block:null] (make-singleton 'block:null #:super struct:block))

(struct attributes (id classes others) #:transparent)

(define ordered-list-style?
  (or/c 'default 'example 'decimal 'lower-roman 'upper-roman 'lower-alpha 'upper-alpha))
(define ordered-list-delimiter? (or/c 'default 'period 'one-paren 'two-parens))

(define table-column-alignment? (or/c 'default 'left 'right 'center))

(struct inline () #:transparent)
(struct inline:string inline (content) #:transparent)
(struct inline:emph inline (content) #:transparent)
(struct inline:strong inline (content) #:transparent)
(struct inline:strikeout inline (content) #:transparent)
(struct inline:superscript inline (content) #:transparent)
(struct inline:subscript inline (content) #:transparent)
(struct inline:small-caps inline (content) #:transparent)
(struct inline:quoted inline (type content) #:transparent)
(struct inline:cite inline (citations content) #:transparent)
(struct inline:code inline (attributes content) #:transparent)
(define-values [inline:space? inline:space] (make-singleton 'inline:space #:super struct:inline))
(struct inline:line-break inline () #:transparent)
(define-values [inline:line-break:soft? inline:line-break:soft]
  (make-singleton 'inline:line-break:soft #:super struct:inline:line-break))
(define-values [inline:line-break:hard? inline:line-break:hard]
  (make-singleton 'inline:line-break:hard #:super struct:inline:line-break))
(struct inline:math inline (type content) #:transparent)
(struct inline:raw inline (format content) #:transparent)
(struct inline:link inline (attributes content target) #:transparent)
(struct inline:image inline (attributes content target) #:transparent)
(struct inline:note inline (content) #:transparent)
(struct inline:span inline (attributes content) #:transparent)

(define quote-type? (or/c 'single 'double))

(struct citation (id prefix suffix mode note-num hash) #:transparent)
(define citation-mode? (or/c 'normal 'author-in-text 'suppress-author))

(define math-type? (or/c 'display 'inline))

(struct link-target (url title) #:transparent)

(define metadata?
  (flat-rec-contract metadata?
    (and/c (hash/c symbol? metadata? #:immutable #t) hash-eq?)
    (listof metadata?)
    boolean?
    (listof block?)
    (listof inline?)))

;; ---------------------------------------------------------------------------------------------------
;; reading

(define version/p (and/p (satisfy/p string?)
                         (λ (str)
                           )))
