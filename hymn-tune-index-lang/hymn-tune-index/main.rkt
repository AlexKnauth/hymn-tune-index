#lang racket/base

(provide (rename-out [module-begin #%module-begin]))

(require racket/pretty
         syntax/parse/define
         json
         (for-syntax racket/base
                     racket/format
                     racket/list
                     racket/match
                     syntax/stx))

(begin-for-syntax
  (define-syntax-class hymnal
    #:datum-literals [WC LEVAS A&M-R A&M-NS]
    [pattern 1982]
    [pattern WC]
    [pattern LEVAS]
    [pattern A&M-R]
    [pattern A&M-NS])

  (define major (list 0 2 4 5 7 9 11))
  (define (major-degree i) (list-ref major (sub1 i)))

  (define-syntax-class scale-degree
    #:attributes [n]
    #:datum-literals [b2 b3 b4 b5 b6 b7 \#1 \#2 \#3 \#4 \#5 \#6]
    [pattern {~and d {~or 1 2 3 4 5 6 7}}
      #:attr n (major-degree (syntax-e #'d))]
    [pattern {~and d {~or b2 b3 b4 b5 b6 b7}}
      #:do [(define d*
              (string->number (substring (symbol->string (syntax-e #'d)) 1)))]
      #:attr n (sub1 (major-degree d*))]
    [pattern {~and d {~or \#1 \#2 \#3 \#4 \#5 \#6}}
      #:do [(define d*
              (string->number (substring (symbol->string (syntax-e #'d)) 1)))]
      #:attr n (add1 d*)])

  (define-syntax-class scale-degrees
    #:attributes [[n 1]]
    [pattern (:scale-degree ...)])

  (define-syntax-class hymn-number
    [pattern :number])

  (define-syntax-class hymn-info
    #:attributes [hymnal number scale-degrees [scale-degrees.n 1]]
    [pattern (hymnal:hymnal number:hymn-number scale-degrees:scale-degrees)])

  (define (list-of-number<? as bs)
    (match* [as bs]
      [['() '()] #false]
      [['() (cons _ _)] #true]
      [[(cons _ _) '()] #false]
      [[(cons a _) (cons b _)] #:when (< a b) #true]
      [[(cons a _) (cons b _)] #:when (< b a) #false]
      [[(cons a as) (cons b bs)] #:when (= a b) (list-of-number<? as bs)]))
  )

(define-simple-macro (module-begin h:hymn-info ...)
  #:do [(define hs*
          (for/list ([h      (in-list (attribute h))]
                     [deg-ns (in-list (attribute h.scale-degrees.n))])
            (list deg-ns h)))
        (define (hymn-info-json h)
          (syntax-parse h
            [h:hymn-info
             #`(#,(~a (syntax->datum #'h.hymnal))
                #,(~a (syntax->datum #'h.number))
                #,(map ~a (syntax->datum #'h.scale-degrees)))]))
        (define (make-tree entries)
          (cond
            [(empty? entries) (hash)]
            [else
             (define-values [entries-here entries-more]
               (partition (λ (e) (empty? (first e))) entries))
             (define groups
               (group-by (λ (e) (first (first e))) entries-more))
             (define here
               (hash '|| (map hymn-info-json (map second entries-here))))
             (for/fold ([acc here])
                       ([g (in-list groups)])
               (define n (first (first (first g))))
               (define k (string->symbol (number->string n)))
               (define sub (make-tree (for/list ([e (in-list g)])
                                        (match e
                                          [(list ns h) (list (rest ns) h)]))))
               (hash-set acc k sub))]))]
  #:with tree (make-tree hs*)
  (#%module-begin
   (provide json)
   (define json 'tree)
   (module+ main
     (write-json json)
     (newline))))

