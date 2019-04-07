#lang racket/base

(provide parse-indentation-single-deep
         parse-indentation-multiple-deep)

(require racket/list
         racket/match
         syntax/stx)
(module+ test
  (require rackunit))

;; Natural -> [Syntax -> Boolean]
(define ((syntax-column>? col) stx)
  (> (syntax-column stx) col))

;; Natural -> [Syntax -> Boolean]
(define ((syntax-line<=? ln) stx)
  (<= (syntax-line stx) ln))

;; Syntax -> Boolean
(define (stx-keyword? stx)
  (or (keyword? stx) (and (syntax? stx) (keyword? (syntax-e stx)))))

;; Syntax -> Syntax
(define (parse-indentation-single-deep x)
  (cond
    [(stx->list x)
     =>
     (λ (xs)
       (match (map parse-indentation-single-deep xs)
         [(list y)
          (datum->syntax x (list y) x x)]
         [xs
          (datum->syntax
           x
           (syntax-e (parse-indentation-single xs))
           x
           x)]))]
    [else x]))

;; [Listof Syntax] -> [Listof Syntax]
(define (parse-indentation-multiple-deep xs)
  (parse-indentation-multiple (map parse-indentation-single-deep xs)))

;; [Listof Syntax] -> [Listof Syntax]
(define (parse-indentation-multiple xs)
  (match xs
    ['() '()]
    [(list x) (list x)]
    ;; TODO: what about keywords?
    [(cons x rst)
     (define col (syntax-column x))
     (define-values [x-subs after] (splitf-at rst (syntax-column>? col)))
     (cons (parse-indentation-single (cons x x-subs))
           (parse-indentation-multiple after))]))

;; [Listof Syntax] -> Syntax
(define (parse-indentation-single xs)
  (match xs
    ['()      (wrap-list '())]
    [(list x) x]
    [(cons x rst)
     (define ln (syntax-line x))
     (define-values [same-line after-line] (splitf-at rst (syntax-line<=? ln)))
     (define x-line (cons x same-line))
     (let loop ([rev-xs (reverse x-line)] [acc '()] [rst after-line])
       (match rev-xs
         ['() (wrap-list (append acc (parse-indentation-multiple rst)))]
         [(list x)
          (wrap-list (cons x (append acc (parse-indentation-multiple rst))))]
         [(cons last-x xs)
          (define col (syntax-column last-x))
          (define-values [x-subs after-x] (splitf-at rst (syntax-column>? col)))
          (match x-subs
            ['() (loop xs (cons last-x acc) after-x)]
            [x-subs
             (define x/subs
               (parse-indentation-single (cons last-x (append acc x-subs))))
             (loop xs (list x/subs) after-x)])]))]))

;; [Listof Syntax] -> Syntax
(define (wrap-list stxs)
  (match stxs
    ['()        (datum->syntax #f '())]
    [(list stx) stx]
    [(list  first-stx _ ... last-stx)
     (define source (syntax-source first-stx))
     (define line (syntax-line first-stx))
     (define column (syntax-column first-stx))
     (define start (syntax-position first-stx))
     (define end (+ (syntax-position last-stx)
                    (syntax-span last-stx)))
     (define span (- end start))
     (datum->syntax first-stx stxs (list source line column start span))]))

;; ----------------------------------------------------

(module+ test
  (define stx   #'(define f xs = ~> for (x in xs)
                                      define y =
                                        + x 1
                                      number->string y
                                    accumulate
                                 return
                                   map string->number accumulated
                      where
                        accumulated =
                          accumulatable ()
                   define thing = f
                                    xs
                   thing))
  (define shape '((define f xs (= (~> (for (x in xs)
                                        (define y =
                                          (+ x 1))
                                        (number->string y))
                                      accumulate)
                                  (return
                                    (map string->number accumulated)))
                    (where
                      (accumulated =
                        (accumulatable ()))))
                  (define thing = (f
                                     xs))
                  thing))

  (check-equal? (map syntax->datum
                     (parse-indentation-multiple (syntax->list stx)))
                shape)

  (check-equal? (map syntax->datum
                     (parse-indentation-multiple-deep
                      (parse-indentation-multiple (syntax->list stx))))
                shape)
  )
