#lang s-exp syntax/module-reader hymn-tune-index
#:wrapper1 (λ (rd)
             (parameterize ([current-readtable (make-no-hash-readtable)])
               (rd)))
#:module-wrapper (λ (get-stx)
                   (define stx (get-stx))
                   (syntax-parse stx
                     [(m n l {~and b (mb stuff ...)})
                      (define stuff*
                        (parse-indentation-multiple-deep (attribute stuff)))
                      (define b*
                        (datum->syntax
                         #'b
                         `(,#'mb ,@stuff*)
                         #'b
                         #'b))
                      (datum->syntax
                       stx
                       `(,#'m ,#'n ,#'l ,b*)
                       stx
                       stx)]))

(require syntax/parse
         "reader/parse-indentation.rkt")

(define (make-no-hash-readtable [rt (current-readtable)])
  (make-readtable rt #\# #\a #f))
