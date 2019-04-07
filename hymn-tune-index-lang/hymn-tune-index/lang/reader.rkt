#lang s-exp syntax/module-reader hymn-tune-index
#:wrapper1 (λ (rd)
             (parameterize ([current-readtable (make-no-hash-readtable)])
               (rd)))

(define (make-no-hash-readtable [rt (current-readtable)])
  (make-readtable rt #\# #\a #f))
