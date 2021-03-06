(import (rnrs (6))
        (testing assertions)
        (utility pmatch))

(define (test-pmatch)
  (assert-equal
   (pmatch '(1 2 3)
     ((,a . ,b) b))
   '(2 3))

  (assert-equal
    (pmatch '(define (hello) 'world)
      ((define (,name . ,args) . ,body)
       name)
      (else #f))
    'hello)
)
