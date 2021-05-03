(import (rnrs (6))
        (algebra geometric)
        (algebra rings)
        (format format)
        (utility receive)
        (testing assertions))

(define :m: monomial)
(define (polynomial-expr p)
  (map (lambda (m)
         (cons* (coefficient m) (basis m)))
       p))

(define (test-basis-wedge)
  (receive (sign x) (basis-wedge-1 'e1 '(e2))
    (assert-equal (cons* sign x) '(1 e1 e2)))
  (receive (sign x) (basis-wedge-1 'e1 '(e1 e2))
    (assert-equal (cons* sign x) '(1 e2)))
  (receive (sign x) (basis-wedge-1 'e1 '(e2 e1))
    (assert-equal (cons* sign x) '(-1 e2)))

  (receive (sign x) (basis-wedge '(e1 e2) '(e2 e3))
    (assert-equal (cons* sign x) '(1 e1 e3)))
  (receive (sign x) (basis-wedge '(e1) '(e1))
    (assert-equal (cons* sign x) '(1)))
  (receive (sign x) (basis-wedge '(e1 e2) '(e1 e2))
    (assert-equal (cons* sign x) '(-1)))
  (receive (sign x) (basis-wedge '(e1 e2) '(e1 e3))
    (assert-equal (cons* sign x) '(-1 e2 e3))))


(define (test-collect-terms)
  (assert-equal (polynomial-expr
                  (collect-terms (list (:m: 1 '(e1)) (:m: 1 '(e1)))))
                '((2 e1)))
  (assert-equal (polynomial-expr
                  (collect-terms (list (:m: 1 '(e1)) (:m: 2 '(e2)))))
                '((1 e1) (2 e2)))
  (assert-equal (polynomial-expr
                  (collect-terms (list (:m: 1 '(e1 e2)) (:m: 2 '(e2 e1)))))
                '((-1 e1 e2)))
  (assert-equal (polynomial-expr
                  (collect-terms (list (:m: 1 '()))))
                  '((1))))

(define (test-polynomials)
  (let* ((p (polynomial (:m: (symbolic 'a-x) '(e1)) (:m: (symbolic 'a-y) '(e2))))
         (q (polynomial (:m: (symbolic 'b-x) '(e1)) (:m: (symbolic 'b-y) '(e2))))
         (r (:* p q)))
    (println "{}" (repr r)) (newline)))

(define (test-vector-sizes)
  (assert-equal (vector-sizes 2) '(1 2 1))
  (assert-equal (vector-sizes 3) '(1 3 3 1))
  (assert-equal (vector-sizes 4) '(1 4 6 4 1)))
