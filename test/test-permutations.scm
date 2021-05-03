(import (rnrs (6))
        (testing assertions)
        (utility permutations))

(define (test-permutation-vector)
  (assert-equal (permutation-vector '(a b c) '(c b a)) '#(2 1 0))
  (assert-equal (permutation-vector '() '()) '#())
  (assert-equal (permutation-vector '(a b c d) '(a b d c)) '#(0 1 3 2)))

(define (test-cycle-notation)
  (assert-equal (cycle-notation '#(2 1 0)) '((0 2) (1)))
  (assert-equal (cycle-notation '#(1 2 0 3 4)) '((0 1 2) (3) (4)))
  (assert-equal (cycle-notation '#()) '())
  (assert-equal (cycle-notation '#(4 3 2 1 0)) '((0 4) (1 3) (2))))

(define (test-parity)
  (assert-true (permutation-even? '() '()))
  (assert-true (permutation-even? '(a b c) '(a b c)))
  (assert-true (permutation-even? '(a b c) '(b c a)))
  (assert-false (permutation-even? '(a b c d) '(b c d a)))
  (assert-false (permutation-even? '(a b c) '(c b a)))
  (assert-true (permutation-even? '(a b c d e) '(b a d c e))))

