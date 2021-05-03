(library (utility permutations)
         (export cycle-notation permutation-vector permutation-parity permutation-even? permutation-odd?)
         (import (rnrs (6))
                 (utility algorithms)
                 (utility sets)
                 (utility cut))

  #| Compute the cycle notation for a permutation vector. O(n)
   |#
  (define (cycle-notation perm-vec)
    (if (zero? (vector-length perm-vec)) '()
      (let ((not-visited (apply int-set (vector->list perm-vec))))
        (let loop ((result '(()))
                   (ptr    0))
          (cond
            ((set-contains? not-visited ptr)
             (begin (set-delete! not-visited ptr)
                    (loop (cons (cons ptr (car result)) (cdr result))
                          (vector-ref perm-vec ptr))))
            ((set-empty? not-visited) (reverse (map reverse result)))
            (else   (loop (cons '() result)
                          (vector-ref (set->vector not-visited) 0))))))))

  #| From two arrays that are assumed to contain the same set of elements, compute the
   | permutation vector, that is: the vector containing numbers from 0 .. (n - 1) that
   | encodes the permutation from `a` to `b`. O(n^2)
   |#
  (define (permutation-vector a b)
    (list->vector (map (cut find-index <> b) a)))

  (define (permutation-parity a b)
    (let* ((pv (permutation-vector a b))
           (pc (cycle-notation pv)))
      (mod (- (vector-length pv) (length pc)) 2)))

  (define (permutation-even? a b)
    (zero? (permutation-parity a b)))

  (define (permutation-odd? a b)
    (not (permutation-even? a b)))

)
