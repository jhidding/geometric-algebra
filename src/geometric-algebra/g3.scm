(library (geometric-algebra g3)
         (export)
         (import (rnrs (6))
                 (algebra rings)
                 (geometric-algebra symbolic))

  (define-record-type g3-vector
    (fields grade data))

  (define (g3-vector->multi a)
    (let ((grade (g3-vector-grade a))
          (data  (g3-vector-data a)))
      (if (eq? grade 'multi)
        data
        (let ((out    (make-vector 8 0))
              (offset (vector-ref '#(0 1 4 7) grade)))
          (do ((i 0 (+ i 1)))
              ((>= i (vector-length data)) out)
            (vector-set! out (+ offset i) (vector-ref data i)))))))

  (define (g3-map f a b)
    (let ((g-a (g3-vector-grade a))
          (g-b (g3-vector-grade b)))
      (if (eq? g-a g-b)
        (make-g3-vector
          g-a
          (vector-map f (g3-vector-data a) (g3-vector-data b)))
        (make-g3-vector
          'multi
          (vector-map f (g3-vector->multi a) (g3-vector->multi b))))))

  (define (g3-add a b) (g3-map + a b))
  (define (g3-sub a b) (g3-map - a b))
)
