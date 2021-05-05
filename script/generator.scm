(import (rnrs (6))
        (format format)
        (algebra rings)
        (algebra commutative)
        (algebra geometric)
        (utility cut)
        (utility algorithms)
        (utility pmatch)
        (algebra syntactic)
        (algebra internal syntactic)
        (only (chezscheme) pretty-print))

(define (to-multivector a)
  (if (blade? a)
    (let* ((offsets '#(0 1 4 7))
           (sizes   '#(1 3 3 1))
           (grade   (multivector-grade a))
           (result  (make-vector 8 0)))
      (do ((i 0 (+ i 1)))
          ((= i (vector-ref sizes grade)) (make-multivector result))
        (vector-set! result (+ i (vector-ref offsets grade))
                     (vector-ref (multivector-data a) i))))
    a))

(define (multivector-project grade a)
  (let* ((offsets '#(0 1 4 7))
         (sizes   '#(1 3 3 1))
         (result  (make-vector (vector-ref sizes grade) 0)))
    (do ((i 0 (+ i 1)))
        ((= i (vector-ref sizes grade)) (make-multivector grade result))
      (vector-set! result i
                   (vector-ref (multivector-data a)
                               (+ i (vector-ref offsets grade)))))))

(define (add a b)
  (cond
    ((and (blade? a) (blade? b) (= (blade-grade a) (blade-grade b)))
     (make-multivector
       (blade-grade a)
       (vector-map + (multivector-data a) (multivector-data b))))
    (else
     (add (to-multivector a) (to-multivector b)))))

(define mul
  (make-mul 3 *))

(define (scalar s)
  (make-multivector 0 (vector s)))

(define (:- x y z)
  (make-multivector 1 (vector x y z)))

(define (:> x y z)
  (make-multivector 2 (vector x y z)))

(define I (make-multivector 3 '#(1)))

(pretty-print (mul (:- 1 2 3) (:- 4 5 6)))

