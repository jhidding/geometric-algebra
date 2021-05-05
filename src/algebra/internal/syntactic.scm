(library (algebra internal syntactic)
         (export gen-mul multivector? make-multivector multivector-data multivector-grade)
         (import (rnrs (6))
                 (utility cut)
                 (utility algorithms)
                 (format format)
                 (algebra rings)
                 (algebra geometric))

  (define-record-type multivector (fields grade data)
    (protocol
      (lambda (new)
        (case-lambda
          ((d)   (new 'multi d))
          ((g d) (new g d))))))
  
  (define (blade? v)
    (and (multivector? v) (number? (multivector-grade v))))
  
  (define blade-grade multivector-grade)

  (define (make-sym first . args)
    (let ((str (fold-left (cut format "{}{}" <> <>) first args)))
      (string->symbol str)))
  
  (define (get-blade-size g k)
    (if (eq? k 'multi)
      (expt 2 (g-info-dim g))
      (vector-ref (g-info-sizes g) k)))
  
  (define (get-blade-offset g k)
    (if (eq? k 'multi)
      0
      (vector-ref (g-info-offsets g) k)))
  
  (define (gen-basis g grade)
    (if (eq? grade 'multi)
      (full-basis g)
      (blade-basis g grade)))
  
  (define (gen-blade-mul op g a k b l)
    (let* ((av (symbolic-vector g k a))
           (bv (symbolic-vector g l b))
           (cv (op av bv)))
      `(let (,@(map (lambda (i)
                      `(,(make-sym 'a (+ i (get-blade-offset g k)))
                        (vector-ref (multivector-data ,a) ,i)))
                    (range (get-blade-size g k)))
             ,@(map (lambda (i)
                      `(,(make-sym 'b (+ i (get-blade-offset g l)))
                        (vector-ref (multivector-data ,b) ,i)))
                    (range (get-blade-size g l))))
         (make-multivector
            ,(if (eq? (grade cv) 'multi) '(quote multi) (grade cv))
            (vector ,@(map (lambda (b) (unbox (project cv b)))
                           (gen-basis g (grade cv))))))))
  
  (define (gen-mul op dim)
    (let ((g (make-g-info dim)))
      `(lambda (a b)
         (case (multivector-grade a)
           ,@(map (lambda (i)
               `((,i) (case (multivector-grade b)
                        ,@(map (lambda (j)
                            `((,j) ,(gen-blade-mul op g 'a i 'b j)))
                            (cons 'multi (range (+ 1 dim)))))))
               (cons 'multi (range (+ 1 dim))))))))
)
