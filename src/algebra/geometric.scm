(library (algebra geometric)
         (export monomial? monomial coefficient basis
                 basis-wedge basis-wedge-1 collect-terms
                 polynomial? polynomial project grade
                 dot-product wedge-product
                 g-info? make-g-info g-info-sizes g-info-offsets g-info-dim
                 g-info-basis blade-basis full-basis
                 vector-sizes symbolic-vector)

         (import (rnrs (6))
                 (format format)
                 (utility gen-id)
                 (utility receive)
                 (utility cut)
                 (utility algorithms)
                 (utility permutations)
                 (utility yasos)
                 (algebra rings)
                 (algebra commutative))

  #| Checks if two monomials commute, assuming every symbol only commutes
   | with itself, and anti-commutes with every other symbol. Returns `1` if
   | `a` and `b` commute, `-1` if they anti-commute.
   |#
  (define (commute-sign a b)
    (apply * (map (lambda (x)
                    (if (even? (length (filter (cut eq? x <>) a)))
                      1 -1))
                  b)))
  
  #| Compute a new basis element by wedging in one more orthonormal basis vector.
   | If that basis vector is already present in `a`, it is eliminated.
   | Returns (values sign b), where sign is the sign picked up in this multiplication
   | and `b` is the new basis element.
   |#
  (define (basis-wedge-1 e a)
    (receive (rev-head tail) (split-at (cut eq? e <>) a)
      (if (null? tail)
        (values 1 (cons e a))
        (values (if (odd? (length rev-head)) -1 1)
                (append-reverse rev-head (cdr tail))))))

  #| Compute the product of two basis vectors
   |#
  (define (basis-wedge a b)
    (let loop ((a (reverse a))
               (b b)
               (sign 1))
      (cond
        ((null? a) (values sign b))
        (else      (receive (sign* b*) (basis-wedge-1 (car a) b)
                     (loop (cdr a) b* (* sign sign*)))))))

  (define (dual-basis I a)
    (receive (sign w) (basis-wedge I a)
      (if (= sign -1)
        (if (< (length w) 2)
          (error 'dual-basis "The dual of the given basis vector is negative.")
          (cons* (cadr w) (car w) (cddr w)))
        w)))

  #| Generate a function that computes the product between two monomials.
   |#
  (define (all? pred lst)
    (not (find (lambda (x) (not (pred x))) lst)))

  (define (basis-equiv a b)
    (and (all? (cut memq <> a) b)
         (all? (cut memq <> b) a)))

  #| Collect terms in a polynomial.
   |#
  (define (collect-terms p)
    (filter (lambda (x) (not (:zero? x)))
            (map (lambda (g) (apply :+ (cdr g)))
                 (group-by basis basis-equiv p))))

  (define-predicate polynomial?)
  (define-predicate monomial?)
  (define-operation (basis self))
  (define-operation (coefficient self))
  (define-operation (grade self))
  (define-operation (project self v))

  (define (monomial c e)
    (object
      ((monomial? self) #t)
      ((repr self) (format "[{}]" 
                     (if (null? e)
                       (repr c)
                       (string-join " "
                         (cons (repr c)
                               (map (cut format "{}" <>) e))))))
      ((ring? self) #t)
      ((like self n) (monomial (like c n) '()))
      ((basis self) e)
      ((coefficient self) c)
      ((:one? self) (and (null? e) (eq? c 1)))
      ((:zero? self) (:zero? c))
      ((:+: self other) (let ((sign (if (permutation-even? e (basis other)) :+ :-)))
                          (monomial (sign c (coefficient other)) e)))
      ((:*: self other) (receive (sign w) (basis-wedge e (basis other))
                          (if (= sign -1)
                            (monomial (negate (:* c (coefficient other))) w)
                            (monomial (:* c (coefficient other)) w))))
      ((:-: self other) (:+ self (negate other)))
      ((negate self) (monomial (negate c) e))
      ((grade self) (length e))
      ((project self v) (if (basis-equiv e v)
                          (if (permutation-even? e v) c (negate c))
                          (like c 0)))))

  (define (coerce-monomial a m)
    (let ((b (basis m))
          (x (coefficient m)))
      (if (permutation-even? a b)
        (monomial x a)
        (monomial (:- x) a))))

  (define (polynomial . a)
    (object
      ((polynomial? self) #t)
      ((repr self) (format "{{{}}}" (string-join " " (map repr a))))
      ((ring? self) #t)
      ((unbox self) a)
      ((like self n) (cond
                       ((zero? n) (polynomial))
                       ((null? a) (error 'polynomial "can't create polynomial from null"))
                       (else      (polynomial (like (car a) n)))))
      ((:one? self) (and (= 1 (length a)) (:one? (car a))))
      ((:zero? self) (for-all :zero? a))
      ((:+: self other) (apply polynomial (collect-terms (append a (unbox other)))))
      ((:-: self other) (:+ self (negate other)))
      ((:*: self other) (apply polynomial (collect-terms
                          (append-map (lambda (a-i)
                            (map (cut :* a-i <>) (unbox other))) a))))
      ((negate self) (apply polynomial (map negate a)))
      ((grade self)  (let ((gs (unique = (list-sort < (map grade a)))))
                       (cond
                         ((null? gs)       0)
                         ((null? (cdr gs)) (car gs))
                         (else             'multi))))
      ((project self v) (if (null? a)
                          0
                          (apply :+ (map (cut project <> v) a))))))

  (define (coerce-polynomial as p)
    (let ((get-basis (lambda (m)
                       (let ((a* (memp (cut basis-equiv (basis m) <>) as)))
                         (if (not a*)
                           (error 'coerce-polynomial "element ~s not found in basis ~s" (basis m) as)
                           (car a*))))))
      (map (lambda (m)
             (coerce-monomial (get-basis m) m))
           p)))

  (define (dot-product a b)
    (:* (like a 1/2) (:+ (:* a b) (:* b a))))

  (define (wedge-product a b)
    (:* (like a 1/2) (:- (:* a b) (:* b a))))

  (define (symbolic-basis dim)
    (map (lambda (i)
           (string->symbol (format "e{}" i)))
         (range 1 (+ dim 1))))

  (define (vector-sizes dim)
    (if (zero? dim)
      '(1)
      (let loop ((p (vector-sizes (- dim 1)))
                 (r '(1)))
        (if (null? (cdr p))
          (cons 1 r)
          (loop (cdr p) (cons (+ (car p) (cadr p)) r))))))

  (define (cumsum lst)
    (let loop ((lst lst)
               (res '(0)))
      (if (null? lst)
        (reverse res)
        (let ((n (+ (car res) (car lst))))
          (loop (cdr lst) (cons n res))))))

  (define-record-type g-info
    (fields dim sizes offsets basis)
    (protocol
      (lambda (new)
        (lambda (dim)
          (let* ((sizes   (vector-sizes dim))
                 (offsets (cumsum sizes)))
            (new dim
                 (list->vector sizes)
                 (list->vector offsets)
                 (symbolic-basis dim)))))))

  (define (symbolic-blade g grade sym)
    (let* ((basis  (blade-basis g grade))
           (o      (vector-ref (g-info-offsets g) grade))
           (n      (vector-ref (g-info-sizes g) grade))
           (vars   (map (lambda (i)
                          (expression
                            (string->symbol (format "{}{}" sym i))))
                        (range o (+ o n)))))
      (apply polynomial (map monomial vars basis))))

  (define (blade-basis g grade)
    (let ((dim (g-info-dim g)))
      (if (> grade (/ dim 2))
        (map (cut dual-basis (g-info-basis g) <>)
             (combinations (- dim grade) (g-info-basis g)))
          (combinations grade (g-info-basis g)))))

  (define (full-basis g)
    (append-map (lambda (k)
                  (blade-basis g k))
                (range 0 (+ 1 (g-info-dim g)))))

  (define (symbolic-vector g grade sym)
    (if (eq? grade 'multi)
      (let ((vars (map (lambda (i)
                         (expression
                           (string->symbol (format "{}{}" sym i))))
                       (range 0 (expt 2 (g-info-dim g)))))
            (basis (full-basis g)))
        (apply polynomial (map monomial vars basis)))
      (symbolic-blade g grade sym)))
)

