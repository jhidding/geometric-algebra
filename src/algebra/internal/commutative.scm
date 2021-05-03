(library (algebra internal commutative)
         (export add mul term? term->list term-id term-factor term-equiv collect-terms expand)
         (import (rnrs (6))
                 (format format)
                 (utility yasos)
                 (utility pmatch)
                 (utility algorithms)
                 (utility cut)
                 (algebra rings))

  (define-syntax pred-or
    (syntax-rules ()
      ((_ a ...) (lambda (x) (or (a x) ...)))))
  
  (define-record-type tag)
  
  (define numeric-factor (make-tag))
  
  (define (factor-id f)
    (cond
      ((symbol? f) f)
      ((number? f) numeric-factor)))
  
  (define (term? a)
    (or (symbol? a) (number? a)
        (pmatch a
          ((* . ,factors) (for-all (pred-or symbol? number?) factors))
          (else           #f))))
  
  (define (term->list a)
    (pmatch a
      ((* . ,factors) factors)
      (,x             `(,x))))
  
  (define (symbol<? a b)
    (string<? (symbol->string a) (symbol->string b)))
  
  (define (term-id a)
    (list-sort symbol<? (filter symbol? (term->list a))))
  
  (define (term-factor a)
    (apply * (filter number? (term->list a))))
  
  (define (term-equiv a b)
    (equal? (term-id a) (term-id b)))
  
  (define (collect-terms lst)
    (let ((g (group-by term-id equal? lst)))
      (map (lambda (a)
             (mul (apply + (map term-factor (cdr a)))
                  `(* ,@(car a)))) g)))
  
  (define (expand a)
    (pmatch a
      ((+ . ,k) (sum (collect-terms k)))
      ((* . ,k) (product (cons (term-factor a) (term-id a))))
      (else     a)))

  (define (mul a b)
    (pmatch (list a b)
      ((0        ,_)       0)
      ((,_        0)       0)
      ((1        ,_)       b)
      ((,_        1)       a)
      (((+ . ,k) ,_)       (sum (map (cut mul <> b) k)))
      ((,_       (+ . ,l)) (sum (map (cut mul a <>) l)))
      (((* . ,k) (* . ,l)) `(* ,@k ,@l))
      (((* . ,k) ,_)       `(* ,@k ,b))
      ((,_       (* . ,l)) `(* ,a  ,@l))
      ((,_       ,_)       `(* ,a  ,b))))
  
  (define (add a b)
    (pmatch (list a b)
      ((0        ,_)       b)
      ((,_       0)        a)
      (((+ . ,k) (+ . ,l)) `(+ ,@k ,@l))
      (((+ . ,k) ,_)       `(+ ,@k ,b))
      ((,_       (+ . ,l)) `(+ ,a  ,@l))
      ((,_       ,_)       `(+ ,a  ,b))))
  
  (define (sum lst)
    (if (null? lst)
      0
      (fold-left add (car lst) (cdr lst))))
  
  (define (product lst)
    (if (null? lst)
      1
      (fold-left mul (car lst) (cdr lst)))) 
)
