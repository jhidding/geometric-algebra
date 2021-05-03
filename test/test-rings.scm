(import (rnrs (6))
        (format format)
        (testing assertions)
        (algebra geometric)
        (algebra internal commutative)
        (algebra commutative)
        (algebra rings))

(define (test-default-ring)
  (assert-equal (:+ 1 2 3 4 5 6) 21))

(define (test-like)
  (assert-equal (like 1 2) 2)
  (assert-equal (unbox (:+ (like (symbolic 1) 0) (symbolic 'a))) 'a))

(define (:m: c . e)
  (monomial c e))

(define (test-operations)
  (let ((a (polynomial (:m: 1 'e1) (:m: 2 'e1 'e2)))
        (b (polynomial (:m: 3 'e2) (:m: 4 'e2 'e1))))
    (assert-true (polynomial? (:* a b)))
    (println "{}" (repr (:+ a b)))
    (println "{}" (repr (:* a b)))
    (println "{}" (repr (dot-product a b)))))

(define (test-operations-symbolic)
  (let ((a (polynomial (:m: (expression 'a) 'e1) (:m: (expression 'b) 'e1 'e2)))
        (b (polynomial (:m: (expression 'c) 'e2) (:m: (expression 'd) 'e2 'e1))))
    (assert-true (polynomial? (:* a b)))
    (println "{}" (repr (:+ a b)))
    (println "{}" (repr (dot-product a b)))))

(define (test-symbolic-ring)
  (assert-equal (unbox (:+ (symbolic 1) (symbolic 2))) '(+ 1 2)))

(define (test-term)
  (assert-equal (term-id 'a) (term-id '(* 3 a))))

(define (test-mul)
  (assert-equal (mul '(+ a b) '(+ c d))
                '(+ (* a c) (* a d) (* b c) (* b d)))
  (assert-equal (mul '(* a b) '(* c d))
                '(* a b c d)))

(define (test-add)
  (assert-equal (add '(+ a b) '(+ c d))
                '(+ a b c d))
  (assert-equal (add '(* a 2) '(+ a b))
                '(+ (* a 2) a b)))

(define (test-term-factor)
  (assert-equal (term-factor '(* a 2)) 2)
  (assert-equal (term-factor 'a) 1))

(define (test-term-id)
  (assert-equal (term-id '(* a 2)) '(a))
  (assert-equal (term-id '(* b 2 a)) '(a b))
  (assert-equal (term-id 'a) '(a)))

(define (test-collect-terms)
  (assert-equal (collect-terms '((* 2 a) a)) '((* 3 a))))

(define (test-expression)
  (let ((a (expression 'a))
        (b (expression 'b))
        (half (expression 1/2)))
    (assert-equal (unbox (:+ a a)) '(* 2 a))
    (assert-equal (unbox (:- a a)) 0)
    (assert-equal (unbox (:* half (:+ a a))) 'a)
    (assert-true (:zero? (:- (:* a b) (:* b a))))))

(define-syntax :e
  (syntax-rules ()
    ((_ x e ...) (monomial (expression 'x) '(e ...)))))

(define (test-outer-3)
  (let ((a (polynomial (:e ax e1) (:e ay e2) (:e az e3)))
        (b (polynomial (:e bx e1) (:e by e2) (:e bz e3))))
    (println "{}" (repr (wedge-product a b)))))
