(import (rnrs (6))
        (format format)
        (algebra rings)
        (algebra commutative)
        (algebra geometric)
        (utility cut)
        (utility pmatch))

(define-syntax :m:
  (syntax-rules ()
    ((_ a b ...) (monomial (expression (quote a)) (list (quote b) ...)))))

(let ((a (polynomial
           (:m: a-x e1) (:m: a-y e2) (:m: a-z e3)))
      (b (polynomial
           (:m: b-x e1) (:m: b-y e2) (:m: b-z e3))))
  (println "{} . {} = {}" (repr a) (repr b) (repr (dot-product a b))))
       
