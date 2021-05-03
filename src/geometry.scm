(library (geometry)
         (export make-g2-monovector g2-monovector? g2-monovector-grade g2-multivector)

         (import (rnrs (6))
                 (format format)
                 (utility gen-id)
                 (utility receive)
                 (utility cut)
                 (utility algorithms)
                 (utility permutations)
                 (geometric-algebra rings))

  (define-syntax define-geometric-algebra
    (lambda (x)
      (syntax-case x ()
        ((_ name (elems ...))
         (with-syntax ((newtype-monovector (gen-id #'name #'name "-monovector"))
                       (newtype-multivector (gen-id #'name #'name "-multivector")))
           #'(begin
               (define-record-type newtype-monovector
                 (fields grade data))

               (define-record-type newtype-multivector
                 (fields data))
           ))))))

  (define-geometric-algebra g2 (e1 e2))
)
