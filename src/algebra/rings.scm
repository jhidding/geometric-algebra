(library (algebra rings)
         (export ring? unbox like :zero? :one? :+: :*: :-: :+ :* :- negate symbolic repr)
         (import ;(rename (rnrs (6)) (+ rnrs:+) (- rnrs:-) (* rnrs:*))
                 (rnrs (6))
                 (format format)
                 (utility cut)
                 (utility pmatch)
                 (utility yasos))

  (define-predicate ring?)
  (define-operation (repr self) (format "{}" self))
  (define-operation (unbox self) self)
  (define-operation (like self other) other)
  (define-operation (:zero? self) (zero? self))
  (define-operation (:one? self) (eq? 1 self))
  (define-operation (negate self) (- self))
  (define-operation (:+: self other) (+ self other))
  (define-operation (:-: self other) (- self other))
  (define-operation (:*: self other) (* self other))

  (define (symbolic expr)
    (object
      ((ring? self) #t)
      ((repr self) (format "{}" expr))
      ((unbox self) expr)
      ((like self other) (symbolic other))
      ((:zero? self) (eq? 0 expr))
      ((:one? self) (eq? 1 expr))
      ((:+: self other) (cond
                          ((:zero? self) other)
                          ((:zero? other) self)
                          (else (symbolic (list '+ expr (unbox other))))))
      ((:*: self other) (cond
                          ((:one? self) other)
                          ((:one? other) self)
                          (else (symbolic (list '* expr (unbox other))))))
      ((negate self)    (symbolic (list '- expr)))
      ((:-: self other) (cond
                          ((:zero? self) (symbolic (list '- (unbox other))))
                          ((:zero? other) self)
                          (else (symbolic (list '- expr (unbox other))))))))

  (define (:+ first . rest)
    (fold-left :+: first rest))

  (define (:* first . rest)
    (fold-left :*: first rest))

  (define :-
    (case-lambda
      ((x) (negate x))
      ((a b) (:-: a b))))
)
