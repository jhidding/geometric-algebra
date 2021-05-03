(library (utility algorithms)
  (export append-reverse append-map string-join unfold range iterate-n find-index
          split-at group-by combinations)
  (import (rnrs (6))
          (utility receive))

  (define (append-reverse rev-head tail)
    (if (null? rev-head)
        tail
        (append-reverse
         (cdr rev-head)
         (cons (car rev-head) tail))))

  (define (append-map f . args)
    (apply append (apply map f args)))

  (define (iterate-n f x n)
    (if (zero? n)
      x
      (iterate-n f (f x) (- n 1))))

  (define (range n)
    (let loop ((n n)
               (result '()))
      (if (zero? n)
          result
          (loop (- n 1) (cons (- n 1) result)))))

  (define (find-index elem lst)
    (let loop ((lst lst)
               (count 0))
      (cond
        ((null? lst)          #f)
        ((eq? elem (car lst)) count)
        (else                 (loop (cdr lst) (+ count 1))))))

  (define unfold
    (case-lambda
      ((p f g seed) (unfold p f g seed (lambda (x) '())))
      ((p f g seed tail-gen)
       (do ((x seed (g x))
            (result '() (cons (f x) result)))
           ((p x) (cons (tail-gen x) (reverse result)))))))
  
  (define (string-join sep lst)
    (if (null? lst)
      ""
      (do ((result (car lst) (string-append result sep (car rest)))
           (rest   (cdr lst) (cdr rest)))
          ((null? rest) result))))

  #| Splits a list into a reverse-head and tail portions where the first
   | element of tail is the first element for which pred returns #t.
   |#
  (define (split-at pred lst)
    (let loop ((head '())
               (tail lst))
      (if (or (null? tail) (pred (car tail)))
        (values head tail)
        (loop (cons (car tail) head) (cdr tail)))))

  #| Creates an associative list from a given list and a key function. The
   | argument `key-fn` should be a function mapping elements to their keys,
   | while `equiv` is a function testing the keys for equivalence. This preserves
   | order of the input list, meaning that the expressions:
   | - (unique (map key-fn lst))
   | - (map car (group-by key-fn equiv lst))
   | evaluate to the same result, and elements within the groups appear in the
   | same order as they were in the input list.
   |#
  (define (group-by key-fn equiv lst)
    (let ((update-bin (lambda (elem alst)
                        (receive (head tail) (split-at (lambda (p) (equiv (car p) (key-fn elem))) alst)
                          (if (null? tail)
                            (append-reverse head (list (list (key-fn elem) elem)))
                            (append-reverse head (cons (cons* (caar tail) elem (cdar tail)) (cdr tail)))))))
          (reverse-alist (lambda (alst)
                           (map (lambda (a) (cons (car a) (reverse (cdr a)))) alst))))
      (let loop ((lst lst)
                 (result '()))
        (if (null? lst)
          (reverse-alist result)
          (loop (cdr lst) (update-bin (car lst) result))))))

  (define (combinations m lst)
    (cond
      ((= m 0)     '(()))
      ((null? lst) '())
      (else (append
              (map (lambda (y) (cons (car lst) y))
                   (combinations (- m 1) (cdr lst)))
              (combinations m (cdr lst))))))
)
