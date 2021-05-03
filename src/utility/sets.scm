(library (utility sets)
         (export set? make-set set-insert! set-delete! set-contains? set->vector set-empty?
                 symbol-set int-set)
         (import (rnrs (6)))

  (define-record-type set
    (fields table)
    (protocol
      (lambda (new)
        (lambda (hash equiv?)
          (new (make-hashtable hash equiv?))))))

  (define (set-insert! s v)
    (hashtable-set! (set-table s) v '()))

  (define (set-delete! s v)
    (hashtable-delete! (set-table s) v))

  (define (set-contains? s v)
    (hashtable-contains? (set-table s) v))
  
  (define (set->vector s)
    (hashtable-keys (set-table s)))

  (define (set-empty? s)
    (zero? (hashtable-size (set-table s))))

  (define (symbol-set . elems)
    (let ((s (make-set symbol-hash symbol=?)))
      (for-each (lambda (e) (set-insert! s e)) elems)
      s))

  (define (int-set . elems)
    (let ((s (make-set (lambda (x) x) eq?)))
      (for-each (lambda (e) (set-insert! s e)) elems)
      s))
)
      
