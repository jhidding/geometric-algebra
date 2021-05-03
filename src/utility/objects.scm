(library (utility objects)
         (export has-class instance? make-instance instance-vtable instance-data)
         (import (rnrs (6)))

  (define-record-type instance
    (fields vtable data))

  (define (has-class pred obj)
    (and (instance? obj) (pred (instance-data obj))))
)
