(library (utility gen-id)
         (export gen-id)
         (import (rnrs (6)))
 
  (define gen-id
    (lambda (template-id . args)
      (datum->syntax template-id
        (string->symbol
          (apply string-append
                 (map (lambda (x)
                        (if (string? x)
                          x
                          (symbol->string (syntax->datum x))))
                      args))))))
)
