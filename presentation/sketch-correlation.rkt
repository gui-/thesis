#lang racket/base
(require (for-syntax racket/base))
(provide define/img)

(define-syntax (point-at stx)
  (syntax-case stx ()
    [(_ from to)
     (let (#;(px (syntax-e #'at-px))
           #;(py (syntax-e #'at-py))
           (from-len (string-length 
                      (symbol->string (syntax-e #'from)))) 
           #;(to-len (string-length 
                    (syntax-e #'to))))
       (syntax-property
        #'(void)
        'sub-range-binders
        (vector (syntax-local-introduce #'from)
                0 from-len 0.5 0.5
                (syntax-local-introduce #'to)
                0 1 0.5 0.5)))]))

(define-syntax (λ/point stx)
  (syntax-case stx ()
    [(_ (x) img e)
     #'(λ (x)
         (point-at x img)
         (let ([x x]) e))]
    [(_ (x y) img e)
     #'(λ (x y)
         (point-at x img)
         (point-at y img)
         (let ([x x]
               [y y]) e))]
    [(_ (x y z) img e)
     #'(λ (x y z)
         (point-at x img)
         (point-at y img)
         (point-at z img)
         (let ([x x]
               [y y]
               [z z]) e))]))

(define-syntax (define/img stx)
  (syntax-case stx ()
    [(_ (name param ...) img body ...)
     #'(define name (λ/point (param ...)
                             img
                             body ...))]))
