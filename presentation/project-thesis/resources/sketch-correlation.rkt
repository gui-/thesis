#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide define/img)

(define-syntax (point-at stx)
  (syntax-case stx ()
    [(_ from to at-px at-py)
     (let ((px (syntax-e #'at-px))
           (py (syntax-e #'at-py))
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
                0 1 px py)))]))

(define-syntax (λ/point stx)
  (syntax-parse stx
    #:literals (λ/point)
    [(_ ([arg val1 val2] ...) img body ...)
     #'(λ (arg ...)
         (point-at arg img val1 val2) ...
         (let ((arg arg) ...)
           body ...))]))

(define-syntax (define/img stx)
    (syntax-case stx ()
      [(_ (name param ...) img body ...)
       #`(define name (λ/point #,(map (lambda(x) `(,x 0.5 0.5)) (syntax-e #'(param ...)))
                               img
                               body ...))]))

#;(define-syntax (λ/point stx)
  (syntax-case stx ()
    [(_ (x ...) img e ...)
     
     (displayln #'(x ...))
     
     #;#`(#;displayln λ x
                      #,@(map (λ (e)
                                `(point-at ,e img 1 1))
                              #'x ...)
                      (let (#,@(map (λ (ee)
                                      (displayln ee)
                                      `[,ee ,ee])
                                    #'x ...))
                        e ...))]
    #;[(_ (x y) img e)
       #'(λ (x y)
           (point-at x img)
           (point-at y img)
           (let ([x x]
                 [y y]) e))]
    #;[(_ (x y z) img e)
       #'(λ (x y z)
           (point-at x img)
           (point-at y img)
           (point-at z img)
           (let ([x x]
                 [y y]
                 [z z]) e))]))