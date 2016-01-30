#lang racket
(require (for-syntax racket/base syntax/parse))
(provide (rename-out [define/img define]))

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
;
;(define-syntax (define/img stx)
;  (syntax-case stx ()
;    [(_ name val)
;     #'(define name val)]
;    [(_ (name param ...) img body ...)
;     (if (syntax-property #'img 'args)
;         (begin
;           (printf "list: ~s\n" (syntax-property #'img 'args))
;           (with-syntax ([((arg val1 val2) ...) (syntax-property #'img 'args)])
;             (printf "got the args at compile-time: ~s\n" #'([param val1 val2] ...))
;             #`(define name (λ/point ([param val1 val2] ...)
;                                     img
;                                     body ...))))
;         #'(define (name param ...) img body ...))]))


(define-syntax (define/img stx)
  (syntax-case stx ()
    [(_ name val)
     #'(define name val)]
    [(_ (name param ...) img body ...)
     (let ((sp (syntax-property #'img 'args)))
       (cond [(and sp (null? sp))
              #`(define name (λ/point #,(map (lambda(x) `(,x 0.5 0.5))
                                             (syntax-e
                                              #'(param ...)))
                                      img
                                      body ...))]
             [(and sp (not (null? sp)))
              (with-syntax ([((arg val1 val2) ...) sp])
                (printf "got the args at compile-time: ~s\n" #'([arg val1 val2] ...))
                #;(printf "got the args at compile-time: ~s\n" #`#,(sort (syntax-e
                                                                          #'(([param val1 val2] ...)))
                                                                         symbol<?  #:key car))
                (printf "got the args at compile-time: ~s\n" #`#,(sort (syntax-e #'(param ...))
                                                                       (lambda (x y)
                                                                         (symbol<? (syntax-e x)
                                                                                   (syntax-e y))
                                                                         )))
                (with-syntax ([(sorted-param ...) #`#,(sort (syntax-e #'(param ...))
                                                            (lambda (x y)
                                                              (symbol<? (syntax-e x)
                                                                        (syntax-e y))
                                                              ))])
                  #`(define name (λ/point ([sorted-param val1 val2] ...)
                                          img
                                          body ...))))]
             [else
              #'(define (name param ...) img body ...)]))]))