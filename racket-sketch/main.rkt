#lang racket
(provide (except-out (all-from-out racket)
                     #%module-begin
                     #%app
                     #%top
                     #%datum)
         (rename-out [module-begin #%module-begin]
                     [app #%app]
                     [top #%top]
                     [datum #%datum])
         define/img)

(require racket/snip)

(define-syntax-rule (module-begin expr ...)
  (#%module-begin
   (displayln "Entering Module Verbose")
   expr ...
   (displayln "Leaving Module Verbose")))

(define-syntax-rule (app f arg ...)
  (begin (display "Applying: ")
         (displayln '(f arg ...))
         (let ([res (#%app f arg ...)])
           (display " res: ")
           (displayln res)
           res)))

(define-syntax-rule (top . arg)
  (begin (display "Not found ")
         (displayln 'arg)
         'arg))

(define (maybe-saving-value arg)
  (cond ([is-a? arg image-snip%]
         (display #'arg))
        (else
         #f)))

(define-syntax-rule (datum . arg)
  (begin (display "Value: ")
         ;(displayln 'arg)
         (maybe-saving-value 'arg)
         #;(#%datum . arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (syntax-case stx ()
    [(_ (x . [px]) img e)
     (let ((coord (map syntax-e (syntax-e #'px))))
       #`(λ (x)
           (point-at x img #,(car coord) #,(cadr coord))
           (let ([x x]) e)))]))


(define-syntax (define/img stx)
  (syntax-case stx ()
    [(_ (name param ...) img body ...)
     #'(define name (λ/point (param ...)
                             img
                             body ...))]))
