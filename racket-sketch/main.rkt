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
         def-img)

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

(define-syntax (def-img stx)
  (syntax-case stx ()
    [(_ (name param ...) img body ...)
     (with-syntax ([(stuff ...)
                    (map (lambda (p)
                           (datum->syntax
                            p
                            (syntax->datum p)
                            #'img
                            p))
                         (syntax->list #'(param ...)))])
       #'(define (name param ...)
           stuff ...
           img
           body ...))]))
