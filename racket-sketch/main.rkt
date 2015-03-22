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
         define*)

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

(define-syntax-rule (datum . arg)
  (begin (display "Value: ")
         (displayln 'arg)
         (#%datum . arg)))

(define-syntax (define* stx)
  (syntax-case stx ()
    [(_ (name param ...) body ...)
     (let ([f syntax-e])
       (display "id: ")
       (displayln (f #'name))
       (display "args: ")
       (displayln (map f (syntax->list 
                          #'(param ...))))
       (display "body: ")
       (displayln (map f (syntax->list 
                          #'(body ...))))
       #'(define (name param ...) body ...))]))