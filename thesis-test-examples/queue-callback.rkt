#lang racket/gui

(define (slow-thing)
  (printf "slow-thing\n"))

(define timer #f)

(define (button-callback)
  (unless timer
    (set! timer
          (new timer%
               [notify-callback
                (λ ()
                  (set! timer #f)
                  (slow-thing))]
               [interval 1]
               [just-once? #t]))))

(define f (new frame% [label ""]))

(define b (new button%
               [parent f]
               [label "button"]
               ;              [callback (λ (x y) (queue-callback (λ () (slow-thing)) #t))]))
               [callback (λ (x y) (button-callback))]))

(send f show #t)


(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))


(define n 17003)

(define t (current-inexact-milliseconds))
(let ()
  (fact n)
  (void))
(displayln (- (current-inexact-milliseconds) t))
