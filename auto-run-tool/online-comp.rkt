#lang racket/base
(require racket/class
         racket/place
         racket/match
         racket/contract
         (for-syntax racket/base)
         drracket/private/syncheck/traversals
         drracket/private/syncheck/syncheck-intf)

(provide monitor go set-run! auto-run)

(define auto-run #f)
(define (set-run!)
  (set! auto-run (not auto-run)))


(define (monitor send-back path the-source orig-cust)
  (void))

(define-logger online-check-syntax)
(define (go expanded path the-source orig-cust)
  (define c (make-channel))
  ;GF
  (unless (exn? expanded)
    (log-message online-check-syntax-logger 'info  "" (list expanded)))
  (log-message online-check-syntax-logger 'info  "" c)
  ;; wait for everything to actually get sent back to the main place
  (channel-get c))