#lang racket/base
(require drracket/tool
         racket/runtime-path
         racket/class
         racket/gui/base
         racket/unit
         mrlib/switchable-button
         "online-comp.rkt")

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define auto-run-mixin
      (mixin ((class->interface text%)) ()
 
        (inherit begin-edit-sequence
                 end-edit-sequence
                 insert
                 get-text)
        
        (define/augment (on-insert start len)
          (begin-edit-sequence))
        
        (define/augment (after-insert start len)
          ;(display "after-insert: ")
          ;(displayln (get-text 0 (+ start len)))
          (end-edit-sequence))
 
        (define/augment (on-delete start len)
          (begin-edit-sequence))
        
        (define/augment (after-delete start len)
          (end-edit-sequence))
 
        (define/private (check-range start stop)
          (void))
 
        (super-new)))
    
    (define auto-run-frame-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-definitions-text)
        (inherit register-toolbar-button)
        
        (define btn
          (new switchable-button%
               (label "Auto Run")
               (bitmap (create-bitmap auto-run))
               (callback (λ (button)
                           (set-run!)
                           (set-field! bitmap btn 
                                       (create-bitmap auto-run))))
               (parent (get-button-panel))
               [min-width-includes-label? #t]
               [enabled #t] ;see how can I access this field
               ))
        (register-toolbar-button btn #:number 11)
        (send (get-button-panel) change-children
              (λ (l)
                (cons btn (remq btn l))))))
 
    (define (create-bitmap run?)
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp))
             (dark-green (make-object color% 0 100 0))
             (lawn-green (make-object color% 124 252 0)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-brush (if run? dark-green lawn-green) 'opaque)
        (send bdc draw-polygon (list (make-object point% 0 0)
                                     (make-object point% 0 16)
                                     (make-object point% 16 8)))
        (send bdc set-bitmap #f)
        bmp))
    
 
    (define (phase1) (void))
    (define (phase2) (void))
    
    (drracket:get/extend:extend-definitions-text auto-run-mixin)
    (drracket:get/extend:extend-unit-frame auto-run-frame-mixin)
    
     #|(drracket:module-language-tools:add-online-expansion-monitor
     online-comp.rkt
     'monitor
     (λ (defs-text val)
      (define tab (send defs-text get-tab))
       (define current-replay-state (send tab get-replay-state))
       (define drr-frame (send (send defs-text get-tab) get-frame))
       (cond
         [(not current-replay-state)
          (displayln "o.k.")]
         [else
          (displayln "not o.k.")]))
(display defs-text)))|#
    
    (drracket:module-language-tools:add-online-expansion-handler
     online-comp.rkt
     'go
     (λ (defs-text start?)
       (when auto-run
         (begin
           (define drr-frame (send (send defs-text get-tab) get-frame))
           (send drr-frame execute-callback)
           (send drr-frame ensure-rep-hidden)))))
    ))

(define-runtime-path online-comp.rkt "online-comp.rkt")