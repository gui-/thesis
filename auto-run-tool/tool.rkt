#lang racket/base
(require drracket/tool
         racket/runtime-path
         racket/class
         racket/gui/base
         racket/unit
         racket/contract/base
         racket/match
         racket/control
         racket/contract
         mrlib/switchable-button
         "online-comp.rkt")

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define (phase1) 
      (drracket:module-language-tools:add-opt-out-toolbar-button
       (λ (frame parent)
         (define btn (new switchable-button%
              (label "Auto Run")
              (bitmap (create-bitmap state))
              (parent parent)
              (callback (λ (button)
                          (send frame autorun:button-callback btn)))))
         btn)
       'auto-run
       #:number 11))
    (define (phase2) (void))
          
    (define autorun-frame<%>
      (interface ()
        autorun:button-callback))
    
    (define (docs-text-mixin %)
      (class %
        (super-new)))
    
    (define auto-run-mixin
      (λ (super%)
        (class* (docs-text-mixin super%) ()
          (field (x #f))
          (field (y #f))
          
          (inherit get-top-level-window
                   begin-edit-sequence
                   end-edit-sequence
                   insert
                   get-text)
          
          (define/augment (after-set-next-settings settings)
            (let ([frame (get-top-level-window)])
              (when frame
                (send frame update-button-visibility/settings settings)))
            (inner (void) after-set-next-settings settings))
        
          (define/private (add-slider x y)
            (define start (send this get-start-position))
            (define end (send this get-end-position))
            (define str (get-text start end))
            (display str)
            (define frame (new frame%	 
                               [label ""]
                               [width 64]
                               [x (- x (round (/ x 2)))]
                               [y (- y (round (/ y 2)))]))
            (define slider (new slider%	 
                                [label ""]	 
                                [min-value 0]	 
                                [max-value 10]	 
                                [parent frame]	 
                                [callback (lambda(b e) 
                                            (define num (send slider 
                                                              get-value))
                                            (send this insert 
                                                  (number->string num) 
                                                  start
                                                  end))]))
            (send frame show #t))
        
          (define/override (on-char event)
            (when (and (eq? (send event get-key-code) 'f4) x y)
              (add-slider x y))
            (super on-char event))
          
          (define/override (on-default-event event)
            (when (send event dragging?)
              (set! x (send event get-x))
              (set! y (send event get-y)))
            (super on-default-event event))
                
          (define/augment (on-insert start len)
            (begin-edit-sequence))
          
          (define/augment (after-insert start len)
            #;(display "after-insert: ")
            #;(displayln (get-text 0 (+ start len)))
            (end-edit-sequence))
          
          (define/augment (on-delete start len)
            (begin-edit-sequence))
          
          (define/augment (after-delete start len)
            (end-edit-sequence))
          
          (define/private (check-range start stop)
            (void))
          
          (super-new))))
    
    (define auto-run-frame-mixin
      (mixin (drracket:unit:frame<%>) (autorun-frame<%>)
        
        (inherit get-button-panel
                 get-definitions-text
                 get-current-tab)

        (define/augment (on-tab-change old-tab new-tab)
          (displayln "tab-changed")
          (update-button-visibility/tab new-tab))

        (define/private (update-button-visibility/tab tab)
          (update-button-visibility/settings (send (send tab get-defs) get-next-settings)))
        
        (inherit sort-toolbar-buttons-panel)
        (define/public (update-button-visibility/settings settings)
          (let* ([lang (drracket:language-configuration:language-settings-language settings)]
                 [visible? (and (not (is-a? lang drracket:module-language:module-language<%>))
                                (send lang capability-value 'auto-run-button))]) 
            (send (get-button-panel) change-children
                  (λ (l)
                    (if visible?
                        (cons auto-run-button (remq auto-run-button l))
                        (remq auto-run-button l)))) 
            (sort-toolbar-buttons-panel)))
        
        (inherit ensure-rep-hidden)
        (define/public (autorun:button-callback btn)
          (set-state!)
          (set-field! bitmap btn 
                      (create-bitmap state)))
          
        (super-new)
        
        (define auto-run-button
          (new switchable-button%
               [label "Auto Run"]
               [bitmap (create-bitmap state)]
               [parent (get-button-panel)]
               [callback (λ (button) (autorun:button-callback auto-run-button))]))
        (inherit register-toolbar-button)
        (register-toolbar-button auto-run-button #:number 11)
        (define/public (autorun:get-button) auto-run-button)
        (update-button-visibility/tab (get-current-tab))))
        
 
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
    
    
    (drracket:get/extend:extend-definitions-text auto-run-mixin)
    (drracket:get/extend:extend-unit-frame auto-run-frame-mixin)
    (drracket:language:register-capability 'auto-run-button (flat-contract boolean?) #f)
    
    #;(drracket:module-language-tools:add-online-expansion-monitor
       online-comp.rkt
       'monitor
       (λ (defs-text val)
         (void)))
    
    (drracket:module-language-tools:add-online-expansion-handler
     online-comp.rkt
     'go
     (λ (defs-text start?)
       (when state
         (begin
           (define drr-frame (send (send defs-text get-tab) get-frame))
           (send drr-frame execute-callback)
           (send drr-frame ensure-rep-hidden)))))
    ))

(define-runtime-path online-comp.rkt "online-comp.rkt")
