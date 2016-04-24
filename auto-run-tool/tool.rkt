#lang racket/base
(require drracket/tool
         racket/runtime-path
         racket/class
         racket/gui/base
         racket/unit
         racket/contract
         racket/string
         mrlib/switchable-button
         image-data-snip
         "online-comp.rkt")

(provide tool@)

(define frame #f)
(define fields-coord-lst #f)


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
         
         ;(define img-edit (new menu% [label "Image Edit"] [parent (send frame get-menu-bar)]))
         ;(append-editor-operation-menu-items img-edit #t)
         ;(displayln (send frame get-menu-bar))
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
                   get-text
                   get-tab
                   get-keymap)
          
          
          (define/augment (after-set-next-settings settings)
            (let ([frame (get-top-level-window)])
              (when frame
                (begin
                  (send frame update-button-visibility/settings settings)
                  (display (send frame get-menu-bar)))))
            (inner (void) after-set-next-settings settings))
          
          (define/private (add-slider x y)
            (define start (send this get-start-position))
            (define end (send this get-end-position))
            (define str (get-text start end))
            (define val (string->number str))
            (define min (- val 10))
            (define max (+ val 10))
            
            (set! frame (new frame%	 
                             [label ""]
                             [width 128]
                             [height 20]
                             [x (- x 70) #;(- x (round (/ x 2)))]
                             [y (- y 10) #;(- y (round (/ y 2)))]
                             [style '(float no-caption)]
                             [alignment '(left bottom)]))
            
            (define slider (new slider%	 
                                [label ""]	 
                                [min-value min]	 
                                [max-value max]	 
                                [parent frame]	 
                                [callback (lambda(b e)
                                            (define drr-frame (send (get-tab) get-frame))
                                            (define num (send slider get-value))
                                            (define num-str (number->string num))
                                            (define end (+ start (string-length str)))
                                            (set! str num-str)
                                            (send this delete start end)
                                            (send this insert num-str start)
                                            (send drr-frame execute-callback))]
                                [init-value val]))
            
            (send frame show #t))
          
          (define/private (add-text-box)
            (define text-frame #f)
            (define btn #f)
            
            (set! frame (new frame%
                             [label ""]
                             [width 256]
                             [height 20]
                             [x 300]
                             [y 300]
                             [style '(toolbar-button)]
                             [alignment '(right bottom)]))
            (set! text-frame (new text-field%	 
                                  [label "fields"]	 
                                  [parent frame]	 
                                  [callback (lambda(t e)
                                              (when (eq? 'text-field-enter (send e get-event-type))
                                                (set! fields-coord-lst (send t get-value))
                                                (send frame show #f)
                                                ))]))
            (set! btn (new button%
                           [label "Ok"]
                           [parent frame]
                           [callback (lambda (b e)
                                       (set! fields-coord-lst (send text-frame get-value))
                                       (send frame show #f))]))
            (send frame show #t))
          
          (define/override (on-new-image-snip filename kind relative-path? inline?)
            (displayln "@on-new-image-snip")
            #;(super on-new-image-snip filename kind relative-path? inline?)
            (make-object image-data-snip% filename kind relative-path? inline?))
          
          (define/override (on-char event)
            (when (and (eq? (send event get-key-code) 'f4) x y)
              (add-slider x y))
            (super on-char event))
          
          (define/override (on-default-event event)
            (when (send event dragging?)
              (set! x (send event get-x))
              (set! y (send event get-y)))
            (when (and (send event button-down? 'left)
                       (send event get-control-down))
              (let* ([x (send event get-x)]
                     [y (send event get-y)]
                     [pos (send this find-position x y)]
                     [snip (send this find-snip pos 'before)])
                (when (is-a? snip image-data-snip%)
                  (begin
                    (add-text-box)
                    (when fields-coord-lst 
                      (define str (string-split fields-coord-lst))
                      (define sym (string->symbol (car str)))
                      (define px (string->number (car (cdr str))))
                      (define py (string->number (car (cdr (cdr str)))))
                      (send snip set-arg-pos sym (list px py)))
                    
                    (displayln fields-coord-lst)))))
            (super on-default-event event))
          
          (define/augment (on-insert start len)
            (begin-edit-sequence))
          
          (define/override (on-focus on?)
            (when (and on? frame)
              (send frame show #f))
            (super on-focus on?))
          
          #;(append-editor-operation-menu-items
             (new menu%
                  [parent (new popup-menu%)]
                  [label "Set image..."]
                  [callback (lambda (a b) (void))]))
          
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
          
          (super-new))))
    
    (define auto-run-frame-mixin
      (mixin (drracket:unit:frame<%>) (autorun-frame<%>)
        
        (inherit get-button-panel
                 get-definitions-text
                 get-current-tab)
        
        #;(define/override (edit-menu:between-redo-and-cut edit-menu)
            (super edit-menu:between-redo-and-cut edit-menu)
            (new menu-item%	 
                 [label "edit image"]	 
                 [parent edit-menu]	 
                 [callback (lambda(i e) (displayln "here"))]))
        
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
