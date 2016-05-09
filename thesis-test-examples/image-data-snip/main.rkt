#lang racket/gui
(require "sketch-correlation.rkt"
         (prefix-in - racket/base))

(provide (rename-out [image-data-snip-class snip-class]
                     [define/img define])
         image-data-snip%)

(define image-data-snip%
  (class* image-snip% (readable-snip<%>)
    (inherit set-snipclass
             get-flags set-flags
             get-bitmap)
    
    (define metadata (make-hash))
    
    (define/public (set-metadata md) (set! metadata md))
    
    (define frame #f)
    (define arg-field #f)
    (define x-field #f)
    (define y-field #f)
    (define h-orig #f)
    
    (define (setup-main-frame at-x at-y)
      (set! frame (new frame%
                       [label ""]
                       #;[width 256]
                       #;[height 20]
                       [x at-x]
                       [y at-y]
                       [style '(toolbar-button)]
                       [alignment '(right bottom)])))
    
    (define (setup-arg-field)    
      (set! arg-field (new text-field%	 
                           [label "argument:"]	 
                           [parent frame]	 
                           [callback (lambda(t e)
                                       (when (eq? 'text-field-enter (send e get-event-type))
                                         (let ((arg (send arg-field get-value))
                                               (x (send x-field get-value))
                                               (y (send y-field get-value)))
                                           (tokenize (list arg x y))
                                           (send frame show #f))))])))
    
    (define (setup-x-field init-value)
      (set! x-field (new text-field%	 
                         [label "x [0, 1]:"]	 
                         [parent frame]	 
                         [callback (lambda(t e)
                                     (when (eq? 'text-field-enter (send e get-event-type))
                                       (let ((arg (send arg-field get-value))
                                             (x (send x-field get-value))
                                             (y (send y-field get-value)))
                                         (tokenize (list arg x y))
                                         (send frame show #f))))]
                         [init-value init-value])))
    
    (define (setup-y-field init-value)
      (set! y-field (new text-field%	 
                         [label "y [0, 1]:"]	 
                         [parent frame]	 
                         [callback (lambda(t e)
                                     (when (eq? 'text-field-enter (send e get-event-type))
                                       (let ((arg (send arg-field get-value))
                                             (x (send x-field get-value))
                                             (y (send y-field get-value)))
                                         (tokenize (list arg x y))
                                         (send frame show #f))))]
                         [init-value init-value])))
    
    (define (setup-btn)
      (new button%
           [label "Ok"]
           [parent frame]
           [callback (lambda(t e)
                       (when (eq? 'button (send e get-event-type))
                         (let ((arg (send arg-field get-value))
                               (x (send x-field get-value))
                               (y (send y-field get-value)))
                           (tokenize (list arg x y))
                           (send frame show #f))))]))
    
    (define/public (read-special src line col pos)
      (define cpy (send this copy))
      (define bmp (get-bitmap))
      (send cpy set-bitmap bmp)
      (send cpy set-metadata metadata)
      (syntax-property (datum->syntax #f
                                      cpy
                                      (vector src line col pos 1)
                                      #f) 'args (send cpy get-args-pos)))
    (super-new)
    (set-snipclass image-data-snip-class)
    (send (get-the-snip-class-list) add image-data-snip-class)
    (set-flags (cons 'handles-events (get-flags)))
    
    (define/override (copy)
      (define ids (new image-data-snip%))
      (send ids set-bitmap (get-bitmap))
      (send ids set-metadata metadata)
      ids)
    
    (define/override (write f)
      (define bmp (get-bitmap))
      (define w (send bmp get-width))
      (define h (send bmp get-height) )
      (define p (open-output-bytes))
      (-write (list metadata w h) p)
      (define b (get-output-bytes p))
      (define ib (make-bytes (* 4 w h)))
      (send bmp get-argb-pixels 0 0 w h ib)
      (send f put (bytes-length b) b)
      (send f put (bytes-length ib) ib))
    
    (define/override (get-text offset num [flattened? #f])
      "my-snip")
    
    (define/private (tokenize input)
      ;pattern '("sym" "num" "num")
      (define sym (string->symbol (car input)))
      (define px (string->number (car (cdr input))))
      (define py (string->number (car (cdr (cdr input)))))
      (send this set-arg-pos sym (list px py)))    
    
    (inherit get-admin resize)
    (define/override (on-event dc x y editorx editory e)
      (let* ((admin (get-admin))
             (x-display (send e get-x))
             (y-display (send e get-y))
             (x-coord (- x-display x))
             (y-coord (- y-display y))
             (btm (get-bitmap))
             (btm-w (send btm get-width))
             (btm-h (send btm get-height))
             (px (real->decimal-string (/ x-coord btm-w) 2))
             (py (real->decimal-string (/ y-coord btm-h) 2)))
        (when (and (send e button-down? 'left)
                   (send e get-control-down))
          (when admin         
            (setup-main-frame (+ x-display (inexact->exact editorx))
                              (+ y-display (inexact->exact editory)))
            (setup-arg-field)
            (setup-x-field px)
            (setup-y-field py)
            (setup-btn)
            (unless (send frame is-shown?)
              (send frame show #t))))
        (when (and (send e button-down? 'left)
                   (send e get-shift-down))
          (when admin
            (set! h-orig btm-h)
            (resize btm-w 10)))
        (when (and (send e button-down? 'right)
                   (send e get-shift-down))
          (when admin
            (resize btm-w h-orig)))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;Operations on the metadata ;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/public (get-arg-pos arg)
      (hash-ref metadata arg '(0.5 0.5)))
    
    (define/public (set-arg-pos arg pos)
      (define cpy (hash-copy metadata))
      (hash-set! cpy arg pos)
      (set! metadata cpy))
    
    (define/public (get-args-pos)
      (sort (hash->list
             metadata) symbol<?
                       #:key car))))

(define image-data-snip-class%
  (class snip-class%
    (inherit set-classname)
    
    (super-new)
    (set-classname (format "~s" '((lib "main.rkt" "image-data-snip")
                                  (lib "wxme-image-data-snip.rkt" "image-data-snip"))))
    
    (define/override (read f)
      (match-define (list md w h)
        (-read (open-input-bytes (send f get-unterminated-bytes))))
      (define img-bytes (send f get-unterminated-bytes))
      (define bmp (make-bitmap w h))
      (send bmp set-argb-pixels 0 0 w h img-bytes)
      (define ids (new image-data-snip%))
      (send ids set-metadata md)
      (send ids set-bitmap bmp)
      ids)))

(define image-data-snip-class (new image-data-snip-class%))