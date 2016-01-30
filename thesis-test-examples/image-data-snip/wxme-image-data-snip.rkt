#lang racket
(require
  (prefix-in - racket/base)
  racket/class
  wxme)

(provide reader)

(define image-data-reader%
  (class* object% (snip-reader<%>)
    (define/public (read-header version stream) (void))
    (define/public (read-snip text-only? version stream)
      (match-define (list md w h)
        (-read (open-input-bytes (send stream read-raw-bytes
                                       "image-data-snip"))))
      (define img-bytes (send stream read-raw-bytes "image-data-snip"))
      (if text-only?
          #"."
          (new readable-image-data-snip% [args-prop (sort (hash->list
                                                                md) symbol<?
                                                                    #:key car)])))
    (super-new)))

(define readable-image-data-snip%
  (class* object% (readable<%>)
    (init-field [args-prop #f])
    (define/public (read-special source line column position)
      (syntax-property
       (syntax-property
        (datum->syntax #'here
                       'void
                       (vector source line column position 1))
        'args args-prop)
       'original-for-check-syntax #t))
    (super-new)))

(define reader (new image-data-reader%))