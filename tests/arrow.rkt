#lang racket
(require image-data-snip)
(require (planet aml/rosetta))

(define (arrow p ρ α σ β)
  "The function @arrow draws  parameter @p defines the orin point where the arrow will be drawn
   @ρ defines the flf
   @α
   @σ
   @β"
  (line p
        (+pol p ρ α)
        (+pol (+pol p ρ α) σ (+ α pi (- β)))
        (+pol (+pol p ρ α) σ (+ α pi β))
        (+pol p ρ α)))