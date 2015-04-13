#lang racket
(require (planet aml/rosetta))
(backend tikz)


(define (ramo p topo)
  (line p topo))
(define (folha topo)
  (circle topo 0.2))

(define (arvore p c a min-a max-a min-f max-f)
  (let ((topo (+pol p c a)))
    (ramo p topo)
    (if (< c 2)
        (folha topo)
        (begin
          (arvore topo
                  (* c (random-range min-f max-f))
                  (+ a (random-range min-a max-a))
                  min-a max-a min-f max-f)
          (arvore topo
                  (* c (random-range min-f max-f))
                  (- a (random-range min-a max-a))
                  min-a max-a min-f max-f)))))

(arvore (xy 0 1) 20 (/ pi 2) (/ pi 16) (/ pi 4) 0.6 0.9)

(define (generateTikz [file-name "tmp"] [scale 1] [pdf-viewer "evince"])
  (define (tikz->tex str out)
    (let ((scale (* scale 0.024)))
      (fprintf out
               "\\documentclass[]{article}\n\\usepackage{tikz}\n\\begin{document}\n\\begin{tikzpicture}[yscale=~a,xscale=~a]\n~a\n\\end{tikzpicture}\n\\end{document}"
               scale scale str)))
  (define (display-tikz-to-string)
    (let ([output-port (open-output-string)])
      (parameterize ([current-output-port output-port])
        (display-tikz))
      (get-output-string output-port)))
  
  (define out
    (open-output-file (string-append file-name ".tex") #:exists 'replace))
  
  (tikz->tex (display-tikz-to-string) out)
  (close-output-port out)
  (system (string-append "pdflatex " file-name ".tex"))
  (process (string-append pdf-viewer " " file-name ".pdf"))
  (system "rm *.tex *.log *.aux -f"))

(generateTikz)