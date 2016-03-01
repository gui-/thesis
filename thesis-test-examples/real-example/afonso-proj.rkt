#lang racket

#|
==ADVANCED DIGITAL FABRICATION==
=DIGITAL AND PARAMETRIC DESIGN USING RACKET=

Advanced Studies Program
Computation Applied to Architecture, Urban Planning and Design
University of Lisbon - Faculty of Architecture
2013/2014

Afonso Gonçalves
20130528
|#

(require (planet aml/rosetta))
(backend rhino5)

(erase-2d-top)

;;========================================================================| AUXILIARY FUNCTIONS |==

(define (transpose-matrix matrix)          ;calculates the transpose matrix of a given matrix
  (if (null? (car matrix))
      (list)
      (cons (map car matrix)
            (transpose-matrix (map cdr matrix)))))

(define (midpoint p0 p1)                   ;calculates the midpoint between two other points
  (xy (/ (+ (cx p0) (cx p1)) 2.0)
      (/ (+ (cy p0) (cy p1)) 2.0)))

(define (pts-average p0 p1)
  (/c (+c p0 p1) 2))

(define (between-pts p0 p1 a)              ;evaluates a distance between p0 (a=0) and p1 (a=1)
  (xyz (+ (cx p0) (* a (- (cx p1) (cx p0))))
       (+ (cy p0) (* a (- (cy p1) (cy p0))))
       (+ (cz p0) (* a (- (cz p1) (cz p0))))))

(define (normal pt0 pt1 pt2 pt3)           ;Computes the face normal of a quadrangle
  (let ((c (quad-center pt0 pt1 pt2 pt3))
        (n (quad-normal pt0 pt1 pt2 pt3))
        (d (distance pt0 pt2)))
    (cylinder c
              (/ d 40)
              (+c c (*c n (* d 0.5))))))

(define (radians<-degrees angle)           ;converts degrees to radians
  (/ (* 180 angle) pi))

(define (dot-product u v)                  ;computes the dot product (or scalar product) between two vectors
  (+ (* (cx u) (cx v))
     (* (cy u) (cy v))
     (* (cz u) (cz v))))

(define (angle-vectors u v)                ;measures the angle between two vectors
  (radians<-degrees
   (acos
    (/ (dot-product u v)
       (* (sqrt (+ (sqr (cx u)) (sqr (cy u)) (sqr (cz u))))
          (sqrt (+ (sqr (cx v)) (sqr (cy v)) (sqr (cz v)))))))))

(define (ptf p0 p1 f)
  (+c p0 (*c (-c p1 p0) f)))

(define (perpendicular-vector vector)      ;computes a vector perpendicular to a given one
   (xy
    (* -1 (cy vector))
    (cx vector)))

(define (verify-normal pt0 pt1 pt2 pt3)    ;verifies the normals of a quad-iterated surface by drawing a line orientated along the surface normal
  (let ((c (quad-center pt0 pt1 pt2 pt3))
        (n (quad-normal pt0 pt1 pt2 pt3))
        (d (distance pt0 pt2)))
    (line c (+c c (*c n (* d 1.5))))))

(define (vector-ab pta ptb)                ;calculates the vector between two points
  (xyz (- (cx pta) (cx ptb))
       (- (cy pta) (cy ptb))
       (- (cz pta) (cz ptb))))

;;========================================================================| PARAMETRIC SURFACES |==

;;CONIC SPIRAL
(define (conic-spiral p a b c n1 u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (+ (* a (- 1 (/ v 2pi)) (cos (* n1 v)) (+ 1 (cos u))) (* c (cos (* n1 v))))
           (+ (* a (- 1 (/ v 2pi)) (sin (* n1 v)) (+ 1 (cos u))) (* c (sin (* n1 v))))
           (/ (+
               (* b v) (* a (- 1 (/ v 2pi)) (sin u))) 2pi)))
   u0 u1 n
   v0 v1 m))


;;CROSS-CAP
(define (cross-cap p u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* (cos u) (sin (* 2 v)))
           (* (sin u) (sin (* 2 v)))
           (- (* (cos v) (cos v)) (* (sqr (cos u)) (sqr (sin v))))))
   u0 u1 n
   v0 v1 m))


;;DINI
(define (dini p a u0 u1 m v0 v1 n)
  (map-division
   (lambda (u v)
     (+xyz p
           (* (cos u) (sin v))
           (* (sin u) (sin v))
           (+ (cos v) (log (tan (/ v 2.0))) (* a u))))
   u0 u1 m
   v0 v1 n))


;;ELLIPSOID
(define (ellipsoid p a b c u0 u1 m v0 v1 n)
  (map-division
   (lambda (u v)
     (+xyz p
           (* a (cos u) (sin v))
           (* b (sin u) (sin v))
           (* c (cos v))))
   u0 u1 m
   v0 v1 n))


;;ENNEPER'S SURFACE
(define (enneper1 p u0 u1 m v0 v1 n)
  (map-division
   (lambda (u v)
     (+xyz p
           (+ (- u (/ (expt u 3) 3) (* u (sqr v))))
           (+ (- v (/ (expt v 3) 3) (* v (sqr u))))
           (- (sqr u) (sqr v))))
   u0 u1 m
   v0 v1 n))

(define (enneper2 p u0 u1 m v0 v1 n)
  (map-division
   (lambda (u v)
     (+xyz p
           (/ (* u (/ (- 1 (expt u 2)) (+ 3 (expt v 2)))) 3)
           (/ (* (- v) (/ (- 1 (expt v 2)) (+ 3 (expt u 2)))) 3)
           (/ (- (expt u 2) (expt v 2)) 3)))
   u0 u1 m
   v0 v1 n))


;;FIGURE 8 KLEIN BOTTLE
(define (fig8-klein-bottle p a u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* (cos u) (- (+ a (* (sin v) (cos (/ u 2.0)))) (/ (* (sin (* 2 v)) (sin (/ u 2.0))) 2.0)))
           (* (sin u) (- (+ a (* (sin v) (cos (/ u 2.0)))) (/ (* (sin (* 2 v)) (sin (/ u 2.0))) 2.0)))
           (+ (* (sin (/ u 2.0)) (sin v)) (/ (* (cos (/ u 2.0)) (sin (* 2 v))) 2.0))))
     u0 u1 m
     v0 v1 n))


;;MOBIUS STRIP
(define (mobius-strip1 p a u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (- a (* v (sin (/ u 2.0))) (sin u))
           (- a (* v (sin (/ u 2.0))) (cos u))
           (* v (cos (/ u 2.0)))))
     u0 u1 m
     v0 v1 n))

(define (mobius-strip2 p a u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+cyl p
           (+ 1 (* v (cos (/ u 2.0))))
           u
           (* v (sin (/ u 2.0)))))
     u0 u1 m
     v0 v1 n))


;;STEINER'S ROMAN SURFACE
(define (steiner-roman-surface p a u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (/ (* (sqr a) (sqr (cos v)) (sin (* 2 u))) 2.0)
           (/ (* (sqr a) (sin u) (sin (* 2 v))) 2.0)
           (/ (* (sqr a) (cos u) (sin (* 2 v))) 2.0)))
     u0 u1 m
     v0 v1 n))


;;SUPER ELLIPSOID
(define (super-ellipsoid p a b c n1 n2 u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* a (expt (sin v) n1) (* v (expt (cos u) n2)))
           (* b (expt (sin v) n1) (* v (expt (sin u) n2)))
           (* c (expt (sin v) n))))
     u0 u1 m
     v0 v1 n))


;;SUPER TOROID
(define (super-toroid p rx ry rz c a n1 n2 u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* rx (+ c (* a (expt (cos v) n2))) (expt (cos u) n1))
           (* ry (+ c (* a (expt (cos v) n2))) (expt (sin u) n1))
           (* rz a (expt (sin v) n2))))
     u0 u1 m
     v0 v1 n))


;;TORUS
(define (torus-surface p c a u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* (+ c (* a (cos v))) (cos u))
           (* (+ c (* a (cos v))) (sin u))
           (* a (sin v))))
     u0 u1 m
     v0 v1 n))


;;APPLE SURFACE
(define (apple-surface p u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           (* (cos u) (+ 4 (* 3.8 (cos v))))
           (* (sin u) (+ 4 (* 3.8 (cos v))))
           (+ (* (- (+ (cos v) (sin v)) 1) (+ 1 (sin v)) (log (- 1 (* pi (/ v 10.0))))) (* 7.5 (sin v)))))
     u0 u1 m
     v0 v1 n))


;;SIN-U*V
(define (sin-u*v p a u0 u1 n v0 v1 m)
  (map-division
   (lambda (u v)
     (+xyz p
           u
           v
           (* (/ 4.0 10) (* a (sin (* u v))))))
   u0 u1 n
   v0 v1 m))


;;SIN-U*V2
(define (sin-u*v2 p u0 u1 n v0 v1 m a b)
  (map-division
   (lambda (u v)
     (+xyz p
           u
           v
           (+ (* (/ 4.0 10) (* a (sin (+ u v))))
              (exp (/ (abs (- u 1)) 10.0))
              (* b (sin (- v 1))))))
   u0 u1 n
   v0 v1 m))


;;ELLIPTIC PARABOLOID
(define (elliptic-paraboloid p a b u0 u1 n v0 v1 m)
  (map-division
         (lambda (u v)
           (+xyz p
                 u 
                 v
                 (- (+ (/ (sqr u) (sqr a)) (/ (sqr v) (sqr b))))))
         u0 u1 n
         v0 v1 m))


;;HYPERBOLOID
(define (hyperboloid p a b c u0 u1 n v0 v1 m)
  (map-division
         (lambda (u v)
           (+xyz p
                 (* a (+ 5 (cosh v)) (cos u))
                 (* b (+ 5 (cosh v)) (sin u))
                 (* c (sinh v))))
         u0 u1 n
         v0 v1 m))


;;========================================================================| SURFACE PROCESSING |==

(define (iterate-quads f ptss)
  (for/list ((pts0 ptss)
             (pts1 (cdr ptss)))
    (for/list ((p0 pts0)
               (p1 pts1)
               (p2 (cdr pts1))
               (p3 (cdr pts0)))
      (f p0 p1 p2 p3))))

(define (quad-center p0 p1 p2 p3)
  (pts-average
   (pts-average p0 p2)
   (pts-average p1 p3)))

(define (quad-normal p0 p1 p2 p3)
  (poligon-normal (list p0 p1 p2 p3)))

(define (poligon-normal pts)
  (norm-c
   (crossed-products
    (append pts (list (car pts))))))

(define (crossed-products pts)
  (if (null? (cdr pts))
      (xyz 0 0 0)
      (+c (cross-c (car pts) (cadr pts))
          (crossed-products (cdr pts)))))


;;========================================================================| GRID/STRUCTURE GENERATION |==

(define (hex-grid p0 p1 p2 p3 a b)
  (let* ((o (quad-center p0 p1 p2 p3))
         (m01 (pts-average p0 p1))
         (m12 (pts-average p1 p2))
         (m23 (pts-average p2 p3))
         (m30 (pts-average p3 p0))
         (q0 (between-pts p0 m01 b))
         (q1 (between-pts m01 p1 (- 1.0 b)))
         (q2 (between-pts o m12 (- 1.0 a)))
         (q3 (between-pts m23 p2 (- 1.0 b)))
         (q4 (between-pts p3 m23 b))
         (q5 (between-pts m30 o a)))
    (if (= a 0)
        (line q0 q1 q2 q3 q4 q5 q0)
        (begin
          (line q0 q1 q2 q3 q4 q5 q0)
          (line q2 m12)
          (line m30 q5)))))

(define (truss-node p r) (sphere p r))
(define (truss-bar p0 p1 r) (cylinder p0 r p1))

(define (truss-bars ps qs r)
  (if (or (null? ps) (null? qs))
      #t
      (begin
        (truss-bar (car ps) (car qs) r)
        (truss-bars (cdr ps) (cdr qs) r))))

(define (truss-nodes ps r)
  (if (null? ps)
      #t
      (begin
        (truss-node (car ps) r)
        (truss-nodes (cdr ps) r))))

(define (space-truss pts r-nodes r-bars)
  (let ((as (car pts))
        (bs (cadr pts))
        (cs (caddr pts)))
    (truss-nodes as r-nodes)
    (truss-nodes bs r-nodes)
    (truss-bars as cs r-bars)
    (truss-bars bs as r-bars)
    (truss-bars bs cs r-bars)
    (truss-bars bs (cdr as) r-bars)
    (truss-bars bs (cdr cs) r-bars)
    (truss-bars (cdr as) as r-bars)
    (truss-bars (cdr bs) bs r-bars)
    (if (null? (cdddr pts))
        (begin
          (truss-nodes cs r-nodes)
          (truss-bars (cdr cs) cs r-bars))
        (begin
          (truss-bars bs (cadddr pts) r-bars)
          (space-truss (cddr pts) r-nodes r-bars)))))

#|(define (hex-space-truss pts r-nodes r-bars b)    ;Function not working
  (let* ((as (car pts))
         (bs (cadr pts))
         (cs (caddr pts))
         (m1s (ptf (car as) (car cs) 1/2))
         (q0s (ptf as (cdr as) 1/2))
         (q1s (between-pts as cs (- 1 b)))
         (q2s (between-pts cs as (- 1 b)))
         (q3s (ptf cs (cdr cs) 1/2)))
    (truss-nodes q0s r-nodes)
    (truss-nodes q1s r-nodes)
    (truss-nodes q2s r-nodes)
    (truss-bar q0s q1s r-bars)
    (truss-bar q1s q2s r-bars)
    (truss-bar q2s q3s r-bars)
    (truss-bar q3s (cdr q2s) r-bars)
    (truss-bar q0s (cdr q1s) r-bars)
    (truss-bar q0s bs r-bars)
    (truss-bar q1s bs r-bars)
    (truss-bar q2s bs r-bars)
    (truss-bar q3s bs r-bars)
    (truss-bar bs (cdr q1s) r-bars)
    (truss-bar bs (cdr q2s) r-bars)
    (truss-bar q0s (cdr q0s) r-bars)
    (truss-bar bs (cdr bs) r-bars)
    (if (null? (cdddr pts))
        (begin
          (truss-nodes q3s r-nodes)
          (truss-bars q3s (cdr q3s) r-bars))
        (begin
          (truss-bars bs (cadddr pts) r-bars)
          (space-truss (cddr pts) r-nodes r-bars b)))))|#

(define (truss-midpoint p0 p1 p2 p3 h)
  (+c (quad-center p0 p1 p2 p3)
      (*c (quad-normal p0 p1 p2 p3) (- h))))

(define (truss-midpoints pts0 pts1 h)
  (cons (truss-midpoint (car pts0) (car pts1) (cadr pts1) (cadr pts0) h)
        (if (null? (cddr pts0))
            (list)
            (truss-midpoints (cdr pts0) (cdr pts1) h))))

(define (truss-quad ptss h)
  (if (null? (cdr ptss))
      ptss
      (cons
       (car ptss)
       (cons (truss-midpoints (car ptss) (cadr ptss) h)
             (truss-quad (cdr ptss) h)))))

(define (truss+surface srf r-nodes r-bars h-truss)
  (iterate-quads (lambda (p0 p1 p2 p3)
                   (surface-polygon p0 p1 p2 p3))
                 srf)
  (space-truss (truss-quad srf h-truss) r-nodes r-bars))

(define (hex-truss p0 p1 p2 p3 a b r-nodes r-bars r-pilar h1 h2)
  (let* ((o (quad-center p0 p1 p2 p3))
         (n (quad-normal p0 p1 p2 p3))
         (m01 (pts-average p0 p1))
         (m12 (pts-average p1 p2))
         (m23 (pts-average p2 p3))
         (m30 (pts-average p3 p0))
         (q0 (between-pts p0 m01 b))
         (q1 (between-pts m01 p1 (- 1.0 b)))
         (q2 (between-pts o m12 (- 1.0 a)))
         (q3 (between-pts m23 p2 (- 1.0 b)))
         (q4 (between-pts p3 m23 b))
         (q5 (between-pts m30 o a))
         (q6 (+c o (*c uz (- h1)))))
    (if (= a 0)
        (begin
          (truss-bar q0 q1 r-bars)
          (truss-bar q1 q2 r-bars)
          (truss-bar q2 q3 r-bars)
          (truss-bar q3 q4 r-bars)
          (truss-bar q4 q5 r-bars)
          (truss-bar q5 q0 r-bars)
          (truss-bar q0 q6 r-bars)
          (truss-bar q1 q6 r-bars)
          (truss-bar q2 q6 r-bars)
          (truss-bar q3 q6 r-bars)
          (truss-bar q4 q6 r-bars)
          (truss-bar q5 q6 r-bars)
          (truss-nodes (list q0 q1 q2 q3 q4 q5 q6) r-nodes)
          (truss-bar q6 (+c q6 (- h2)) r-pilar))
        (begin
          (truss-bar q0 q1 r-bars)
          (truss-bar q1 q2 r-bars)
          (truss-bar q2 q3 r-bars)
          (truss-bar q3 q4 r-bars)
          (truss-bar q4 q5 r-bars)
          (truss-bar q5 q0 r-bars)
          (truss-bar q0 q6 r-bars)
          (truss-bar q1 q6 r-bars)
          (truss-bar q2 q6 r-bars)
          (truss-bar q3 q6 r-bars)
          (truss-bar q4 q6 r-bars)
          (truss-bar q5 q6 r-bars)
          (truss-bar q2 m12 r-bars)
          (truss-bar m30 q5 r-bars)
          (truss-nodes (list q0 q1 q2 q3 q4 q5 q6) r-nodes)
          (truss-bar q6 (+z q6 (- h2)) r-pilar))))) 

(define (truss+surface2 srf a b r-nodes r-bars r-pilar h-truss h-pilar)
  (iterate-quads (lambda (p0 p1 p2 p3)
                   (surface-polygon p0 p1 p2 p3))
                 srf)
  (iterate-quads (lambda (p0 p1 p2 p3)
                  (hex-truss p0 p1 p2 p3 a b r-nodes r-bars r-pilar h-truss h-pilar))
                srf))


;;========================================================================| PANELING/SKIN |==

;;HEXAGONAL PANELS - ATTRACTOR POINT

(define (hex-panel-1 p0 p1 p2 p3 p4 p5 f h att)
  (let* ((c (quad-center p0 p1 p2 p3))
        (n (quad-normal p0 p1 p2 p3))
        (d (* f (distance att c))))
    (loft (list
     (closed-line p0 p1 p2 p3 p4 p5)
     (move
      (scale (closed-line p0 p1 p2 p3 p4 p5) d c)
      (*c n (- h)))))))

(define (hex-panel-2 p0 p1 p2 p3 p4 p5 f h att)
  (let* ((c (quad-center p0 p1 p2 p3))
        (n (quad-normal p0 p1 p2 p3))
        (d (* f (distance att c))))
    (loft (list
     (closed-line p0 p1 p2 p3 p4 p5)
     (move
      (scale (closed-line p0 p1 p2 p3 p4 p5) (/ d 1.3) c)
      (*c n (/ (- h) 2.0)))
     (move
      (scale (closed-line p0 p1 p2 p3 p4 p5) d c)
      (*c n (- h)))))))

(define (hex-panel-3 p0 p1 p2 p3 p4 p5 panel-f panel-h att)
  (surface (closed-line p0 p1 p2 p3 p4 p5)))

(define (skin-closed-srf srf panel panel-f panel-h att)   ;to be used on closed surfaces
  (iterate-quads
   (lambda (h0 h1 h2 h3)
     (panel (list-ref h0 4)
            (list-ref h0 3)
            (list-ref h1 4)
            (list-ref h2 1)
            (list-ref h2 0)
            (list-ref h3 1)
            panel-f panel-h att))
   (let ((hexss
          (iterate-quads
           (lambda (p0 p1 p2 p3)
             (let* ((a (ptf p0 p3 0.17))
                    (b (ptf p0 p1 1/2))
                    (f (ptf p0 p3 1/2))
                    (j (ptf p3 p2 1/2))
                    (e (ptf b j 2/3))
                    (k p1)
                    (l p2)
                    (c (ptf k l 0.17))
                    (d (ptf k l 1/2))
                    (w (ptf k l 2/3))
                    (g (ptf (ptf p0 p3 2/3) (ptf p1 p2 2/3) 3/2))
                    (h (ptf p3 p2 3/2))
                    (i (ptf k l 1.17)))
               (panel a b c d e f panel-f panel-h att)
               (list a b c d e f)))
           (transpose-matrix srf))))
     (for/list ((hexs hexss))
       (append hexs (list (car hexs)))))))

(define (skin-open-srf srf panel panel-f panel-h att)   ;to be used on open surfaces
  (sphere att 0.03)
  (iterate-quads
   (lambda (h0 h1 h2 h3)
     (panel (list-ref h0 4)
            (list-ref h0 3)
            (list-ref h1 4)
            (list-ref h2 1)
            (list-ref h2 0)
            (list-ref h3 1)
            panel-f panel-h att))
   (iterate-quads
    (lambda (p0 p1 p2 p3)
      (let* ((a (ptf p0 p3 0.17))
             (b (ptf p0 p1 1/2))
             (f (ptf p0 p3 1/2))
             (j (ptf p3 p2 1/2))
             (e (ptf b j 2/3))
             (k p1)
             (l p2)
             (c (ptf k l 0.17))
             (d (ptf k l 1/2))
             (w (ptf k l 2/3))
             (g (ptf (ptf p0 p3 2/3) (ptf p1 p2 2/3) 3/2))
             (h (ptf p3 p2 3/2))
             (i (ptf k l 1.17)))
        (panel a b c d e f panel-f panel-h att)
        (list a b c d e f)))
    (transpose-matrix srf))))


;;QUADRANGULAR PANELS - ATTRACTOR VECTOR

(define (quad-panel-1 p0 p1 p2 p3 h f)
  (let* ((c (quad-center p0 p1 p2 p3))
         (n (quad-normal p0 p1 p2 p3))
         (d (* f (/ (angle-vectors n uz) 90))))
    (loft (list
           (line p0 p1 p2 p3 p0)
           (move
            (scale (line p0 p1 p2 p3 p0) d c)
            (*c n h))))))

(define (responsive-skin-1 srf h)
  (iterate-quads (lambda (p0 p1 p2 p3)
                   (quad-panel-1 p0 p1 p2 p3 h)) srf))

(define (quad-panel-2 p0 p1 p2 p3 h f)
  (let* ((c (quad-center p0 p1 p2 p3))
         (n (quad-normal p0 p1 p2 p3))
         (d (* f (/ (angle-vectors n uz) 90))))
    (subtraction (list
                  (extrusion (surface-polygon p0 p1 p2 p3) h)
                  (sphere (+c c (*c n (/ h 2))) d)))))
     
(define (responsive-skin-2 srf h f)
  (iterate-quads (lambda (p0 p1 p2 p3)
                   (quad-panel-2 p0 p1 p2 p3 h f)) srf))

(define (quad-panel-3 p0 p1 p2 p3 h f)
  (let* ((c (quad-center p0 p1 p2 p3))
         (n (quad-normal p0 p1 p2 p3))
         (d (* f (/ (angle-vectors n uz) 90))))
    (subtraction
     (extrusion
     (surface-polygon p0 p1 p2 p3) h)
     (cylinder (+c c (*c n (- h 2)))
               d
               (+c c (*c n (+ h 2)))))))

(define (responsive-skin-3 srf h f)
  (iterate-quads (lambda (p0 p1 p2 p3)
                   (quad-panel-3 p0 p1 p2 p3 h f)) srf))

(define (quad-panel-4 p0 p1 p2 p3 h1 h2 f)
  (let* ((c (quad-center p0 p1 p2 p3))
         (n (quad-normal p0 p1 p2 p3))
         (d (* f (/ (angle-vectors n uz) 90))))
    (loft (list
           (polygon p0 p1 p2 p3)
           (move
            (scale (polygon p0 p1 p2 p3) (/ d 1.3) c)
            (*c n h1))
           (move
            (scale (polygon p0 p1 p2 p3) d c)
            (*c n h2))))))

(define (responsive-skin-4 srf h1 h2 f)
  (iterate-quads (lambda (p0 p1 p2 p3)
                   (quad-panel-4 p0 p1 p2 p3 h1 h2 f)) srf))


;;========================================================================| EXAMPLES/TESTING |==

;Parametric surfaces

;(surface-grid (conic-spiral (xyz 0 0 0) 0.2 1 0.1 2 0 2pi 80 0 2pi 80))
;(surface-grid (cross-cap (xyz 0 0 20) 0 pi 80 0 pi 80))
;(surface-grid (dini (xyz 0 0 0) 0.2 0 (* 6 pi) 80 0.1 (* pi 0.5) 80))
;(surface-grid (dini (xyz 0 0 0) 0.2 0 (* 6 pi) 80 0.1 (* pi 0.7) 80))
;(surface-grid (ellipsoid (xyz 0 0 0) 50 30 30 0 2pi 80 0 2pi 80))
;(surface-grid (enneper1 (xyz 0 0 0) 0 (* 2 pi) 80 0 (* 2 pi) 80))
;(surface-grid (fig8-klein-bottle (xyz 0 0 0) 2 0 (* 2 pi) 80 0 (* 2 pi) 80))
;(surface-grid (steiner-roman-surface (xyz 0 0 0) 0.2 0 pi 80 0 pi 80))
;(surface-grid (mobius-strip2 (xyz 0 0 0) 0.2 0 (* 4 pi) 10 0 0.3 80))
;(surface-grid (torus-surface (xyz 0 0 0) 50 10 0 2pi 50 0 2pi 80))
;(surface-grid (apple-surface (xyz 0 0 0) 0 2pi 50 (- pi) pi 80))
;(surface-grid (sin-u*v (xyz 0 0 0) 0.8 0 2pi 80 0 2pi 80))  
;(surface-grid (sin-u*v2 (xyz 0 0 0) 0 10 80 0 10 80 1 0.2))
;(surface-grid (elliptic-paraboloid (xyz 0 0 0) 2.2 2.2 -pi pi 80 -pi pi 80))
;(surface-grid (hyperboloid (xyz 0 0 0) 2 2 2 -pi pi 80 -pi pi 80))

;Examples of trusses being iterated along a surface

;(truss+surface (sin-u*v (xyz 0 0 0) 0.2 -pi pi 10 -pi pi 10) 0.03 0.01 0.5)
;(truss+surface2 (elliptic-paraboloid (xyz 0 0 3) 5 5 -pi pi 5 -pi pi 5) 0.2 0.8 0.03 0.01 0.03 0.6 2.7)


;Example of hexagonal iteration along a closed surface

;(skin-closed-srf (mobius-strip2 (xyz 0 0 0) 0.2 0 (* 4 pi) 10 0 0.3 80) hex-panel-3 0 0 (xyz 0 0 0))
;(skin-open-srf (elliptic-paraboloid (xyz 0 0 0) 2.2 2.2 -pi pi 20 -pi pi 20) hex-panel-3 0 0 (xyz 0 0 0))

;Paneling examples of hexagonal panels where the opening is being influenced by the distance to an attractor point

;(skin-open-srf (elliptic-paraboloid (xyz 0 0 0) 2.5 2.5 -pi pi 20 -pi pi 20) hex-panel-1 0.15 0.2 (xyz 0 0 1))
;(skin-open-srf (sin-u*v2 (xyz 0 0 0) 0 10 20 0 10 20 1 0.2) hex-panel-2 0.15 0.2 (xyz 5 5 2.5))
;(skin-closed-srf (transpose-matrix (fig8-klein-bottle (xyz 0 0 0) 2 0 2pi 30 0 2pi 30)) hex-panel-1 0.15 0.03 (xyz 0 0 1.3))


;Paneling examples of quadrangular panels where the opening is being influenced by the angle between the normals and vector z

;(responsive-skin-4 (sin-u*v (xyz 0 0 0) 1 -pi pi 20 -pi pi 20) 0.08 0.1 1.0)
;(responsive-skin-4 (sin-u*v2 (xyz 0 0 0) 0 10 20 0 10 20 1 0.2) -0.15 -0.4 1.8)
;(responsive-skin-3  (sin-u*v (xyz 0 0 0) 0.8 -pi pi 20 -pi pi 20) 0.05 0.25)
;(responsive-skin-2  (elliptic-paraboloid (xyz 0 0 0) 2.5 2.5 -pi pi 20 -pi pi 20) 0.05 0.25)

;Examples complete structures (truss + paneling solution)

#|(space-truss (truss-quad (sin-u*v2 (xyz 0 0 0) 0 10 10 0 10 10 1 0.2) 0.5) 0.03 0.01)
(skin-open-srf (sin-u*v2 (xyz 0 0 0) 0 10 10 0 10 10 1 0.2) hex-panel-2 0.15 0.1 (xyz 5 5 2.5))|#

#|(iterate-quads (lambda (p0 p1 p2 p3) (hex-truss p0 p1 p2 p3 0.2 0.8 0.03 0.01 0.03 0.5 2.5)) (sin-u*v (xyz 0 0 0) 0.2 -pi pi 10 -pi pi 10))
(responsive-skin-4 (sin-u*v (xyz 0 0 0) 0.2 -pi pi 10 -pi pi 10) 0.08 0.1 1.5)|#
