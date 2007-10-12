;; This file contains initial data for «problem statement»
;; 
;; It must contain definition of a function (f x) preceded by one-line
;; `;;@ ` comments at top-level with TeX $n(x)$ function definition,
;; as well as comment lines starting with `;;@ ` characters containg
;; pairs of the following variables and values (example):
;; 
;; right-bound 2
;; wave-number 3
;; subintervals 100

;; INITIAL DATA STARTS BELOW

;;@ $n(x) = \left \{ \begin{array}{ll} 35+3(x-1)^2 & 0<x<2\\ 36 & x \leq 0,\ x \geq 2 \end{array} \right .$
(define (f x)
  (if (and (< x 2) (> x 0))
      (+ 35 (* 3 (expt (- x 1) 2)))
      36))

;;@ right-bound 2
;;@ wave-number 5
;;@ subintervals 200