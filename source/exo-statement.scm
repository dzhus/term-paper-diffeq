;; This file contains initial data for «problem statement»
;; 
;; It must contain definition of a function (f x) preceded by one-line
;; `;;@ ` comments at top-level with TeX $n(x)$ function definition,
;; as well as definition of variables `right-bound`, `subintervals`
;; and `test-epsilon`. Example:
;;
;; ;;@ $n(x) = x^2$
;; (define (f x)
;;   (expt x 2))
;;   
;; (define right-bound 2)
;; (define subintervals 100)
;; (define test-epsilon 0.001)
;;
;; REAL INITIAL DATA STARTS BELOW:

;;@ $n(x) = \left \{ \begin{array}{ll} 35+e^x x^5 & 0<x<2\\ 100 & x \leq 0,\ x \geq 2 \end{array} \right .$
(define (f x)
  (if (and (> x 0) (< x 2))
      (+ 35 (* (exp x) (expt x 5)))
      100))

(define right-bound 2)
(define subintervals 250)
(define test-epsilon 0.000001)