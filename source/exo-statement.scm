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

;;@ $n(x) = \begin{cases} \frac{1}{x+0.1}+e^x x^5 & 0<x<2\\ 100 & x \leq 0,\ x \geq 2 \end{cases}$
(define (f x)
  (if (and (> x 0) (< x 2))
      (+ (/ 1 (+ 0.1 x)) (* (exp x) (expt x 5)))
      100))

(define right-bound 2)
(define subintervals 250)
(define test-epsilon 0.00001)