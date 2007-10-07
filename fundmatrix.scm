(use-modules (srfi srfi-1))

(load "shared.scm")
(load "matrices.scm")
(load "gauss.scm")

;; Build a sequence of fundamental matrix approximations for each x_i
;; in [a; b] divided by n parts given a matrix of differential
;; equation d²(u) / dx² + n(x)u = 0
(define (build-fundamentals a b n matrix)
  (let ((step (/ (- b a) 
                 n)))
    (evolve-sequence
     (lambda (prev b)
       (matrix-*-matrix
        prev
        ((matrix-exp matrix 5)
         (- b (/ step 2))
         b
         (- b step))))
     (identity-matrix 2)
     (evolve-series
      (lambda (prev n) (+ prev step))
      a
      n))))

;; For d²(u) / dx² + f(x)u = 0
(define (variable-matrix f)
  (lambda (x)
    (list (list 0         1)
          (list (- (f x)) 0))))

;; Find A, B coefficients of wave equations given a sequence of
;; fundamental matrices built for interval [0; right-bound] and k
;; coefficient from wave equations
(define (find-A-B fundamentals k right-bound)
  (let ((fundamental (list-ref fundamentals
                               (- (length fundamentals) 1)))
        (a right-bound))
    (define (w i j)
      (list-ref
       (list-ref fundamental (- i 1))
       (- j 1)))
    (solve-linear
     (list
      ;; @todo Rewrite using infix package
      (list (- (w 1 1) (* (w 1 2) +i k)) (- (exp (* +i k a))))
      (list (- (w 2 1) (* (w 2 2) +i k)) (- (* (exp (* +i k a)) +i k))))
     (list (- (- (* (w 1 2) +i k)) (w 1 1))
           (- (- (* (w 2 2) +i k)) (w 2 1))))))

;; Approximate u(x) on [0; right-bound] given a sequence of
;; fundamental matrices
(define (approximate-solution fundamentals A k right-bound)
  (let ((n (length fundamentals)))
    (map
     (lambda (matrix)
       ;; u(x) is a sum of first row
       (caar
        (matrix-*-vector
         matrix
         (list (+ 1 A)
               (* +i k (- 1 A))))))
     fundamentals)))     

;; Tabulate approximate solution (sutiable for plotting tools)
(define (print-approximate approximation a b)
  (let ((step (/ (- b a)
                 (length approximation))))
    (for-each
     (lambda (n)
       (let ((z (list-ref approximation (- n 1))))
         (display (+ a (* (- n 0.5) step)))
         (display " ")
         (display (real-part z))
         (display " ")
         (display (imag-part z))
         (newline)))
     (enumerate-n (length approximation)))))

;; Check whether found a, b coefficients meet the conservation of
;; energy law:
;;@ $|A|^2 + |B|^2 = 1$
(define (energy-conserves? A B eps)
  (< (abs (- 1 
             (+ (expt (magnitude A) 2)
                (expt (magnitude B) 2))))
     eps))

;;@ $n(x) = \left \{ \begin{array}{ll} 35+3(x-1)^2 & 0<x<2\\ 36 & x \leq 0, x \geq 2 \end{array} \right .$
(define (function x)
  (if (and (< x 2) (> x 0))
      (+ 35 (* 3 (expt (- x 1) 2)))
      36))

;; (define (print-all-solution a b k n f)

;; To be replaced
;; (define (workit)
  (let ((a 0)
      (b 2)
      (k 6)
      (f function)
      (n 200))
  (let ((fundamentals (build-fundamentals a b n (variable-matrix f))))
    (let ((coeffs (find-A-B fundamentals k b)))
      (let ((approx (approximate-solution fundamentals (car coeffs) k b)))
        (print-approximate approx a b)))))
;; )