#!/usr/bin/guile -s
!#

(use-modules (srfi srfi-1))

;;@ $\sum \limits_n s_n$
(define (sum sequence)
  (fold + 0 sequence))

;;@ $( \ldots ((a_n x + a_{n-1})x + a_{n-2}) x \ldots + a_1)x + a_0)$
(define (general-horner-eval x coefficient-sequence mult add zero)
  (fold-right (lambda (this-coeff higher-terms)
                (add (mult higher-terms x)
                     this-coeff))
              zero
              coefficient-sequence))

;; Square matrix dimension
(define (matrix-size m)
  (length m))

;;@ $A = (a_{ij})_{m \times n} \to A^T = (a_{ji})_{n \times m}$
(define (transpose m)
  (array->list
   (transpose-array 
    (list->array (list (length m) 
                       (length (car m))) 
                 m)
    1 0)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col) 
                  (sum (map * row col)))
                cols))
         m)))

;;@ $A \times \vec{v}$
(define (matrix-*-vector matrix vector)
  (matrix-*-matrix matrix (map list vector)))

;;@ $A = (a_{ij}) \to A \cdot c = (a_{ij} \cdot c)$
(define (matrix-*-number matrix n)
  (map
   (lambda (row)
     (map (lambda (x) (* x n)) row))
   matrix))

;;@ $M + N = (m_{ij} + n_{ij})$
(define (add-matrices m n)
  (map 
   (lambda (row1 row2)
     (map + row1 row2))
   m n))

;;@ $A = (a_{ij} = 0)_{n \times n}$
(define (zero-matrix n)
  (map (lambda (row)
         (map (lambda (i) 0)
              (enumerate-n n)))
       (enumerate-n n)))

;;@ $A = (a_{ij} = \delta^i_j)_{n \times n}$
(define (identity-matrix n)
  (map (lambda (row)
         (map (lambda (i) (if (= i row)
                              1
                              0))
              (enumerate-n n)))
       (enumerate-n n)))

;;@ $l, l+1 \ldots h-1, h$
(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

;;@ $1, 2, 3 \ldots n$
(define (enumerate-n n)
  (enumerate-interval 1 n))

;;@ $a_{s_1}, a_{s_2} \ldots a_{s_n}$, $a_{s_k} = f(a_{s_{k-1}}, s_k)$
(define (evolve-sequence evolve initial index)
  (define (iter index prev result)
    (if (null? index)
        result
        (let ((current (evolve prev (car index))))
          (iter (cdr index)
                current
                (append result (list current))))))
  (iter index initial (list initial)))

;;@ $a_1, a_2 \ldots a_n$
(define (evolve-series evolve initial n)
  (evolve-sequence evolve initial (enumerate-n n)))

;;@ $1/0!, 1/1!, 1/2! \ldots 1/n!$
(define (exp-series-coefficients n)
  (evolve-series (lambda (prev i) (/ prev i)) 
                 1
                 (- n 1)))
  
;;@ $f(x, y, z) = e^{A(x)(y-z)}$
(define (matrix-exp A n)
  (lambda (x y z)
    (let ((matrix (A x)))
    (general-horner-eval
     (matrix-*-number matrix (- y z))
     (exp-series-coefficients n)
     matrix-*-matrix
     (lambda (high-terms coeff)
       (add-matrices high-terms
                     (matrix-*-number 
                      (identity-matrix (matrix-size matrix))
                      coeff)))
     (zero-matrix (matrix-size matrix))))))

;;@ $e^A$
(define (const-matrix-exp matrix n)
  ((matrix-exp (lambda (x) matrix) n) 0 1 0))


;; Simpliest version of Gauss method solving
(define (solve-linear coeff vector)
  ;; Make all zeroes in `coeff` first column (except first row)
  (define (diag-matrix-step)
    (map (lambda (subrow)
           (map
            (lambda (x y)
              (- x 
                 (/ (* (caar coeff) y) 
                    (car subrow))))
            (cdar coeff) (cdr subrow)))
         (cdr coeff)))
  ;; Perform the same operation upon a vector
  (define (diag-vector-step)
    (map
     (lambda (f v)
       (- (car vector) 
          (/ (* v (caar coeff))
             (car f))))
     (cdr coeff) (cdr vector)))
  (if (= (matrix-size coeff) 1)
      ;; Solve trivial equation (ax=c) immediately
      (let ((a (caar coeff)))
        (if (= a 0)
            #f
            (list (/ (car vector) a))))
      (let ((subsolution (solve-linear (diag-matrix-step)
                                       (diag-vector-step))))
        (append
         ;; Solve an equation with only 1 variable
         (solve-linear (list (list (caar coeff)))
                       (list (- (car vector)
                                (sum (map * 
                                          (cdar coeff)
                                          subsolution)))))
         subsolution))))

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

;; workflow:
;; 
;; (define (n x) (..))
;; (define (variable-matrix f) (..))
;; (define fundamentals (build-fundamentals 0.0 a 10^n (variable-matrix n)))
;; (define coeffs (find-A-B fundamentals k a))
;; (define A (car coeffs))
;; (define B (cadr coeffs))
;; 
;; (define solution (approximate-solution fundamentals A k a))


;;@ $n(x) = \left \{ \begin{array}{ll} 35+3(x-1)^2 & 0<x<2\\ 36 & x \leq 0, x \geq 2 \end{array} \right .$
(define (function x)
  (if (and (< x 2) (> x 0))
      (+ 35 (* 3 (expt (- x 1) 2)))
      36))

;; (define (print-all-solution a b k n f)

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