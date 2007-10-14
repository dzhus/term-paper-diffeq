(use-modules (srfi srfi-1))

(load "shared.scm")
(load "matrices.scm")
(load "gauss.scm")

;; Build a sequence of fundamental matrix approximations for each x_i
;; in [0; right-bound] divided by n parts given a matrix of differential
;; equation d²(u) / dx² + n(x)u = 0
(define (build-fundamentals right-bound n matrix)
  (let ((a right-bound)
        (step (/ right-bound n)))
    (evolve-sequence
     (lambda (prev a)
       (matrix-*-matrix
        prev
        ((matrix-exp matrix 5)
         (- a (/ step 2))
         a
         (- a step))))
     (identity-matrix 2)
     (evolve-series (lambda (prev n) (+ prev step))
                    0 n))))

;; For d²(u) / dx² + n(x)u = 0
(define (variable-matrix n)
  (lambda (x)
    (make-matrix (make-row 0         1)
                 (make-row (- (n x)) 0))))

;; Find A, B coefficients of wave equations given a sequence of
;; fundamental matrices built for interval [0; right-bound] and k
;; coefficient from wave equations
(define (find-A-B fundamentals k right-bound)
  (let ((fundamental (list-ref fundamentals
                               (- (length fundamentals) 1)))
        (a right-bound))
    (define (w i j)
      (row-item j (get-row i fundamental)))
    (solve-linear
     (make-matrix
      (make-row (- (w 1 1) (* (w 1 2) +i k))
                (- (exp (* +i k a))))
      (make-row (- (w 2 1) (* (w 2 2) +i k)) 
                (- (* (exp (* +i k a)) +i k))))
     (make-vector 
      (- (- (* (w 1 2) +i k)) (w 1 1))
      (- (- (* (w 2 2) +i k)) (w 2 1))))))

;; Approximate u(x) on [0; right-bound] given a sequence of
;; fundamental matrices and A, k coefficients
(define (approximate-solution fundamentals
                              A k right-bound)
  (let ((n (length fundamentals)))
    (map
     (lambda (matrix)
       (caar
        (matrix-*-vector
         matrix
         (make-vector (+ 1 A)
                      (* +i k (- 1 A))))))
     fundamentals)))     

;; Check whether found a, b coefficients meet the conservation of
;; energy law:
;;@ $|A|^2 + |B|^2 = 1$
(define (energy-conserves? A B eps)
  (< (abs (- 1 
             (+ (expt (magnitude A) 2)
                (expt (magnitude B) 2))))
     eps))
