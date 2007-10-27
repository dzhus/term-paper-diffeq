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
         a
         (+ a (/ step 2))
         (- a (/ step 2)))))
     (identity-matrix 2)
     (split-interval 0 right-bound n))))

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
(define (approximate-solution fundamentals A k right-bound)
  (map
   (lambda (matrix)
     (caar
      (matrix-*-vector
       matrix
       (make-vector (+ 1 A)
                    (* +i k (- 1 A))))))
   fundamentals))

;; Print approximate solution (tabulate u(x)) given right bound of
;; interval, wave number, subintervals count and refraction function
(define (print-all-solution right-bound wave-number
                            subintervals function
                            test-eps method-alias)
  (let* ((fundamentals (build-fundamentals right-bound
                                           subintervals
                                           (variable-matrix f)))
         (coeffs (find-A-B fundamentals
                           wave-number
                           right-bound))
         (approx (approximate-solution fundamentals
                                       (car coeffs)
                                       wave-number right-bound)))
    (print-approximate approx 0 right-bound)
    (display "%%")
    (newline)
    (print-A-B coeffs test-eps)
    (newline)
    (display "method: ")
    (display method-alias)))