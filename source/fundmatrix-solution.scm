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
;; coefficient (wave number)
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

;; Keep doubling subintervals count until energy conserves.
;; 
;; Returns a pair (U . COEFFS), where U is an approximate solution
;; _function_ and COEFFS is (A . B) pair
(define (make-solution refract right-bound subintervals eps)
  (let ((k (get-k refract)))
    (define (improve solution)
      (let* ((fundamentals (build-fundamentals 
                            right-bound
                            (* 2 (length (car solution)))
                            (variable-matrix refract)))
             (coeffs (find-A-B fundamentals k right-bound))
             (A (car coeffs))
             (approx (approximate-solution 
                      fundamentals A k
                      right-bound)))
        (cons approx (cons A (cadr coeffs)))))
    (define (good? solution)
      (let* ((coeffs (cdr solution))
             (A (car coeffs))
             (B (cdr coeffs)))
        (energy-conserves? A B eps)))
    (let* ((initial-solution (cons
                              (tabulate-function (lambda (x) x) 0 right-bound (/ subintervals 2))
                              (cons 0 0))))
      ((iterative-improve good? improve) initial-solution))))

(define (get-solution right-bound subintervals function test-epsilon)
  (make-solution function right-bound subintervals test-epsilon))