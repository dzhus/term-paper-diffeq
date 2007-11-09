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
    (matrix (row 0         1)
            (row (- (n x)) 0))))

;; Find A, B coefficients of wave equations given a sequence of
;; fundamental matrices built for interval [0; right-bound] and k
;; coefficient (wave number)
(define (find-A-B fundamentals k right-bound)
  (let ((fundamental (list-ref fundamentals
                               (- (length fundamentals) 1)))
        (a right-bound))
    (define (w i j)
      (get-item (get-row fundamental i) j))
    (solve-linear
     (matrix
      (row (- (w 1 1) (* (w 1 2) +i k))
           (- (exp (* +i k a))))
      (row (- (w 2 1) (* (w 2 2) +i k)) 
           (- (* (exp (* +i k a)) +i k))))
     (vector 
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
       (vector (+ 1 A)
                    (* +i k (- 1 A))))))
   fundamentals))

;; Keep doubling subintervals count until energy conserves.
;; 
;; Returns a pair (U . COEFFS), where U is an approximate solution
;; _function_ and COEFFS is (A . B) pair
(define (make-solution refraction right-bound subintervals eps)
  (let ((wave-number (get-wave-number refraction)))
    (define (improve solution)
      (let* ((fundamentals (build-fundamentals 
                            right-bound
                            (* 2 (length (car solution)))
                            (variable-matrix refraction)))
             (coeffs (find-A-B
                      fundamentals
                      wave-number
                      right-bound))
             (A (car coeffs))
             (approx (approximate-solution 
                      fundamentals
                      A wave-number
                      right-bound)))
        (cons approx (cons A (cadr coeffs)))))
    (define (good? solution)
      (let* ((coeffs (cdr solution))
             (A (car coeffs))
             (B (cdr coeffs)))
        (energy-conserves? A B eps)))
    (let* ((initial-solution
            ;; Trivial dummy function from «initial» solution is not
            ;; used ever
            (cons (tabulate-function (lambda (x) 0)
                                     0 right-bound subintervals)
                  (cons 0 0))))
      ((iterative-improve good? improve) initial-solution))))

(define (get-solution refraction right-bound subintervals test-epsilon)
  (make-solution refraction right-bound subintervals test-epsilon))