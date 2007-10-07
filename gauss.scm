(use-modules (srfi srfi-1))

(load "shared.scm")
(load "matrices.scm")

(define (max-nonzero-index lst)
  (fold
   (lambda (i prev)
     (if (> (real-part (list-ref lst (- i 1)))
            (real-part (list-ref lst (- prev 1))))
         i
         prev))
   1
   (enumerate-n (length lst))))

(define (swap-items i j lst)
  (map
   (lambda (index)
     (if (= index i)
         (list-ref lst (- j 1))
         (if (= index j)
             (list-ref lst (- i 1))
             (list-ref lst (- index 1)))))
   (enumerate-n (length lst))))

;; Solve a linear system with square matrix A, given A is invertible
;; @correct
(define (solve-linear coeffs vector)
  ;; Make all zeroes in `coeff` first column (except first row)
  (define (top-left coeffs)
    (caar coeffs))
  (define (coeffs-reduce coeffs)
    (map
     (lambda (coeff-row)
       (map
        (lambda (first-row coeff)
          (- coeff
             (* first-row
                (/ (car coeff-row)
                   (top-left coeffs)))))
        (cdr (car coeffs))
        (cdr coeff-row)))
     (cdr coeffs)))
  ;; Perform the same operation upon a vector
  (define (vector-reduce vector coeffs)
    (map
     (lambda (coeff-row vector-row)
       (- vector-row
          (* (car vector)
             (/ (car coeff-row)
                (top-left coeffs)))))
     (cdr coeffs)
     (cdr vector)))
  (if (= (matrix-size coeffs) 1)
      ;; Solve trivial equation (ax=c) immediately
      (if (= (top-left coeffs) 0)
          #f
          (make-vector (/ (car vector)
                          (top-left coeffs))))
      ;; Choose maximum non-zero element in first column and make that
      ;; row a new top to avoid accidental division by zero (non-zero
      ;; element always exists as A is invertible)
      (let ((next-row (max-nonzero-index
                       (first-column coeffs))))
          (let ((coeffs (swap-items 1 next-row coeffs))
                (vector (swap-items 1 next-row vector)))
            (let ((subsolution (solve-linear 
                                (coeffs-reduce coeffs)
                                (vector-reduce vector
                                               coeffs))))
              (add-to-vector
               ;; Solve an equation with only 1 variable (backward
               ;; pass)
               (solve-linear (make-matrix (make-row (top-left coeffs)))
                             (make-vector (- (first vector)
                                             (sum (map * 
                                                       (cdr (car coeffs))
                                                       subsolution)))))
               subsolution))))))