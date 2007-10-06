(use-modules (srfi srfi-1))

(define (enumerate i j)
  (if (= i j)
      (list j)
      (append (list i) (enumerate (+ i 1) j))))

(define (enumerate-n n)
  (enumerate 1 n))

(define (make-row . items)
  items)

(define (make-matrix . rows)
  rows)

(define (make-vector . items)
  items)

(define (add-to-vector vector . items)
  (append vector items))

(define (get-row n matrix)
  (list-ref matrix (- n 1)))

(define (get-column n matrix)
  (map (lambda (row) (list-ref row (- n 1)))
       matrix))

(define (first-row matrix)
  (get-row 1 matrix))

(define (first-column matrix)
  (get-column 1 matrix))

(define (matrix-size matrix)
  (length matrix))

(define (count-rows matrix)
  (length matrix))

(define (square? matrix)
  (=
   (length (get-row 1 matrix))
   (length (get-column 1 matrix))))

(define (sum sequence)
  (fold + 0 sequence))

(define (swap-items i j lst)
  (map
   (lambda (index)
     (if (= index i)
         (list-ref lst (- j 1))
         (if (= index j)
             (list-ref lst (- i 1))
             (list-ref lst (- index 1)))))
   (enumerate-n (length lst))))

(define (max-nonzero-index lst)
  (fold
   (lambda (i prev)
     (if (> (list-ref lst (- i 1))
            (list-ref lst (- prev 1)))
         i
         prev))
   1
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