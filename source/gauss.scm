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

;;@ $l_1, \dotsc, l_i, l_j, \dotsc, l_n \rightarrow l_1, \dotsc, l_j, l_i, \dotsc, l_n$
(define (swap-items i j lst)
  (map
   (lambda (index)
     (if (= index i)
         (list-ref lst (- j 1))
         (if (= index j)
             (list-ref lst (- i 1))
             (list-ref lst (- index 1)))))
   (enumerate-n (length lst))))

;; Solve a system of linear equations given its matrix and , given A is _invertible_
(define (solve-linear A v)
  (define (top-left equations)
    (caar equations))
  (define (top-right equations)
    (let ((first (car equations)))
      (get-item first (length first))))
  (define (row-reduce equations)
    (map
     (lambda (equation)
       (map
        (lambda (first-eq coeff)
          (- coeff
           (* first-eq
              (/ (car equation)
                 (top-left equations)))))
        (cdr (car equations))
        (cdr equation)))
     (cdr equations)))
  ;; Solve a system of linear equations given its augmented matrix
  (define (solve-equations equations)
    (if (= (matrix-size equations) 1)
        ;; Solve trivial equation (ax=c) immediately
        (vector (/ (top-right equations)
                   (top-left equations)))
        ;; Choose maximum element in first column and make that row a
        ;; new top to avoid accidental division by zero (non-zero
        ;; element always exists as A is invertible)
        (let* ((next-row (max-nonzero-index
                         (first-column equations)))
               (equations (swap-items 1 next-row equations))
               (subsolution (solve-equations 
                             (row-reduce equations))))
          (append
           ;; Solve an equation with only 1 variable (backward
           ;; pass)
           (solve-equations (matrix
                             (row (top-left equations)
                                  (- (top-right equations)
                                     (sum (map * 
                                               (drop-right
                                                (cdr (car equations)) 1)
                                               subsolution))))))
           subsolution))))
  (let ((augmented (map (lambda (matrix-row vector-item)
                          (append matrix-row (list vector-item)))
                        A v)))
    (solve-equations augmented)))
