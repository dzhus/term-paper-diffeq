(use-modules (srfi srfi-1))

(load "shared.scm")

(define (vector . items)
  items)

(define (matrix . rows)
  rows)

(define row
  vector)

(define (get-item vector n)
  (list-ref vector (- n 1)))

(define get-row get-item)

(define (first-column matrix)
  (map car matrix))

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

;;@ $M \times N$
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

;;@ $\delta^i_j$
(define (kronecker i j)
  (if (= i j) 1 0))

;;@ $E = (\delta^i_j)_{n \times n}$
(define (identity-matrix n)
  (map
   (lambda (i)
     (map
      (lambda (j)
        (kronecker i j))
      (enumerate-n n)))
   (enumerate-n n)))

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
