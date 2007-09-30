(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (accumulate-n op initial seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op initial (map car seqs))
            (accumulate-n op initial (map cdr seqs)))))

(define (general-horner-eval x coefficient-sequence mult add zero)
  (accumulate (lambda (this-coeff higher-terms)
                (add (mult higher-terms x) this-coeff))
              zero
              coefficient-sequence))

(define (matrix-size m)
  (length (car m)))

(define (transpose m)
  (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col) 
                  (accumulate + 0 (map * row col)))
                cols))
         m)))

(define (matrix-*-number m n)
  (map
   (lambda (row)
     (map (lambda (x) (* x n)) row))
   m))

(define (add-matrices m n)
  (map 
   (lambda (row1 row2)
     (map + row1 row2))
   m n))

(define (zero-matrix n)
  (map (lambda (row)
         (map (lambda (i) 0)
              (enumerate-n n)))
       (enumerate-n n)))

(define (identity-matrix n)
  (map (lambda (row)
         (map (lambda (i) (if (= i row)
                              1
                              0))
              (enumerate-n n)))
       (enumerate-n n)))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-n n)
  (enumerate-interval 1 n))

;; n > 0
(define (evolve-sequence initial evolve n)
  (define (iter k prev result)
    (if (>= k n)
        result
        (let ((this (evolve prev k)))
          (iter (+ k 1) this (append result (list this))))))
  (iter 1 initial (list initial)))

;; 1/0!, 1/1!, 1/2!, .. 1/n!
(define (exp-series-coefficients n)
  (evolve-sequence 1 (lambda (prev i) (/ prev i)) n))

;; f(x, y, z) = e^{A(x)(y-z)}
;; @correct
(define (matrix-exp A n)
  (lambda (x y z)
    (let ((matrix (A x)))
    (general-horner-eval
     (matrix-*-number matrix (- y z))
     (exp-series-coefficients n)
     matrix-*-matrix
     (lambda (high coeff) (add-matrices high (matrix-*-number 
                                              (identity-matrix (matrix-size matrix))
                                              coeff)))
     (zero-matrix (matrix-size matrix))))))

;; e^A
(define (const-matrix-exp matrix n)
  ((matrix-exp (lambda (x) matrix) 0 n) 1))

(define (variable-matrix n)
  (lambda (x)
    (list (list 0         1)
          (list (- (n x)) 0))))

;; n(x)
(define (f x)
  (if (and (< x 2) (> x 0))
      (+ 35 (* 3 (expt (- x 1) 2)))
      36))

(define (build-fundamental a b n function)
  (define matrix
    (variable-matrix function))
  (if (>= a b)
      (identity-matrix 2)
      (matrix-*-matrix
       ((matrix-exp matrix 5)
        (- b (/ (- b a) (* 2 n))) 
        b
        (- b (/ (- b a) n)))
       (build-fundamental a (- b (/ (- b a) n)) (- n 1) function))))

(define (find-a-b k a n function)
  (let ((fundamental (build-fundamental 0 a n function)))
    (define (w i j)
      (list-ref
       (list-ref fundamental
                 (- i 1))
       (- j 1)))
    (solve-linear
     (list
      (list (- (w 1 1) (* (w 1 2) +i k)) (- (exp (* +i k a))))
      (list (- (w 2 1) (* (w 2 2) +i k)) (- (* (exp (* +i k a)) +i k))))
     (list (- (- (* (w 1 2) +i k)) (w 1 1))
           (- (- (* (w 2 2) +i k)) (w 2 1))))))

;; Simpliest version of Gauss method solving
;; @correct
(define (solve-linear coeff vector)
  (define (diag-matrix-step)
    (map (lambda (subrow)
           (map
            (lambda (x y)
              (- x 
                 (/ (* (caar coeff) y) 
                    (car subrow))))
            (cdar coeff) (cdr subrow)))
         (cdr coeff)))
  (define (diag-vector-step)
    (map
     (lambda (f v)
       (- (car vector) 
          (/ (* v (caar coeff))
             (car f))))
     (cdr coeff) (cdr vector)))
  (if (= (length coeff) 1)
      (let ((c (caar coeff)))
        (if (= c 0)
            #f
            (list (/ (car vector) c))))
      (let ((subsolution (solve-linear (diag-matrix-step)
                                       (diag-vector-step))))
        (append 
         (solve-linear (list (list (caar coeff)))
                       (list (- (car vector)
                          (accumulate + 0 (map * (cdar coeff)
                                               subsolution)))))
         subsolution))))