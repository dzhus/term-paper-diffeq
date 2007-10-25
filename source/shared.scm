;;@ $\sum \limits_n s_n$
(define (sum sequence)
  (fold + 0 sequence))

;;@ $( \ldots ((a_n \cdot x + a_{n-1}) \cdot x + a_{n-2}) \cdot x \ldots + a_1) \cdot x + a_0)$
(define (general-horner-eval x coefficient-sequence
                             mult add zero)
  (fold-right 
   (lambda (this-coeff higher-terms)
     (add (mult higher-terms x) this-coeff))
   zero
   coefficient-sequence))

;;@ $l,\ l+1\ \ldots\ h-1,\ h$
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

;;@ $ 1,\ 2\ \ldots\ n$
(define (enumerate-n n)
  (enumerate-interval 1 n))

;;@ $a_{s_1},\ a_{s_2}\ \ldots\ a_{s_n},\ a_{s_k} = f(a_{s_{k-1}}, s_k)$
(define (evolve-sequence evolve initial index)
  (define (evolve-next index prev result)
    (if (null? index)
        result
        (let ((current (evolve prev (car index))))
          (evolve-next (cdr index)
                current
                (append result (list current))))))
  (evolve-next index initial (list initial)))

;;@ $a_1,\ a_2\ \ldots\ a_n,\ a_k=f(a_{k-1})$
(define (evolve-series evolve initial n)
  (evolve-sequence evolve initial (enumerate-n n)))

;;@ $ 1/0!,\ 1/1!,\ 1/2! \ldots 1/(n-1)!$
(define (exp-series-coefficients n)
  (evolve-series (lambda (prev i) (/ prev i)) 
                 1
                 (- n 1)))

;; Check whether found a, b coefficients meet the conservation of
;; energy law:
;;@ $|A|^2 + |B|^2 = 1$
(define (energy-conserves? A B eps)
  (< (abs (- 1 
             (+ (expt (magnitude A) 2)
                (expt (magnitude B) 2))))
     eps))

;; Tabulate approximate solution (suitable for plotting tools) given a
;; list of values and min/max variable values
(define (print-approximate solution from to)
  (let ((step (/ (- to from)
                 (length solution))))
    (for-each
     (lambda (n)
       (let ((z (list-ref solution (- n 1))))
         (display (+ from (* (- n 0.5) step)))
         (display " ")
         (display (real-part z))
         (display " ")
         (display (imag-part z))
         (newline)))
     (enumerate-n (length solution)))))

(define (print-A-B coeffs test-eps)
  (let ((A (car coeffs))
        (B (caadr coeffs)))
  (display "A: ")
  (format #t "~,5i" A)
  (newline)
  (display "B: ")
  (format #t "~,5i" B)
  (newline)
  (display "conserves: ")
  (if (energy-conserves? A B test-eps)
      (display "yes")
      (display "no"))
  (newline)
  (display "eps: ")
  (display test-eps)
  ))