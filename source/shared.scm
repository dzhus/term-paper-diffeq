(use-modules (srfi srfi-1))

;;@ $\sum \limits_n s_n$
(define (sum sequence)
  (fold + 0 sequence))

;;@ $x^2$
(define (sqr x) (* x x))

;;@ $( \ldots ((a_n \cdot x + a_{n-1}) \cdot x + a_{n-2}) \cdot x \ldots + a_1) \cdot x + a_0)$
(define (general-horner-eval x coefficient-sequence
                             mult add zero)
  (fold-right 
   (lambda (this-coeff higher-terms)
     (add (mult higher-terms x) this-coeff))
   zero
   coefficient-sequence))

;;@ $l, l+1 \ldots h-1,\ h$
(define (enumerate-interval low high)
  (define (iter low high acc)
    (if (> low high)
        acc
        (iter low (- high 1) (cons high acc))))
  (iter low high '()))

;;@ $ 1, 2 \ldots n$
(define (enumerate-n n)
  (enumerate-interval 1 n))

;;@ $a_{s_1}, a_{s_2} \ldots a_{s_n},\ a_{s_k} = f(a_{s_{k-1}}, s_k)$
(define (evolve-sequence evolve initial index)
  (define (evolve-next index prev acc)
    (if (null? index)
        acc
        (let ((current (evolve prev (car index))))
          (evolve-next (cdr index)
                current
                (append acc (list current))))))
  (evolve-next index initial (list initial)))

;;@ $a_1, a_2 \ldots a_n,\ a_k=f(a_{k-1})$
(define (evolve-series evolve initial n)
  (evolve-sequence evolve initial (enumerate-n n)))

;;@ $x_1, x_2 \ldots x_{n-1},\ x_k = a+h(k-\frac{1}{2}), h = (b-a)/n$
(define (split-interval a b n)
  (let ((step (/ (- b a) n)))
    (evolve-series (lambda (x n) (+ x step)) 
                   (+ a (/ step 2))
                   (- n 1))))

;;@ $ 1/0!, 1/1!, 1/2! \ldots 1/(n-1)!$
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

;; Calculate a set of f(x) values on given [a; b] interval
(define (tabulate-function f a b subintervals)
  (map f (split-interval a b subintervals)))

;;@ $e^{ikx}$
(define (wave k)
  (lambda (x)
    (exp (* +i k x))))

;; Get wave number from given n(x) function definition (it's
;; considered to be constant equal to k^2 when x<0)
(define (get-wave-number function)
  (sqrt (function -1)))

;; General iterative improvement method
(define (iterative-improve good? improve)
  (define (solve x)
    (if (good? x)
        x
        (solve (improve x))))
  solve)