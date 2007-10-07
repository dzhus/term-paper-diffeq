;;@ $\sum \limits_n s_n$
(define (sum sequence)
  (fold + 0 sequence))

;;@ $( \ldots ((a_n x + a_{n-1})x + a_{n-2}) x \ldots + a_1)x + a_0)$
(define (general-horner-eval x coefficient-sequence mult add zero)
  (fold-right (lambda (this-coeff higher-terms)
                (add (mult higher-terms x)
                     this-coeff))
              zero
              coefficient-sequence))

;;@ $l, l+1 \ldots h-1, h$
(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

;;@ $1, 2, 3 \ldots n$
(define (enumerate-n n)
  (enumerate-interval 1 n))

;;@ $a_{s_1}, a_{s_2} \ldots a_{s_n}$, $a_{s_k} = f(a_{s_{k-1}}, s_k)$
(define (evolve-sequence evolve initial index)
  (define (iter index prev result)
    (if (null? index)
        result
        (let ((current (evolve prev (car index))))
          (iter (cdr index)
                current
                (append result (list current))))))
  (iter index initial (list initial)))

;;@ $a_1, a_2 \ldots a_n$
(define (evolve-series evolve initial n)
  (evolve-sequence evolve initial (enumerate-n n)))

;;@ $1/0!, 1/1!, 1/2! \ldots 1/n!$
(define (exp-series-coefficients n)
  (evolve-series (lambda (prev i) (/ prev i)) 
                 1
                 (- n 1)))
