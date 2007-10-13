(use-modules (ice-9 getopt-long))

(load "fundmatrix.scm")

;; The «do stuff» procedure
(define (dispatch args)
  (define option-spec
    '(
      ;; a]
      (right-bound
       (single-char #\a)
       (value #t) (required? #t))
       ;; k
       (wave-number
        (single-char #\k)
        (value #t) (required? #t))
       (subintervals
        (single-char #\n) (value #t))
       (function-file
        (single-char #\f)
        (value #t) (required? #f))
       (test-epsilon
        (single-char #\t)
        (value #t) (required? #f))
       ))
  (let* ((options (getopt-long args option-spec))
         (a (string->number
             (option-ref options 'right-bound 2)))
         (k (string->number
             (option-ref options 'wave-number 5)))
         (n (string->number
             (option-ref options 'subintervals 100)))
         (function-file (option-ref options 'function-file "statement.scm"))
         (epsilon (string->number
                   (option-ref options 'test-epsilon 0.0001))))
    (load function-file)
    (print-all-solution a k n f epsilon)))
  
;; Tabulate approximate solution (suitable for plotting tools) given a
;; list of values and min/max variable values
(define (print-approximate approximation from to)
  (let ((step (/ (- to from)
                 (length approximation))))
    (for-each
     (lambda (n)
       (let ((z (list-ref approximation (- n 1))))
         (display (+ from (* (- n 0.5) step)))
         (display " ")
         (display (real-part z))
         (display " ")
         (display (imag-part z))
         (newline)))
     (enumerate-n (length approximation)))))

(define (print-A-B coeffs test-eps)
  (let ((A (car coeffs))
        (B (caadr coeffs)))
  (display "A: ")
  (display A)
  (newline)
  (display "B: ")
  (display B)
  (newline)
  (if (energy-conserves? A B test-eps)
      (display "success")
      (display "fail"))
  (newline)
  (display "eps: ")
  (display test-eps)
  ))

;; Print approximate solution (tabulate u(x)) given right bound of
;; interval, wave number, subintervals count and refraction function
(define (print-all-solution right-bound wave-number
                            subintervals function test-eps)
  (let* ((fundamentals (build-fundamentals right-bound
                                           subintervals
                                           (variable-matrix f)))
         (coeffs (find-A-B fundamentals
                           wave-number
                           right-bound))
         (approx (approximate-solution fundamentals
                                       (car coeffs)
                                       wave-number right-bound)))
    (print-approximate approx 0 right-bound)
    (print-A-B coeffs test-eps)))