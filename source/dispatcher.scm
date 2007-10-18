(use-modules (ice-9 getopt-long))
(use-modules (ice-9 format))

;; The «do stuff» procedure
(define (dispatch args)
  (define option-spec
    '((method
       (single-char #\m)
       (value #t) (required? #t))
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
         (method (option-ref options 'method "fm"))
         (a (string->number
             (option-ref options 'right-bound "2")))
         (k (string->number
             (option-ref options 'wave-number "5")))
         (n (string->number
             (option-ref options 'subintervals "100")))
         (function-file (option-ref options 'function-file "statement.scm"))
         (epsilon (string->number
                   (option-ref options 'test-epsilon "0.0001"))))
    (load-from-path function-file)
    (if (string=? method "fm")
        (load-from-path "fundmatrix.scm"))
    (print-all-solution a k n f epsilon method)))

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

;; Print approximate solution (tabulate u(x)) given right bound of
;; interval, wave number, subintervals count and refraction function
(define (print-all-solution right-bound wave-number
                            subintervals function
                            test-eps method-alias)
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
    (display "%%")
    (newline)
    (print-A-B coeffs test-eps)
    (newline)
    (display "method: ")
    (display method-alias)))