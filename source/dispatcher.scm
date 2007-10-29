(use-modules (ice-9 getopt-long))
(use-modules (ice-9 format))

(load "shared.scm")

;; The «do stuff» procedure
(define (dispatch args)
  (define option-spec
    '((method (single-char #\m) (value #t))
      (right-bound (single-char #\a) (value #t))
      (wave-number (single-char #\k) (value #t))
      (subintervals (single-char #\n) (value #t))
      (statement-file (single-char #\f) (value #t))
      (test-epsilon (single-char #\t) (value #t))))
  (let ((options (getopt-long args option-spec)))
    (let ((statement-file (option-ref options 'statement-file "statement.scm")))
      (load-from-path statement-file)
      (let* ((method (option-ref options 'method "fundmatrix"))
             ;; Command line options override ones in statement file
             (right-bound
              (string->number
               (option-ref options 'right-bound (number->string right-bound))))
             (wave-number
              (string->number
               (option-ref options 'wave-number (number->string wave-number))))
             (subintervals
              (string->number
               (option-ref options 'subintervals (number->string subintervals))))
             (test-epsilon
              (string->number
               (option-ref options 'test-epsilon  (number->string test-epsilon)))))
        (load-from-path (string-concatenate
                         (list method "-solution.scm")))
        (let ((solution (get-solution right-bound wave-number
                                     subintervals f)))
          (print-all-solution solution right-bound 
                              test-epsilon method))))))

;; Print approximate solution (tabulate u(x)) given right bound of
;; interval, wave number, subintervals count and refraction function
(define (print-all-solution solution right-bound test-epsilon used-method)
  (let ((approx (car solution))
        (coeffs (cdr solution)))
    (print-approximate approx 0 right-bound)
    (display "%%")
    (newline)
    (print-A-B coeffs test-epsilon)
    (newline)
    (display "method: ")
    (display used-method)))


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
  (display test-eps)))