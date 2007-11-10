(use-modules ((srfi srfi-19) :renamer (symbol-prefix-proc 'srfi-19:)))
(use-modules (ice-9 getopt-long))
(use-modules (ice-9 format))

(load "shared.scm")

;; Command line options override ones in statement file
(define-macro (let-options opts . body)
  `(let ,(map (lambda (opt-name)
                `(,opt-name (string->number
                             (option-ref options
                                         ',opt-name
                                         (number->string ,opt-name)))))
              opts)
     ,@body))

;; The «do stuff» procedure
(define (dispatch args)
  (define (get-seconds time)
    (+ (* 1.0 (/ (srfi-19:time-nanosecond time) (expt 10 9)))
       (srfi-19:time-second time)))
  (define option-spec
    '((method (single-char #\m) (value #t))
      (right-bound (single-char #\a) (value #t))
      (subintervals (single-char #\n) (value #t))
      (statement-file (single-char #\f) (value #t))
      (test-epsilon (single-char #\t) (value #t))))
  (let* ((options (getopt-long args option-spec))
         (statement-file (option-ref options 'statement-file "statement.scm")))
    (load-from-path statement-file)
    (let ((method (option-ref options 'method "fundmatrix")))
      (let-options (right-bound subintervals test-epsilon)
                   (load-from-path (string-concatenate
                                    (list method "-solution.scm")))
                   (let* ((start-time (srfi-19:current-time))
                          (solution (get-solution 
                                     f right-bound
                                     subintervals
                                     test-epsilon))
                          (end-time (srfi-19:current-time))
                          (time-taken (srfi-19:time-difference end-time start-time)))
                     (print-all-solution solution 
                                         right-bound
                                         test-epsilon method
                                         (get-seconds time-taken)))))))

;; Print approximate solution (tabulate u(x)) given solution structure
;; from `get-solution` in method implementation modules, right bound
;; of interval, subintervals count. Also print A, B values and used
;; method name. Textual data exchange simplifies further processing.
(define (print-all-solution solution right-bound test-epsilon used-method time-taken)
  (let ((approx (car solution))
        (coeffs (cdr solution)))
    (print-approximate approx 0 right-bound)
    (display "%%")
    (newline)
    (print-A-B coeffs test-epsilon)
    (newline)
    (display "method: ")
    (display used-method)
    (newline)
    (display "time: ")
    (display time-taken)))

;; Tabulate approximate solution (suitable for plotting tools) given a
;; list of values and min/max variable values
(define (print-approximate solution from to)
  (let ((step (/ (- to from)
                 (length solution))))
    (for-each
     (lambda (n)
       (let ((z (list-ref solution (- n 1))))
         (display (format "~f ~f ~f"
                          (+ from (* (- n 0.5) step))
                          (real-part z)
                          (imag-part z)))
         (newline)))
     (enumerate-n (length solution)))))

(define (print-A-B coeffs test-eps)
  (let ((A (car coeffs))
        (B (cdr coeffs)))
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