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
        (print-all-solution right-bound
                            wave-number
                            subintervals
                            f
                            test-epsilon
                            method)))))