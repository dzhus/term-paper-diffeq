(use-modules (ice-9 getopt-long))
(use-modules (ice-9 format))

(load "shared.scm")

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
         (method (option-ref options 'method "fundmatrix"))
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
    (load-from-path (string-concatenate
                     (list method "-solution.scm")))
    (print-all-solution a k n f epsilon method)))