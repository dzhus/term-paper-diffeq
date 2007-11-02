(load "shared.scm")

;;@ $\varphi \colon n(x), u(x) \rightarrow f(x) = e^{ik\abs{x-y}}(k^2-n(y))u(y)$
(define (green-transform u n)
  (let ((k (get-k n)))
    (lambda (x y)
      (* (exp (* +i k (abs (- x y))))
         (- (sqr k) (n y))
         (u y)))))

;; Simpson's formulæ for functions of two arguments
;;@ $\int_a^b f(x) dx$
(define (integrate f a b subintervals)
  (let ((h (/ (- b a) subintervals)))
    (lambda (x)
      (* (/ h 3) 
         (+ (f x a)
            (* 4 (sum
                  (map (lambda (y) (f x y))
                       (split-interval a b
                                       (/ subintervals 2)))))
            (* 2 (sum
                  (map (lambda (y) (f x y))
                       (split-interval (+ a h) (- b h)
                                       (- (/ subintervals 2) 1)))))
            (f x b))))))

;;@ $\op{A} \colon \frac{1}{2ik} \int_0^a {e^{ik|x-y|}(k^2-n(y))u(y) dy} \circ u(x)$
(define (green-integrate u n a subintervals)
  (let ((k (get-k n)))
    (lambda (x)
      (/ ((integrate (green-transform u n) 0 a (/ subintervals 2)) x)
         (* 2 +i k)))))

(define (improve-times u n a subintervals times)
  (define (improve-once u)
    (green-integrate u n a subintervals))
  (if (= times 0)
      u
      (lambda (x)
        (+ (u x)
           ((improve-times (improve-once u) n a subintervals (- times 1)) x)))))

;; Find A, B, given solution u(x)
(define (find-A-B u n a subintervals)
  (let ((k (get-k n)))
    (let ((A ((green-integrate u n a subintervals) 0))
          (B (+ (/ ((integrate (lambda (x y) (* (exp (* -i k y))
                                                (- (sqr k) (n y))
                                                (u y)))
                               0 a subintervals) 0)
                   (* 2 +i k))
                1)))
      (cons A B))))

;; Keep applying `green-integrate` to initial approximation until A
;; and B meet the law of energy conservation.
;;
;; Returns a pair (U . COEFFS), where U is an approximate solution
;;_function_ and COEFFS is (A . B) pair
(define (make-solution n a subintervals eps)
  (let* ((k (get-k n))
         (initial-solution (cons (wave k)
                                 (cons 0 0))))
    (define (improve solution)
      (let* ((u (lambda (x)
                  (+ ((car solution) x)
                     ((green-integrate (car solution) n a subintervals) x))))
             (coeffs (find-A-B u n a subintervals)))
        (cons u coeffs)))
    (define (good? solution)
      (let* ((coeffs (cdr solution))
             (A (car coeffs))
             (B (cdr coeffs)))
        (energy-conserves? A B eps)))
    ((iterative-improve good? improve) initial-solution)))

(define (get-solution right-bound subintervals function test-epsilon)
  (let* ((k (get-k function))
         (initial (wave k))
         (solution (make-solution function right-bound subintervals test-epsilon)))
    (cons (tabulate-function (car solution) 0 right-bound subintervals)
          (cdr solution))))

;; (load "statement.scm")
;; (let* ((k (get-k f))
;;        (initial (wave k))
;;        (u (improve-times (wave k) f right-bound subintervals 1)))
;;   (for-each
;;    (lambda (i) (display i) (newline))
;;    (tabulate-solution u right-bound subintervals)))
