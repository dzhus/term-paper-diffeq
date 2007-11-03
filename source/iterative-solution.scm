(load "shared.scm")

;;@ $\varphi \colon n(x), u(x) \rightarrow f(x) = e^{ik\abs{x-y}}(k^2-n(y))u(y)$
(define (green-transform u n)
  (let ((k (get-wave-number n)))
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
  (let ((k (get-wave-number n)))
    (lambda (x)
      (/ ((integrate (green-transform u n) 0 a (/ subintervals 2)) x)
         (* 2 +i k)))))

;; Find A, B, given solution u(x), n(x), [0;a] and subintervals count
(define (find-A-B solution refraction right-bound subintervals)
  (let ((k (get-wave-number refraction)))
    (let ((A ((green-integrate solution refraction right-bound subintervals) 0))
          (B (+ (/ ((integrate (lambda (x y) (* (exp (* -i k y))
                                                (- (sqr k) (refraction y))
                                                (solution y)))
                               0 right-bound subintervals) 0)
                   (* 2 +i k))
                1)))
      (cons A B))))

;; Keep applying `green-integrate` to initial approximation until A
;; and B meet the law of energy conservation.
;;
;; Returns a pair (U . COEFFS), where U is an approximate solution
;;_function_ and COEFFS is (A . B) pair
(define (make-solution refraction right-bound subintervals eps)
  (let* ((k (get-wave-number refraction))
         (initial-solution (cons (wave k)
                                 (cons 0 0))))
    (define (improve solution)
      (let* ((u (lambda (x)
                  (+ ((car solution) x)
                     ((green-integrate 
                       (car solution)
                       refraction
                       right-bound subintervals) x))))
             (coeffs (find-A-B
                      u refraction
                      right-bound subintervals)))
        (cons u coeffs)))
    (define (good? solution)
      (let* ((coeffs (cdr solution))
             (A (car coeffs))
             (B (cdr coeffs)))
        (energy-conserves? A B eps)))
    ((iterative-improve good? improve) initial-solution)))

(define (get-solution refraction right-bound subintervals test-epsilon)
  (let* ((wave-number (get-wave-number refraction))
         (initial (wave wave))
         (solution (make-solution
                    refraction
                    right-bound subintervals test-epsilon)))
    (cons (tabulate-function (car solution)
                             0 right-bound subintervals)
          (cdr solution))))