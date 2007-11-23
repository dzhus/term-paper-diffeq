(load "shared.scm")

;;@ $\varphi (u(x), n(x), g(x, t)) = f(x,t) = e^{ik \cdot g(x, t)}(k^2-n(t))u(t)$
(define (green-subtransform u n g)
  (let ((k (get-wave-number n)))
    (lambda (x t)
      (* (exp (* +i k (g x t)))
         (- (sqr k) (n t))
         (u t)))))

;;@ $\hat{\varphi} (u(x), n(x)) = f(x,t) = e^{ik \abs{x-t}}(k^2-n(t))u(t)$
(define (green-transform u n)
  (green-subtransform u n
                      (lambda (x t) (abs (- x t)))))

;; Simpson's formulæ for functions of two arguments
;;@ $\int \limits_a^b {f(x, t) dt}$
(define (integrate f a b subintervals)
  (let ((h (/ (- b a) subintervals)))
    (lambda (x)
      (* (/ h 3) 
         (+ (f x a)
            (* 4 (sum
                  (map (lambda (t) (f x t))
                       (split-interval a b
                                       (/ subintervals 2)))))
            (* 2 (sum
                  (map (lambda (t) (f x t))
                       (split-interval (+ a h) (- b h)
                                       (- (/ subintervals 2) 1)))))
            (f x b))))))

;;@ $\op{A} \comp u(x) \colon \frac{1}{2ik} \int_0^a {e^{ik|x-t|}(k^2-n(t))u(t) dt}$
(define (green-integrate u refraction right-bound subintervals)
  (let ((k (get-wave-number refraction)))
    (lambda (x)
      (/ ((integrate (green-transform u refraction) 
                     0 right-bound subintervals) x)
         (* 2 +i k)))))

;; Find A, B, given solution u(x), n(x), [0;a] and subintervals count
(define (find-A-B solution refraction right-bound subintervals)
  (let ((k (get-wave-number refraction)))
    (let (
          ;;@ $A = \frac{1}{2ik}\int_0^a{e^{ikt}(k^2-n(t))u(t)dt}$
          (A ((green-integrate solution refraction
                               right-bound subintervals) 0))
          ;;@ $B = \frac{1}{2ik}\int_0^a{e^{-ikt}(k^2-n(t))u(t)dt} + 1$
          (B (+ (/ ((integrate (green-subtransform
                                solution refraction 
                                (lambda (x t) (- t)))
                               0 right-bound subintervals) 0)
                   (* 2 +i k))
                1)))
      (cons A B))))

;; Keep applying `green-integrate` to initial approximation until A
;; and B meet the law of energy conservation.
;; 
;; Returns a pair (U . COEFFS), where U is an approximate solution
;; _function_ and COEFFS is (A . B) pair
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