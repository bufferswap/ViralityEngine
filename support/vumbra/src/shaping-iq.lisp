(in-package #:vumbra.shaping)

;;;; Shaping Functions
;;;; Various functions to modify a signal or interpolate a value non-linearly.
;;;; Inigo Quilez http://www.iquilezles.org/www/articles/functions/functions.htm

(defun almost-identity ((x :float)
                        (threshold :float)
                        (min :float))
  (if (> x threshold)
      x
      (let ((a (- (* 2.0 min) threshold))
            (b (- (* 2.0 threshold) (* 3.0 min)))
            (v (/ x threshold)))
        (+ (* (+ (* a v) b) v v) min))))

(defun impulse ((x :float) (k :float))
  (let ((h (* x k)))
    (* h (exp (- 1 h)))))

(defun cubic-pulse ((x :float)
                    (center :float)
                    (width :float))
  (let ((x (abs (- x center))))
    (if (> x width)
        0.0
        (progn
          (divf x width)
          (- 1.0 (* x x (- 3.0 (* 2.0 x))))))))

(defun exponential-step ((x :float)
                         (exponent :float)
                         (sharpness :float))
  (exp (* (- sharpness) (expt x exponent))))

(defun gain ((x :float)
             (k :float))
  (let* ((v (if (< x 0.5) x (- 1.0 x)))
         (a (* 0.5 (expt (* 2 v) k))))
    (if (< x 0.5)
        a
        (- 1.0 a))))

(defun parabola ((x :float)
                 (k :float))
  (expt (* 4.0 x (- 1.0 x)) k))


(defun power-curve ((x :float)
                    (a :float)
                    (b :float))
  (let ((k (/ (expt (+ a b) (+ a b))
              (* (expt a a) (expt b b)))))
    (* k (expt x a) (expt (- 1.0 x) b))))

(defun sinc-curve ((x :float)
                   (k :float))
  (let ((a (* +pi+ (1- (* k x)))))
    (/ (sin a) a)))
