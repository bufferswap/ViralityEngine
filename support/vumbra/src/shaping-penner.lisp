(in-package #:vumbra.shaping)

;;;; Shaping functions
;;;; Various functions to modify a signal or interpolate a value non-linearly.
;;;; Robert Penner http://www.robertpenner.com/easing

(defun linear ((x :float))
  x)

(defun sine-out ((x :float))
  (sin (* +half-pi+ x)))

(defun sine-in ((x :float))
  (1+ (sin (* (1- x) +half-pi+))))

(defun sine-in-out ((x :float))
  (* 0.5 (- 1 (cos (* x +pi+)))))

(defun quadratic-out ((x :float))
  (- (* x (- x 2))))

(defun quadratic-in ((x :float))
  (* x x))

(defun quadratic-in-out ((x :float))
  (if (< x 0.5)
      (* x x 2)
      (1- (+ (* x x -2) (* x 4)))))

(defun cubic-out ((x :float))
  (let ((f (1- x)))
    (1+ (* f f f))))

(defun cubic-in ((x :float))
  (* x x x))

(defun cubic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x 4)
      (let ((f (- (* x 2) 2)))
        (1+ (* 0.5 f f f)))))

(defun quartic-out ((x :float))
  (let ((f (1- x)))
    (1+ (* f f f (- 1 x)))))

(defun quartic-in ((x :float))
  (* x x x x))

(defun quartic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x x 8)
      (let ((f (1- x)))
        (1+ (* -8 f f f f)))))

(defun quintic-out ((x :float))
  (let ((f (- x 1)))
    (1+ (* f f f f f))))

(defun quintic-in ((x :float))
  (* x x x x x))

(defun quintic-in-out ((x :float))
  (if (< x 0.5)
      (* x x x x x 16)
      (let ((f (- (* x 2) 2)))
        (1+ (* 0.5 f f f f f)))))

(defun exponential-out ((x :float))
  (if (= x 1)
      x
      (- 1 (expt 2 (* x -10)))))

(defun exponential-in ((x :float))
  (if (zerop x)
      x
      (expt 2 (* 10 (1- x)))))

(defun exponential-in-out ((x :float))
  (cond
    ((or (zerop x) (= x 1))
     x)
    ((< x 0.5)
     (* 0.5 (expt 2 (- (* x 20) 10))))
    (t
     (1+ (* -0.5 (expt 2 (+ (* x -20) 10)))))))

(defun circular-out ((x :float))
  (sqrt (* (- 2 x) x)))

(defun circular-in ((x :float))
  (- 1 (sqrt (- 1 (* x x)))))

(defun circular-in-out ((x :float))
  (if (< x 0.5)
      (* 0.5 (- 1 (sqrt (- 1 (* 4 x x)))))
      (* 0.5 (1+ (sqrt (- (* (- (* 2 x) 3) (1- (* 2 x)))))))))

(defun back-out ((x :float))
  (let ((f (- 1 x)))
    (- 1 (- (* f f f) (* f (sin (* f +pi+)))))))

(defun back-in ((x :float))
  (- (* x x x) (* x (sin (* x +pi+)))))

(defun back-in-out ((x :float))
  (if (< x 0.5)
      (let ((f (* x 2)))
        (* 0.5 (- (* f f f) (* f (sin (* f +pi+))))))
      (let ((f (- 2 (* x 2))))
        (+ (* 0.5 (- 1 (- (* f f f) (* f (sin (* f +pi+)))))) 0.5))))

(defun elastic-out ((x :float))
  (1+ (* (sin (* -13 +half-pi+ (1+ x))) (expt 2 (* -10 x)))))

(defun elastic-in ((x :float))
  (* (sin (* 13 +half-pi+ x)) (expt 2 (* 10 (1- x)))))

(defun elastic-in-out ((x :float))
  (if (< x 0.5)
      (let ((v (* x 2)))
        (* 0.5 (sin (* 13 +half-pi+ v)) (expt 2 (* 10 (1- v)))))
      (let ((v (1- (* x 2))))
        (* 0.5 (+ (* (sin (* -13 +half-pi+ (1+ v))) (expt 2 (* -10 v))) 2)))))

(defun bounce-out ((x :float))
  (cond
    ((< x 0.36363637)
     (/ (* x x 121) 16.0))
    ((< x 0.72727275)
     (+ (- (* 9.075 x x) (* 9.9 x)) 3.4))
    ((< x 0.9)
     (+ (- (* 12.066482 x x) (* 19.635458 x)) 8.898061))
    (t
     (+ (- (* 10.8 x x) (* 20.52 x)) 10.72))))

(defun bounce-in ((x :float))
  (- 1 (bounce-out (- 1 x))))

(defun bounce-in-out ((x :float))
  (let ((f (* x 2)))
    (if (< x 0.5)
        (* 0.5 (bounce-in f))
        (+ 0.5 (* 0.5 (bounce-out (1- f)))))))
