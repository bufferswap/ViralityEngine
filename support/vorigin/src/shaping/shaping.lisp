(in-package #:cl-user)

(defpackage #:vorigin.shaping
  (:local-nicknames
   (#:const #:vorigin.constants)
   (#:u #:vutils))
  (:use #:cl)
  ;; shaping
  (:export
   #:linear
   #:sine-out
   #:sine-in
   #:sine-in-out
   #:quadratic-out
   #:quadratic-in
   #:quadratic-in-out
   #:cubic-out
   #:cubic-in
   #:cubic-in-out
   #:quartic-out
   #:quartic-in
   #:quartic-in-out
   #:quintic-out
   #:quintic-in
   #:quintic-in-out
   #:exponential-out
   #:exponential-in
   #:exponential-in-out
   #:circular-out
   #:circular-in
   #:circular-in-out
   #:back-out
   #:back-in
   #:back-in-out
   #:elastic-out
   #:elastic-in
   #:elastic-in-out
   #:bounce-out
   #:bounce-in
   #:bounce-in-out
   #:hermite-curve
   #:quintic-curve))

(in-package #:vorigin.shaping)

(u:fn-> linear (u:f32) u:f32)
(declaim (inline linear))
(defun linear (x)
  (declare (optimize speed))
  x)

(u:fn-> sine-out (u:f32) u:f32)
(declaim (inline sine-out))
(defun sine-out (x)
  (declare (optimize speed))
  (sin (* const:pi 0.5f0 x)))

(u:fn-> sine-in (u:f32) u:f32)
(declaim (inline sine-in))
(defun sine-in (x)
  (declare (optimize speed))
  (1+ (sin (* (1- x) 0.5f0 const:pi))))

(u:fn-> sine-in-out (u:f32) u:f32)
(declaim (inline sine-in-out))
(defun sine-in-out (x)
  (declare (optimize speed))
  (* 0.5f0 (- 1 (cos (* x const:pi)))))

(u:fn-> quadratic-out (u:f32) u:f32)
(declaim (inline quadratic-out))
(defun quadratic-out (x)
  (declare (optimize speed))
  (- (* x (- x 2))))

(u:fn-> quadratic-in (u:f32) u:f32)
(declaim (inline quadratic-in))
(defun quadratic-in (x)
  (declare (optimize speed))
  (* x x))

(u:fn-> quadratic-in-out (u:f32) u:f32)
(declaim (inline quadratic-in-out))
(defun quadratic-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (* x x 2)
      (1- (+ (* x x -2) (* x 4f0)))))

(u:fn-> cubic-out (u:f32) u:f32)
(declaim (inline cubic-out))
(defun  cubic-out (x)
  (declare (optimize speed))
  (1+ (expt (1- x) 3)))

(u:fn-> cubic-in (u:f32) u:f32)
(declaim (inline cubic-in))
(defun cubic-in (x)
  (declare (optimize speed))
  (expt x 3))

(u:fn-> cubic-in-out (u:f32) u:f32)
(declaim (inline cubic-in-out))
(defun cubic-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (* (expt x 3) 4)
      (1+ (* 0.5f0 (expt (- (* x 2) 2) 3)))))

(u:fn-> quartic-out (u:f32) u:f32)
(declaim (inline quartic-out))
(defun quartic-out (x)
  (declare (optimize speed))
  (1+ (* (expt (1- x) 3) (- 1 x))))

(u:fn-> quartic-in (u:f32) u:f32)
(declaim (inline quartic-in))
(defun quartic-in (x)
  (declare (optimize speed))
  (expt x 4))

(u:fn-> quartic-in-out (u:f32) u:f32)
(declaim (inline quartic-in-out))
(defun quartic-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (* (expt x 4) 8)
      (1+ (* -8 (expt (1- x) 4)))))

(u:fn-> quintic-out (u:f32) u:f32)
(declaim (inline quintic-out))
(defun quintic-out (x)
  (declare (optimize speed))
  (1+ (expt (1- x) 5)))

(u:fn-> quintic-in (u:f32) u:f32)
(declaim (inline quintic-in))
(defun quintic-in (x)
  (declare (optimize speed))
  (expt x 5))

(u:fn-> quintic-in-out (u:f32) u:f32)
(declaim (inline quintic-in-out))
(defun quintic-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (* (expt x 5) 16)
      (1+ (* 0.5f0 (expt (- (* x 2) 2) 5)))))

(u:fn-> exponential-out (u:f32) u:f32)
(declaim (inline exponential-out))
(defun exponential-out (x)
  (declare (optimize speed))
  (if (= x 1)
      x
      (- 1 (expt 2 (* x -10)))))

(u:fn-> exponential-in (u:f32) u:f32)
(declaim (inline exponential-in))
(defun exponential-in (x)
  (declare (optimize speed))
  (if (zerop x)
      x
      (expt 2 (* 10 (1- x)))))

(u:fn-> exponential-in-out (u:f32) u:f32)
(declaim (inline exponential-in-out))
(defun exponential-in-out (x)
  (declare (optimize speed))
  (cond
    ((or (zerop x) (= x 1))
     x)
    ((< x 0.5f0)
     (* 0.5f0 (expt 2 (- (* x 20) 10))))
    (t
     (1+ (* -0.5f0 (expt 2 (+ (* x -20) 10)))))))

(u:fn-> circular-out (u:f32) u:f32)
(declaim (inline circular-out))
(defun circular-out (x)
  (declare (optimize speed))
  (sqrt (the (u:f32 0f0) (* (- 2 x) x))))

(u:fn-> circular-in (u:f32) u:f32)
(declaim (inline circular-in))
(defun circular-in (x)
  (declare (optimize speed))
  (- 1 (sqrt (the (u:f32 0f0) (- 1 (* x x))))))

(u:fn-> circular-in-out (u:f32) u:f32)
(declaim (inline circular-in-out))
(defun circular-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (* 0.5f0 (- 1 (sqrt (the (u:f32 0f0) (- 1 (* 4 x x))))))
      (* 0.5f0 (1+ (sqrt (the (u:f32 0f0) (- (* (- (* 2 x) 3) (1- (* 2 x))))))))))

(u:fn-> back-out (u:f32) u:f32)
(declaim (inline back-out))
(defun back-out (x)
  (declare (optimize speed))
  (let ((v (- 1 x)))
    (- 1 (- (expt v 3) (* v (sin (* v const:pi)))))))

(u:fn-> back-in (u:f32) u:f32)
(declaim (inline back-in))
(defun back-in (x)
  (declare (optimize speed))
  (- (expt x 2) (* x (sin (* x const:pi)))))

(u:fn-> back-in-out (u:f32) u:f32)
(declaim (inline back-in-out))
(defun back-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (let ((v (* x 2)))
        (* 0.5f0 (- (expt v 3) (* v (sin (* v const:pi))))))
      (let ((v (- 2 (* x 2))))
        (+ (* 0.5f0 (- 1 (- (expt v 3) (* v (sin (* v const:pi)))))) 0.5f0))))

(u:fn-> elastic-out (u:f32) u:f32)
(declaim (inline elastic-out))
(defun elastic-out (x)
  (declare (optimize speed))
  (1+ (* (sin (* -13 const:pi 0.5f0 (1+ x))) (expt 2 (* -10 x)))))

(u:fn-> elastic-in (u:f32) u:f32)
(declaim (inline elastic-in))
(defun elastic-in (x)
  (declare (optimize speed))
  (* (sin (* 13 const:pi 0.5f0 x)) (expt 2 (* 10 (1- x)))))

(u:fn-> elastic-in-out (u:f32) u:f32)
(declaim (inline elastic-in-out))
(defun elastic-in-out (x)
  (declare (optimize speed))
  (if (< x 0.5f0)
      (let ((v (* x 2)))
        (* 0.5f0 (sin (* 13 const:pi 0.5f0 v)) (expt 2 (* 10 (1- v)))))
      (let ((v (1- (* x 2))))
        (* 0.5f0 (+ (* (sin (* -13 const:pi 0.5f0 (1+ v))) (expt 2 (* -10 v))) 2)))))

(u:fn-> bounce-out (u:f32) u:f32)
(declaim (inline bounce-out))
(defun bounce-out (x)
  (declare (optimize speed))
  (cond
    ((< x 0.36363637f0)
     (/ (* x x 121) 16))
    ((< x 0.72727275f0)
     (+ (- (* 9.075f0 x x) (* 9.9f0 x)) 3.4f0))
    ((< x 0.9f0)
     (+ (- (* 12.066482f0 x x) (* 19.635458f0 x)) 8.898061f0))
    (t
     (+ (- (* 10.8f0 x x) (* 20.52f0 x)) 10.72f0))))

(u:fn-> bounce-in (u:f32) u:f32)
(declaim (inline bounce-in))
(defun bounce-in (x)
  (declare (optimize speed))
  (- 1 (bounce-out (- 1 x))))

(u:fn-> bounce-in-out (u:f32) u:f32)
(declaim (inline bounce-in-out))
(defun bounce-in-out (x)
  (declare (optimize speed))
  (let ((v (* x 2)))
    (if (< x 0.5f0)
        (* 0.5f0 (bounce-in v))
        (+ 0.5f0 (* 0.5f0 (bounce-out (1- v)))))))

(u:fn-> hermite-curve (u:f32) u:f32)
(declaim (inline hermite-curve))
(defun hermite-curve (x)
  (declare (optimize speed))
  (* x x (- 3 (* 2 x))))

(u:fn-> quintic-curve (u:f32) u:f32)
(declaim (inline quintic-curve))
(defun quintic-curve (x)
  (declare (optimize speed))
  (* (expt x 3) (+ (* x (- (* x 6) 15)) 10)))
