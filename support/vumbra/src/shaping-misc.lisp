(in-package #:vumbra.shaping)

;;;; Shaping Functions
;;;; Various functions to modify a signal or interpolate a value non-linearly.

;;; Hermite curve
;;; Identical to smoothstep

(defun hermite-curve ((x :float))
  (* x x (- 3 (* 2 x))))

(defun hermite-curve ((x :vec2))
  (* x x (- 3 (* 2 x))))

(defun hermite-curve ((x :vec3))
  (* x x (- 3 (* 2 x))))

(defun hermite-curve ((x :vec4))
  (* x x (- 3 (* 2 x))))

(defun hermite-curve ((x :float)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (hermite-curve x)))

(defun hermite-curve ((x :vec2)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (hermite-curve x)))

(defun hermite-curve ((x :vec3)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (hermite-curve x)))

(defun hermite-curve ((x :vec4)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (hermite-curve x)))

;;; Quintic curve
;;; Ken Perlin https://mrl.nyu.edu/~perlin/paper445.pdf

(defun quintic-curve ((x :float))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun quintic-curve ((x :vec2))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun quintic-curve ((x :vec3))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun quintic-curve ((x :vec4))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defun quintic-curve ((x :float)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve x)))

(defun quintic-curve ((x :vec2)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve x)))

(defun quintic-curve ((x :vec3)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve x)))

(defun quintic-curve ((x :vec4)
                      (min :float)
                      (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve x)))

(defun quintic-curve/interpolate-derivative ((x :vec2))
  (let ((x (.xyxy x)))
    (* x x (+ (* x (+ (* x (+ (* x (vec4 6 6 0 0)) (vec4 -15 -15 30 30)))
                      (vec4 10 10 -60 -60)))
              (vec4 0 0 30 30)))))

(defun quintic-curve/derivative ((x :vec3))
  (* x x (+ (* x (- (* x 30) 60)) 30)))

;;; Fast quintic curve
;;; Brian Sharpe https://github.com/BrianSharpe/GPU-Noise-Lib

(defun quintic-curve/fast ((x :float))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun quintic-curve/fast ((x :vec2))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun quintic-curve/fast ((x :vec3))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun quintic-curve/fast ((x :vec4))
  (let ((x3 (* x x x)))
    (* (+ 7 (* (- x3 7) x)) x3)))

(defun quintic-curve/fast ((x :float)
                           (min :float)
                           (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve/fast x)))

(defun quintic-curve/fast ((x :vec2)
                           (min :float)
                           (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve/fast x)))

(defun quintic-curve/fast ((x :vec3)
                           (min :float)
                           (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve/fast x)))

(defun quintic-curve/fast ((x :vec4)
                           (min :float)
                           (max :float))
  (let ((x (saturate (/ (- x min) (- max min)))))
    (quintic-curve/fast x)))

;;; Quintic Hermite interpolation
;;; David L. Finn https://www.rose-hulman.edu/~finn/CCLI/Notes/day09.pdf

(defun quintic-hermite ((x :float)
                        (ival0 :float)
                        (ival1 :float)
                        (egrad0 :float)
                        (egrad1 :float))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (+ ival0
       (dot (vec3 (- ival1 ival0) egrad0 egrad1)
            (+ h123 (vec3 0 x 0))))))

(defun quintic-hermite ((x :float)
                        (ival0 :vec4)
                        (ival1 :vec4)
                        (egrad0 :vec4)
                        (egrad1 :vec4))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (+ ival0
       (* (- ival1 ival0) (.x h123))
       (* egrad0 (vec4 (+ (.y h123) x)))
       (* egrad1 (.z h123)))))

(defun quintic-hermite ((x :float)
                        (igrad0 :vec2)
                        (igrad1 :vec2)
                        (egrad0 :vec2)
                        (egrad1 :vec2))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (+ (* (vec4 egrad1 igrad0)
          (vec4 (.zz h123) 1 1))
       (* (vec4 egrad0 (.xx h123))
          (vec4 (vec2 (+ (.y h123) x))  (- igrad1 igrad0))))))

(defun quintic-hermite ((x :float)
                        (ival0 :vec4)
                        (ival1 :vec4)
                        (igrad-x0 :vec4)
                        (igrad-x1 :vec4)
                        (igrad-y0 :vec4)
                        (igrad-y1 :vec4)
                        (egrad0 :vec4)
                        (egrad1 :vec4))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (values (+ ival0
               (* (- ival1 ival0) (.x h123))
               (* egrad0 (vec4 (+ (.y h123) x)))
               (* egrad1 (.z h123)))
            (+ igrad-x0 (* (- igrad-x1 igrad-x0) (.x h123)))
            (+ igrad-y0 (* (- igrad-y1 igrad-y0) (.x h123))))))

(defun quintic-hermite ((x :float)
                        (igrad-x0 :vec4)
                        (igrad-x1 :vec4)
                        (igrad-y0 :vec4)
                        (igrad-y1 :vec4)
                        (egrad0 :vec4)
                        (egrad1 :vec4))
  (let* ((c0 (vec3 -15 8 7))
         (c1 (vec3 6 -3 -3))
         (c2 (vec3 10 -6 -4))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) x x x)))
    (values (+ (* egrad0 (vec4 (+ (.y h123) x)))
               (* egrad1 (.z h123)))
            (+ igrad-x0 (* (- igrad-x1 igrad-x0) (.x h123)))
            (+ igrad-y0 (* (- igrad-y1 igrad-y0) (.x h123))))))

(defun quintic-hermite/derivative ((x :float)
                                   (ival0 :float)
                                   (ival1 :float)
                                   (egrad0 :float)
                                   (egrad1 :float))
  (let* ((c0 (vec3 30 -15 -15))
         (c1 (vec3 -60 32 28))
         (c2 (vec3 30 -18 -12))
         (h123 (* (+ (* (+ c1 (* c0 x)) x) c2) x x)))
    (dot (vec3 (- ival1 ival0) egrad0 egrad1)
         (+ h123 (vec3 0 1 0)))))

;;; Falloff functions
;;; Brian Sharpe https://github.com/BrianSharpe/GPU-Noise-Lib

(defun falloff-squared-c1 ((x :float))
  (let ((x (- 1 x)))
    (* x x)))

(defun falloff-squared-c2 ((x :float))
  (let ((x (- 1 x)))
    (* x x x)))

(defun falloff-squared-c2 ((x :vec4))
  (let ((x (- 1 x)))
    (* x x x)))
