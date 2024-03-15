(in-package #:vumbra.common)

;;; Constants

(defconstant +epsilon+ 1e-7)

(defconstant +pi+ (float pi 1.0f0))

(defconstant +half-pi+ (/ +pi+ 2))

;;; log10
;;; Base 10 logarithm

(defun log10 ((x :float))
  (* (log2 x) 0.30103))

(defun log10 ((x :vec2))
  (* (log2 x) 0.30103))

(defun log10 ((x :vec3))
  (* (log2 x) 0.30103))

(defun log10 ((x :vec4))
  (* (log2 x) 0.30103))

;;; Saturate
;;; Clamp a value within the [0, 1] range. From HLSL.

(defun saturate ((x :float))
  (clamp x 0.0 1.0))

(defun saturate ((x :vec2))
  (clamp x 0.0 1.0))

(defun saturate ((x :vec3))
  (clamp x 0.0 1.0))

(defun saturate ((x :vec4))
  (clamp x 0.0 1.0))

;;; Domain mapping

(defun map-domain ((x :float)
                   (source-min :float)
                   (source-max :float)
                   (dest-min :float)
                   (dest-max :float))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))

(defun map-domain ((x :vec2)
                   (source-min :vec2)
                   (source-max :vec2)
                   (dest-min :vec2)
                   (dest-max :vec2))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))

(defun map-domain ((x :vec3)
                   (source-min :vec3)
                   (source-max :vec3)
                   (dest-min :vec3)
                   (dest-max :vec3))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))

(defun map-domain ((x :vec4)
                   (source-min :vec4)
                   (source-max :vec4)
                   (dest-min :vec4)
                   (dest-max :vec4))
  (+ dest-min
     (* (- x source-min) (/ (- dest-max dest-min) (- source-max source-min)))))
