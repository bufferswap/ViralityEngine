(in-package #:vumbra.noise)

;;;; Polka-dot noise
;;;; Brian Sharpe https://github.com/BrianSharpe/GPU-Noise-Lib

;;; 2D Polka-dot noise

(defun polkadot ((point :vec2)
                 (radius-low :float)
                 (radius-high :float)
                 (hash-fn (function (:vec2) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (radius (max 0 (+ radius-low
                           (* (.z hash) (- radius-high radius-low)))))
         (value (/ radius (max radius-high radius-low)))
         (radius (/ 2 radius))
         (vec (+ (* (.xy hash) (- radius 2))
                 (- (* vec radius) (1- radius)))))
    (* (shape:falloff-squared-c2 (min (dot vec vec) 1.0)) value)))

(defun polkadot ((point :vec2)
                 (radius-low :float)
                 (radius-high :float))
  (polkadot point radius-low radius-high (lambda ((x :vec2))
                                           (hash:fast32/cell x))))

;;; 2D Polka-dot noise (box version)

(defun polkadot-box ((point :vec2)
                     (radius-low :float)
                     (radius-high :float)
                     (hash-fn (function (:vec2) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (radius (max 0 (+ radius-low
                           (* (.z hash) (- radius-high radius-low)))))
         (value (/ radius (max radius-high radius-low)))
         (radius (/ 2 radius))
         (vec (expt (+ (* (.xy hash) (- radius 2))
                       (- (* vec radius) (1- radius)))
                    (vec2 2))))
    (* (shape:falloff-squared-c2 (min (dot vec vec) 1.0)) value)))

(defun polkadot-box ((point :vec2)
                     (radius-low :float)
                     (radius-high :float))
  (polkadot-box point radius-low radius-high (lambda ((x :vec2))
                                               (hash:fast32/cell x))))

;;; 3D Polka-dot noise

(defun polkadot ((point :vec3)
                 (radius-low :float)
                 (radius-high :float)
                 (hash-fn (function (:vec3) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (radius (max 0 (+ radius-low
                           (* (.w hash) (- radius-high radius-low)))))
         (value (/ radius (max radius-high radius-low)))
         (radius (/ 2 radius))
         (vec (+ (- (* vec radius) (1- radius))
                 (* (.xyz hash) (- radius 2)))))
    (* (shape:falloff-squared-c2 (min (dot vec vec) 1)) value)))

(defun polkadot ((point :vec3)
                 (radius-low :float)
                 (radius-high :float))
  (polkadot point radius-low radius-high (lambda ((x :vec3))
                                           (hash:fast32/cell x))))

;;; 3D Polka-dot noise (box version)

(defun polkadot-box ((point :vec3)
                     (radius-low :float)
                     (radius-high :float)
                     (hash-fn (function (:vec3) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (radius (max 0 (+ radius-low
                           (* (.w hash) (- radius-high radius-low)))))
         (value (/ radius (max radius-high radius-low)))
         (radius (/ 2 radius))
         (vec (+ (- (* vec radius) (1- radius))
                 (* (.xyz hash) (- radius 2))))
         (vec (* vec vec)))
    (* (shape:falloff-squared-c2 (min (dot vec vec) 1)) value)))

(defun polkadot-box ((point :vec3)
                     (radius-low :float)
                     (radius-high :float))
  (polkadot-box point radius-low radius-high (lambda ((x :vec3))
                                               (hash:fast32/cell x))))
