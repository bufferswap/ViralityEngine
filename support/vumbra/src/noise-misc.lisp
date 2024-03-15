(in-package #:vumbra.noise)

;;;; Miscellaneous noise functions
;;;; Brian Sharpe https://github.com/BrianSharpe/GPU-Noise-Lib

;;; 2D Cubist noise

(defun cubist ((point :vec2)
               (range-clamp :vec2)
               (hash-fn (function (:vec2) (:vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vecs (- (.xyxy point) (vec4 cell (1+ cell))))
           (hash-x hash-y hash (funcall hash-fn cell))
           (grad-x (- hash-x 0.5 +epsilon+))
           (grad-y (- hash-y 0.5 +epsilon+))
           (temp (* (- hash 0.5)
                    (/ (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                          (+ (* grad-x (.xzxz vecs))
                             (* grad-y (.yyww vecs)))))))
           (blend (shape:quintic-curve (.xy vecs)))
           (blend (vec4 blend (- 1 blend)))
           (out (dot temp (* (.zxzx blend) (.wwyy blend)))))
    (saturate (* (- out (.x range-clamp)) (.y range-clamp)))))

(defun cubist ((point :vec2)
               (range-clamp :vec2))
  (cubist point range-clamp (lambda ((x :vec2)) (hash:fast32/3-per-corner x))))

;;; 3D Cubist noise

(defun cubist ((point :vec3)
               (range-clamp :vec2)
               (hash-fn
                (function
                 (:vec3)
                 (:vec4 :vec4 :vec4 :vec4 :vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (vec-1 (1- vec))
           (hash-x0 hash-y0 hash-z0 hash0 hash-x1 hash-y1 hash-z1 hash1
                    (funcall hash-fn cell))
           (grad-x0 (- hash-x0 0.5 +epsilon+))
           (grad-y0 (- hash-y0 0.5 +epsilon+))
           (grad-z0 (- hash-z0 0.5 +epsilon+))
           (grad-x1 (- hash-x1 0.5 +epsilon+))
           (grad-y1 (- hash-y1 0.5 +epsilon+))
           (grad-z1 (- hash-z1 0.5 +epsilon+))
           (temp1 (* (- hash0 0.5)
                     (/ (* (inversesqrt (+ (* grad-x0 grad-x0)
                                           (* grad-y0 grad-y0)
                                           (* grad-z0 grad-z0)))
                           (+ (* (.xyxy (vec2 (.x vec) (.x vec-1))) grad-x0)
                              (* (.xxyy (vec2 (.y vec) (.y vec-1))) grad-y0)
                              (* (.z vec) grad-z0))))))
           (temp2 (* (- hash1 0.5)
                     (/ (* (inversesqrt (+ (* grad-x1 grad-x1)
                                           (* grad-y1 grad-y1)
                                           (* grad-z1 grad-z1)))
                           (+ (* (.xyxy (vec2 (.x vec) (.x vec-1))) grad-x1)
                              (* (.xxyy (vec2 (.y vec) (.y vec-1))) grad-y1)
                              (* (.z vec-1) grad-z1))))))
           (blend (shape:quintic-curve vec))
           (out (mix temp1 temp2 (.z blend)))
           (blend (vec4 (.xy blend) (- 1 (.xy blend)))))
    (saturate (* (- (dot out (* (.zxzx blend) (.wwyy blend)))
                    (.x range-clamp))
                 (.y range-clamp)))))

(defun cubist ((point :vec3)
               (range-clamp :vec2))
  (cubist point range-clamp (lambda ((x :vec3))
                              (hash:fast32/4-per-corner x))))

;;; 2D Stars noise

(defun stars ((point :vec2)
              (probability-threshold :float)
              (max-dimness :float)
              (radius :float)
              (hash-fn (function (:vec2) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (value (- 1 (* max-dimness (.z hash)))))
    (multf vec (vec2 radius))
    (decf vec (vec2 (1- radius)))
    (incf vec (* (.xy hash) (- radius 2)))
    (if (< (.w hash) probability-threshold)
        (* (shape:falloff-squared-c1 (min (dot vec vec) 1)) value)
        0.0)))

(defun stars ((point :vec2)
              (probability-threshold :float)
              (max-dimness :float)
              (radius :float))
  (stars point probability-threshold max-dimness radius
         (lambda ((x :vec2)) (hash:fast32/cell x))))
