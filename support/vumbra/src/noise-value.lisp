(in-package #:vumbra.noise)

;;;; Value noise

;;; 2D Value noise

(defun value ((point :vec2)
              (hash-fn (function (:vec2) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (blend (shape:quintic-curve vec))
         (blend (vec4 blend (- 1 blend))))
    (dot hash (* (.zxzx blend) (.wwyy blend)))))

(defun value ((point :vec2))
  (value point (lambda ((x :vec2)) (hash:fast32 x))))

;;; 2D Value noise with derivatives

(defun value/derivs ((point :vec2)
                     (hash-fn (function (:vec2) :vec4)))
  (let* ((cell (floor point))
         (vec (- point cell))
         (hash (funcall hash-fn cell))
         (blend (shape:quintic-curve/interpolate-derivative vec))
         (out (mix (.xyxz hash) (.zwyw hash) (.yyxx blend))))
    (+ (vec3 (.x out) 0 0)
       (* (- (.yyw out) (.xxz out)) (.xzw blend)))))

(defun value/derivs ((point :vec2))
  (value/derivs point (lambda ((x :vec2)) (hash:fast32 x))))

;;; 3D Value noise

(defun value ((point :vec3)
              (hash-fn (function (:vec3) (:vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (low-z high-z (funcall hash-fn cell))
           (blend (shape:quintic-curve vec))
           (out (mix low-z high-z (.z blend)))
           (blend (vec4 (.xy blend) (- 1 (.xy blend)))))
    (dot out (* (.zxzx blend) (.wwyy blend)))))

(defun value ((point :vec3))
  (value point (lambda ((x :vec3)) (hash:fast32 x))))

;;; 3D Value noise with derivatives

(defun value/derivs ((point :vec3)
                     (hash-fn (function (:vec3) (:vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (low-z high-z (funcall hash-fn cell))
           (blend (shape:quintic-curve vec))
           (temp1 (mix low-z high-z (.z blend)))
           (temp1 (mix (.xyxz temp1) (.zwyw temp1) (.yyxx blend)))
           (temp2 (mix (vec4 (.xy low-z) (.xy high-z))
                       (vec4 (.zw low-z) (.zw high-z))
                       (.y blend)))
           (temp2 (mix (.xz temp2) (.yw temp2) (.x blend))))
    (+ (vec4 (.x temp1) 0 0 0)
       (* (- (vec4 (.yyw temp1) (.y temp2)) (vec4 (.xxz temp1) (.x temp2)))
          (vec4 (.x blend) (shape:quintic-curve/derivative vec))))))

(defun value/derivs ((point :vec3))
  (value/derivs point (lambda ((x :vec3)) (hash:fast32 x))))

;;; 4D Value noise

(defun value ((point :vec4)
              (hash-fn (function (:vec4) (:vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (z0w0 z1w0 z0w1 z1w1 (funcall hash-fn cell))
           (blend (shape:quintic-curve vec))
           (temp (+ z0w0 (* (- z0w1 z0w0) (.w blend))))
           (temp (+ temp (* (- (+ z1w0 (* (- z1w1 z1w0) (.w blend))) temp)
                            (.z blend))))
           (blend (vec4 (.xy blend) (- 1 (.xy blend)))))
    (dot temp (* (.zxzx blend) (.wwyy blend)))))

(defun value ((point :vec4))
  (value point (lambda ((x :vec4)) (hash:fast32-2 x))))

;;; 2D Value Hermite noise

(defun value-hermite ((point :vec2)
                      (value-scale :float)
                      (gradient-scale :float)
                      (normalization-value :float)
                      (hash-fn
                       (function (:vec2)
                        (:vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x hash-y hash-z (funcall hash-fn cell))
           (hash-x (* (- hash-z 0.5) value-scale))
           (hash-y (* (- hash-x 0.5 +epsilon+) gradient-scale))
           (hash-z (* (- hash-y 0.5 +epsilon+) gradient-scale))
           (out (shape:quintic-hermite
                 (.y vec)
                 (vec4 (.xy hash-x) (.xy hash-y))
                 (vec4 (.zw hash-x) (.zw hash-y))
                 (vec4 (.xy hash-z) 0 0)
                 (vec4 (.zw hash-z) 0 0)))
           (out (* (shape:quintic-hermite
                    (.x vec) (.x out) (.y out) (.z out) (.w out))
                   normalization-value)))
    (map-domain out -1 1 0 1)))

(defun value-hermite ((point :vec2)
                      (value-scale :float)
                      (gradient-scale :float)
                      (normalization-value :float))
  (value-hermite point value-scale gradient-scale normalization-value
                 (lambda ((x :vec2)) (hash:fast32/3-per-corner x))))

;;; 3D Value Hermite noise

(defun value-hermite ((point :vec3)
                      (value-scale :float)
                      (gradient-scale :float)
                      (normalization-value :float)
                      (hash-fn
                       (function (:vec3)
                        (:vec4 :vec4 :vec4 :vec4
                         :vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x0 hash-y0 hash-z0 hash-w0 hash-x1 hash-y1 hash-z1 hash-w1
                    (funcall hash-fn cell))
           (hash-x0 (* (- hash-x0 0.5) value-scale))
           (hash-y0 (* (- hash-y0 0.5 +epsilon+) gradient-scale))
           (hash-z0 (* (- hash-z0 0.5 +epsilon+) gradient-scale))
           (hash-w0 (* (- hash-w0 0.5 +epsilon+) gradient-scale))
           (hash-x1 (* (- hash-x1 0.5) value-scale))
           (hash-y1 (* (- hash-y1 0.5 +epsilon+) gradient-scale))
           (hash-z1 (* (- hash-z1 0.5 +epsilon+) gradient-scale))
           (hash-w1 (* (- hash-w1 0.5 +epsilon+) gradient-scale))
           (ival igrad-x igrad-y (shape:quintic-hermite
                                  (.z vec) hash-x0 hash-x1 hash-y0 hash-y1
                                  hash-z0 hash-z1 hash-w0 hash-w1))
           (out (shape:quintic-hermite
                 (.y vec)
                 (vec4 (.xy ival) (.xy igrad-x))
                 (vec4 (.zw ival) (.zw igrad-x))
                 (vec4 (.xy igrad-y) 0 0)
                 (vec4 (.zw igrad-y) 0 0)))
           (out (* (shape:quintic-hermite
                    (.x vec) (.x out) (.y out) (.z out) (.w out))
                   normalization-value)))
    (map-domain out -1 1 0 1)))

(defun value-hermite ((point :vec3)
                      (value-scale :float)
                      (gradient-scale :float)
                      (normalization-value :float))
  (value-hermite point value-scale gradient-scale normalization-value
                 (lambda ((x :vec3)) (hash:fast32/4-per-corner x))))

;;; 2D Value Perlin noise

(defun value-perlin ((point :vec2)
                     (blend-value :float)
                     (hash-fn (function (:vec2) (:vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vecs (- (.xyxy point) (vec4 cell (1+ cell))))
           (hash hash-x hash-y (funcall hash-fn cell))
           (grad-x (- hash-x 0.5 +epsilon+))
           (grad-y (- hash-y 0.5 +epsilon+))
           (grad-results (mix (1- (* hash 2))
                              (* (inversesqrt (+ (* grad-x grad-x)
                                                 (* grad-y grad-y)))
                                 (+ (* grad-x (.xzxz vecs))
                                    (* grad-y (.yyww vecs)))
                                 1.4142135)
                              blend-value))
           (blend (shape:quintic-curve (.xy vecs)))
           (blend (vec4 blend (- 1 blend)))
           (out (dot grad-results (* (.zxzx blend) (.wwyy blend)))))
    (map-domain out -1 1 0 1)))

(defun value-perlin ((point :vec2)
                     (blend-value :float))
  (value-perlin point blend-value (lambda ((x :vec2))
                                    (hash:fast32/3-per-corner x))))

;;; 3D Value Perlin noise

(defun value-perlin ((point :vec3)
                     (blend-value :float)
                     (hash-fn
                      (function (:vec3)
                       (:vec4 :vec4 :vec4 :vec4
                        :vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (vec-1 (1- vec))
           (hash0 hash-x0 hash-y0 hash-z0 hash1 hash-x1 hash-y1 hash-z1
                  (funcall hash-fn cell))
           (grad-x0 (- hash-x0 0.5 +epsilon+))
           (grad-y0 (- hash-y0 0.5 +epsilon+))
           (grad-z0 (- hash-z0 0.5 +epsilon+))
           (grad-x1 (- hash-x1 0.5 +epsilon+))
           (grad-y1 (- hash-y1 0.5 +epsilon+))
           (grad-z1 (- hash-z1 0.5 +epsilon+))
           (temp1 (mix (1- (* hash0 2))
                       (* (inversesqrt (+ (* grad-x0 grad-x0)
                                          (* grad-y0 grad-y0)
                                          (* grad-z0 grad-z0)))
                          (+ (* (.xyxy (vec2 (.x vec) (.x vec-1))) grad-x0)
                             (* (.xxyy (vec2 (.y vec) (.y vec-1))) grad-y0)
                             (* (.z vec) grad-z0))
                          1.1547005)
                       blend-value))
           (temp2 (mix (1- (* hash1 2))
                       (* (inversesqrt (+ (* grad-x1 grad-x1)
                                          (* grad-y1 grad-y1)
                                          (* grad-z1 grad-z1)))
                          (+ (* (.xyxy (vec2 (.x vec) (.x vec-1))) grad-x1)
                             (* (.xxyy (vec2 (.y vec) (.y vec-1))) grad-y1)
                             (* (.z vec-1) grad-z1))
                          1.1547005)
                       blend-value))
           (blend (shape:quintic-curve vec))
           (out (mix temp1 temp2 (.z blend)))
           (blend (vec4 (.xy blend) (- 1 (.xy blend))))
           (out (dot out (* (.zxzx blend) (.wwyy blend)))))
    (map-domain out -1 1 0 1)))

(defun value-perlin ((point :vec3)
                     (blend-value :float))
  (value-perlin point blend-value (lambda ((x :vec3))
                                    (hash:fast32/4-per-corner x))))
