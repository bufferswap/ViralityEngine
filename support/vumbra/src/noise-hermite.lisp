(in-package #:vumbra.noise)

;;;; Hermite noise

;;; 2D Hermite noise

(defun hermite ((point :vec2)
                (hash-fn (function (:vec2) (:vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x hash-y (funcall hash-fn cell))
           (hash-x (- hash-x 0.5 +epsilon+))
           (hash-y (- hash-y 0.5 +epsilon+))
           (norm (inversesqrt (+ (* hash-x hash-x) (* hash-y hash-y))))
           (hash-x (* hash-x norm))
           (hash-y (* hash-y norm))
           (out (shape:quintic-hermite
                 (.y vec) (.xy hash-x) (.zw hash-x) (.xy hash-y) (.zw hash-y)))
           (out (* (shape:quintic-hermite
                    (.x vec) (.x out) (.y out) (.z out) (.w out))
                   2.2627418)))
    (map-domain out -1 1 0 1)))

(defun hermite ((point :vec2))
  (hermite point (lambda ((x :vec2)) (hash:fast32/2-per-corner x))))

;;; 2D Hermite noise with derivatives

(defun hermite/derivs ((point :vec2)
                       (hash-fn (function (:vec2) (:vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x hash-y (funcall hash-fn cell))
           (grad-x (- hash-x 0.5 +epsilon+))
           (grad-y (- hash-y 0.5 +epsilon+))
           (norm (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y))))
           (grad-x (* grad-x norm))
           (grad-y (* grad-y norm))
           (temp-x (shape:quintic-hermite
                    (.y vec) (.xy grad-x) (.zw grad-x) (.xy grad-y)
                    (.zw grad-y)))
           (temp-y (shape:quintic-hermite
                    (.x vec) (.xz grad-y) (.yw grad-y) (.xz grad-x)
                    (.yw grad-x)))
           (noise (shape:quintic-hermite
                   (.x vec) (.x temp-x) (.y temp-x) (.z temp-x) (.w temp-x)))
           (noise (map-domain noise -0.4419417 0.4419417 0 1))
           (derivs (* (vec2 (shape:quintic-hermite/derivative
                             (.x vec) (.x temp-x) (.y temp-x) (.z temp-x)
                             (.w temp-x))
                            (shape:quintic-hermite/derivative
                             (.y vec) (.x temp-y) (.y temp-y) (.z temp-y)
                             (.w temp-y)))
                      1.1313709)))
    (vec3 noise derivs)))

(defun hermite/derivs ((point :vec2))
  (hermite/derivs point
                  (lambda ((x :vec2))
                    (hash:fast32/2-per-corner x))))

;;; 3D Hermite noise

(defun hermite ((point :vec3)
                (hash-fn
                 (function (:vec3)
                  (:vec4 :vec4 :vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x0 hash-y0 hash-z0 hash-x1 hash-y1 hash-z1
                    (funcall hash-fn cell))
           (hash-x0 (- hash-x0 0.5 +epsilon+))
           (hash-y0 (- hash-y0 0.5 +epsilon+))
           (hash-z0 (- hash-z0 0.5 +epsilon+))
           (hash-x1 (- hash-x1 0.5 +epsilon+))
           (hash-y1 (- hash-y1 0.5 +epsilon+))
           (hash-z1 (- hash-z1 0.5 +epsilon+))
           (norm0 (inversesqrt (+ (* hash-x0 hash-x0) (* hash-y0 hash-y0)
                                  (* hash-z0 hash-z0))))
           (norm1 (inversesqrt (+ (* hash-x1 hash-x1) (* hash-y1 hash-y1)
                                  (* hash-z1 hash-z1))))
           (grad-x0 (* hash-x0 norm0))
           (grad-y0 (* hash-y0 norm0))
           (grad-z0 (* hash-z0 norm0))
           (grad-x1 (* hash-x1 norm1))
           (grad-y1 (* hash-y1 norm1))
           (grad-z1 (* hash-z1 norm1))
           (ival igrad-x igrad-y (shape:quintic-hermite
                                  (.z vec) grad-x0 grad-x1 grad-y0 grad-y1
                                  grad-z0 grad-z1))
           (out (shape:quintic-hermite
                 (.y vec)
                 (vec4 (.xy ival) (.xy igrad-x))
                 (vec4 (.zw ival) (.zw igrad-x))
                 (vec4 (.xy igrad-y) 0 0)
                 (vec4 (.zw igrad-y) 0 0)))
           (out (* (shape:quintic-hermite
                    (.x vec) (.x out) (.y out) (.z out) (.w out))
                   1.8475208)))
    (map-domain out -1 1 0 1)))

(defun hermite ((point :vec3))
  (hermite point (lambda ((x :vec3)) (hash:fast32/3-per-corner x))))

;;; 3D Hermite noise with derivatives

(defun hermite/derivs ((point :vec3)
                       (hash-fn
                        (function
                         (:vec3)
                         (:vec4 :vec4 :vec4 :vec4 :vec4 :vec4))))
  (mvlet* ((cell (floor point))
           (vec (- point cell))
           (hash-x0 hash-y0 hash-z0 hash-x1 hash-y1 hash-z1
                    (funcall hash-fn cell))
           (grad-x0 (- hash-x0 0.5 +epsilon+))
           (grad-y0 (- hash-y0 0.5 +epsilon+))
           (grad-z0 (- hash-z0 0.5 +epsilon+))
           (grad-x1 (- hash-x1 0.5 +epsilon+))
           (grad-y1 (- hash-y1 0.5 +epsilon+))
           (grad-z1 (- hash-z1 0.5 +epsilon+))
           (norm0 (inversesqrt (+ (* grad-x0 grad-x0) (* grad-y0 grad-y0)
                                  (* grad-z0 grad-z0))))
           (norm1 (inversesqrt (+ (* grad-x1 grad-x1) (* grad-y1 grad-y1)
                                  (* grad-z1 grad-z1))))
           (grad-x0 (* grad-x0 norm0))
           (grad-y0 (* grad-y0 norm0))
           (grad-z0 (* grad-z0 norm0))
           (grad-x1 (* grad-x1 norm1))
           (grad-y1 (* grad-y1 norm1))
           (grad-z1 (* grad-z1 norm1))
           (ival-z igrad-xz igrad-yz (shape:quintic-hermite
                                      (.z vec) grad-x0 grad-x1 grad-y0 grad-y1
                                      grad-z0 grad-z1))
           (ival-y igrad-xy igrad-zy (shape:quintic-hermite
                                      (.y vec)
                                      (vec4 (.xy grad-x0) (.xy grad-x1))
                                      (vec4 (.zw grad-x0) (.zw grad-x1))
                                      (vec4 (.xy grad-z0) (.xy grad-z1))
                                      (vec4 (.zw grad-z0) (.zw grad-z1))
                                      (vec4 (.xy grad-y0) (.xy grad-y1))
                                      (vec4 (.zw grad-y0) (.zw grad-y1))))
           (temp-x (shape:quintic-hermite
                    (.y vec)
                    (vec4 (.xy ival-z) (.xy igrad-xz))
                    (vec4 (.zw ival-z) (.zw igrad-xz))
                    (vec4 (.xy igrad-yz) 0 0)
                    (vec4 (.zw igrad-yz) 0 0)))
           (temp-y (shape:quintic-hermite
                    (.x vec)
                    (vec4 (.xz ival-z) (.xz igrad-yz))
                    (vec4 (.yw ival-z) (.yw igrad-yz))
                    (vec4 (.xz igrad-xz) 0 0)
                    (vec4 (.yw igrad-xz) 0 0)))
           (temp-z (shape:quintic-hermite
                    (.x vec)
                    (vec4 (.xz ival-y) (.xz igrad-zy))
                    (vec4 (.yw ival-y) (.yw igrad-zy))
                    (vec4 (.xz igrad-xy) 0 0)
                    (vec4 (.yw igrad-xy) 0 0)))
           (noise (shape:quintic-hermite
                   (.x vec) (.x temp-x) (.y temp-x) (.z temp-x) (.w temp-x)))
           (noise (map-domain noise -0.5412659 0.5412659 0 1))
           (derivs (* (vec3 (shape:quintic-hermite/derivative
                             (.x vec) (.x temp-x) (.y temp-x) (.z temp-x)
                             (.w temp-x))
                            (shape:quintic-hermite/derivative
                             (.y vec) (.x temp-y) (.y temp-y) (.z temp-y)
                             (.w temp-y))
                            (shape:quintic-hermite/derivative
                             (.z vec) (.x temp-z) (.y temp-z) (.z temp-z)
                             (.w temp-z)))
                      0.92376035)))
    (vec4 noise derivs)))

(defun hermite/derivs ((point :vec3))
  (hermite/derivs point
                  (lambda ((x :vec3))
                    (hash:fast32/3-per-corner x))))
