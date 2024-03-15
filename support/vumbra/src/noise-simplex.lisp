(in-package #:vumbra.noise)

;;;; Simplex noise

(defconstant +simplex-2d/skew-factor+ (* 0.5 (1- (sqrt 3))))

(defconstant +simplex-2d/unskew-factor+ (/ (- 3 (sqrt 3)) 6))

(defconstant +simplex-2d/triangle-height+ (sqrt 0.5))

(defconstant +simplex-2d/inverse-triangle-height+ (sqrt (/ 0.5)))

(defconstant +simplex-2d/inverse-triangle-half-edge-length+
  (/ (sqrt 0.75) (sqrt 0.125)))

(defconstant +simplex-2d/norm-factor+
  (/ (* 0.4082483 (expt (- 0.5 (expt 0.4082483 2)) 4) 2)))

(defconstant +simplex-3d/skew-factor+ (/ 3.0))

(defconstant +simplex-3d/unskew-factor+ (/ 6.0))

(defconstant +simplex-3d/pyramid-height+ (sqrt 0.5))

(defconstant +simplex-3d/inverse-pyramid-height+ (sqrt (/ 0.5)))

(defconstant +simplex-3d/inverse-triangle-half-edge-length+ (/ 2 (sqrt 0.75)))

(defconstant +simplex-3d/norm-factor+
  (/ (* 0.4330127 (expt (- 0.5 (expt 0.4330127 2)) 3) 2)))

(defun simplex/get-corner-vectors ((point :vec3))
  (let* ((point (* point +simplex-3d/pyramid-height+))
         (cell1 (floor (+ point (dot point (vec3 +simplex-3d/skew-factor+)))))
         (x0 (+ (- point cell1) (dot cell1 (vec3 +simplex-3d/unskew-factor+))))
         (g (step (.yzx x0) (.xyz x0)))
         (l (- 1 g))
         (cell2 (min (.xyz g) (.zxy l)))
         (cell3 (max (.xyz g) (.zxy l)))
         (x1 (+ (- x0 cell2) +simplex-3d/unskew-factor+))
         (x2 (+ (- x0 cell3) +simplex-3d/skew-factor+))
         (x3 (- x0 0.5))
         (v1234-x (vec4 (.x x0) (.x x1) (.x x2) (.x x3)))
         (v1234-y (vec4 (.y x0) (.y x1) (.y x2) (.y x3)))
         (v1234-z (vec4 (.z x0) (.z x1) (.z x2) (.z x3))))
    (values cell1 cell2 cell3 v1234-x v1234-y v1234-z)))

(defun simplex/get-surflet-weights ((v1234-x :vec4)
                                    (v1234-y :vec4)
                                    (v1234-z :vec4))
  (let* ((surflet-weights (+ (* v1234-x v1234-x)
                             (* v1234-y v1234-y)
                             (* v1234-z v1234-z)))
         (surflet-weights (max (- 0.5 surflet-weights) 0)))
    (* surflet-weights surflet-weights surflet-weights)))

;;; 2D Simplex Perlin noise

(defun simplex-perlin ((point :vec2)
                       (hash-fn (function (:vec2) (:vec4 :vec4))))
  (mvlet* ((simplex-points (vec3 (- 1 +simplex-2d/unskew-factor+)
                                 (- +simplex-2d/unskew-factor+)
                                 (- 1 (* 2 +simplex-2d/unskew-factor+))))
           (point (* point +simplex-2d/triangle-height+))
           (cell (floor (+ point (dot point (vec2 +simplex-2d/skew-factor+)))))
           (hash-x hash-y (funcall hash-fn cell))
           (v0 (- cell (dot cell (vec2 +simplex-2d/unskew-factor+)) point))
           (v1pos-v1hash (if (< (.x v0) (.y v0))
                             (vec4 (.xy simplex-points)
                                   (.y hash-x)
                                   (.y hash-y))
                             (vec4 (.yx simplex-points)
                                   (.z hash-x)
                                   (.z hash-y))))
           (v12 (+ (vec4 (.xy v1pos-v1hash) (.zz simplex-points)) (.xyxy v0)))
           (grad-x (- (vec3 (.x hash-x) (.z v1pos-v1hash) (.w hash-x)) 0.5
                      +epsilon+))
           (grad-y (- (vec3 (.x hash-y) (.w v1pos-v1hash) (.w hash-y)) 0.5
                      +epsilon+))
           (m (expt (max (- 0.5 (+ (* (vec3 (.x v0) (.xz v12))
                                      (vec3 (.x v0) (.xz v12)))
                                   (* (vec3 (.y v0) (.yw v12))
                                      (vec3 (.y v0) (.yw v12)))))
                         0)
                    (vec3 4)))
           (out (* (dot m (* (inversesqrt (+ (* grad-x grad-x)
                                             (* grad-y grad-y)))
                             (+ (* grad-x (vec3 (.x v0) (.xz v12)))
                                (* grad-y (vec3 (.y v0) (.yw v12))))))
                   +simplex-2d/norm-factor+)))
    (map-domain out -1 1 0 1)))

(defun simplex-perlin ((point :vec2))
  (simplex-perlin point (lambda ((x :vec2)) (hash:fast32/2-per-corner x))))

;;; 2D Simplex Perlin noise with derivatives

(defun simplex-perlin/derivs ((point :vec2)
                              (hash-fn
                               (function (:vec2)
                                (:vec4 :vec4))))
  (mvlet* ((simplex-points (vec3 (- 1 +simplex-2d/unskew-factor+)
                                 (- +simplex-2d/unskew-factor+)
                                 (- 1 (* 2 +simplex-2d/unskew-factor+))))
           (point (* point +simplex-2d/triangle-height+))
           (cell (floor (+ point (dot point (vec2 +simplex-2d/skew-factor+)))))
           (hash-x hash-y (funcall hash-fn cell))
           (v0 (- cell (dot cell (vec2 +simplex-2d/unskew-factor+)) point))
           (v1pos-v1hash (if (< (.x v0) (.y v0))
                             (vec4 (.xy simplex-points)
                                   (.y hash-x)
                                   (.y hash-y))
                             (vec4 (.yx simplex-points)
                                   (.z hash-x)
                                   (.z hash-y))))
           (v12 (+ (vec4 (.xy v1pos-v1hash) (.zz simplex-points)) (.xyxy v0)))
           (grad-x (- (vec3 (.x hash-x) (.z v1pos-v1hash) (.w hash-x)) 0.5
                      +epsilon+))
           (grad-y (- (vec3 (.x hash-y) (.w v1pos-v1hash) (.w hash-y)) 0.5
                      +epsilon+))
           (norm (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y))))
           (grad-x (* grad-x norm))
           (grad-y (* grad-y norm))
           (grad-results (+ (* grad-x (vec3 (.x v0) (.xz v12)))
                            (* grad-y (vec3 (.y v0) (.yw v12)))))
           (m (max (- 0.5 (+ (* (vec3 (.x v0) (.xz v12))
                                (vec3 (.x v0) (.xz v12)))
                             (* (vec3 (.y v0) (.yw v12))
                                (vec3 (.y v0) (.yw v12)))))
                   0))
           (m2 (* m m))
           (m4 (* m2 m2))
           (temp (* 8 m2 m grad-results))
           (noise (map-domain
                   (dot m4 grad-results) -0.010080204 0.010080204 0 1))
           (derivs (* (vec2 (- (dot temp (vec3 (.x v0) (.xz v12)))
                               (dot m4 grad-x))
                            (- (dot temp (vec3 (.y v0) (.yw v12)))
                               (dot m4 grad-x)))
                      49.60217)))
    (vec3 noise derivs)))

(defun simplex-perlin/derivs ((point :vec2))
  (simplex-perlin/derivs point
                         (lambda ((x :vec2))
                           (hash:fast32/2-per-corner x))))

;;; 3D Simplex Perlin noise

(defun simplex-perlin ((point :vec3)
                       (hash-fn
                        (function (:vec3 :vec3 :vec3)
                         (:vec4 :vec4 :vec4))))
  (mvlet* ((cell1 cell2 cell3 corners-x corners-y corners-z
                  (simplex/get-corner-vectors point))
           (hash0 hash1 hash2 (funcall hash-fn cell1 cell2 cell3))
           (hash0 (- hash0 0.5 +epsilon+))
           (hash1 (- hash1 0.5 +epsilon+))
           (hash2 (- hash2 0.5 +epsilon+))
           (weights (simplex/get-surflet-weights corners-x corners-y corners-z))
           (out (* (dot weights
                        (* (inversesqrt (+ (* hash0 hash0)
                                           (* hash1 hash1)
                                           (* hash2 hash2)))
                           (+ (* hash0 corners-x)
                              (* hash1 corners-y)
                              (* hash2 corners-z))))
                   +simplex-3d/norm-factor+)))
    (map-domain out -1 1 0 1)))

(defun simplex-perlin ((point :vec3))
  (simplex-perlin point (lambda ((x :vec3) (y :vec3) (z :vec3))
                          (hash:fast32/3-per-corner x y z))))

;;; 3D Simplex Perlin noise with derivatives

(defun simplex-perlin/derivs ((point :vec3)
                              (hash-fn (function
                                        (:vec3 :vec3 :vec3)
                                        (:vec4 :vec4 :vec4))))
  (mvlet* ((cell1 cell2 cell3 corners-x corners-y corners-z
                  (simplex/get-corner-vectors point))
           (hash0 hash1 hash2 (funcall hash-fn cell1 cell2 cell3))
           (hash0 (- hash0 0.5 +epsilon+))
           (hash1 (- hash1 0.5 +epsilon+))
           (hash2 (- hash2 0.5 +epsilon+))
           (norm (inversesqrt (+ (* hash0 hash0)
                                 (* hash1 hash1)
                                 (* hash2 hash2))))
           (hash0 (* hash0 norm))
           (hash1 (* hash1 norm))
           (hash2 (* hash2 norm))
           (grad-results (+ (* hash0 corners-x)
                            (* hash1 corners-y)
                            (* hash2 corners-z)))
           (m (+ (* corners-x corners-x)
                 (* corners-y corners-y)
                 (* corners-z corners-z)))
           (m (max (- 0.5 m) 0))
           (m2 (* m m))
           (m3 (* m m2))
           (temp (* -6 m2 grad-results))
           (noise (map-domain
                   (dot m3 grad-results) -0.026428998 0.026428998 0 1))
           (derivs (* (vec3 (+ (dot temp corners-x) (dot m3 hash0))
                            (+ (dot temp corners-y) (dot m3 hash1))
                            (+ (dot temp corners-z) (dot m3 hash2)))
                      18.918613)))
    (vec4 noise derivs)))

(defun simplex-perlin/derivs ((point :vec3))
  (simplex-perlin/derivs point (lambda ((x :vec3) (y :vec3) (z :vec3))
                                 (hash:fast32/3-per-corner x y z))))

;;; 2D Simplex Cellular noise

(defun simplex-cellular ((point :vec2)
                         (hash-fn (function (:vec2) (:vec4 :vec4))))
  (mvlet* ((jitter-window (* 0.105662435 +simplex-2d/inverse-triangle-height+))
           (simplex-points (* (vec3 (- 1 +simplex-2d/unskew-factor+)
                                    (- +simplex-2d/unskew-factor+)
                                    (- 1 (* 2 +simplex-2d/unskew-factor+)))
                              +simplex-2d/inverse-triangle-height+))
           (point (* point +simplex-2d/triangle-height+))
           (cell (floor (+ point (dot point (vec2 +simplex-2d/skew-factor+)))))
           (p0 (* (- cell (dot cell (vec2 +simplex-2d/unskew-factor+)) point)
                  +simplex-2d/inverse-triangle-height+))
           (hash-x hash-y (funcall hash-fn cell))
           (grad-x (+ (* (cellular-weight-samples hash-x) jitter-window)
                      (.x p0)))
           (grad-x (vec4 (.x grad-x) (+ (.yzw grad-x) (.xyz simplex-points))))
           (grad-y (+ (* (cellular-weight-samples hash-y) jitter-window)
                      (.y p0)))
           (grad-y (vec4 (.x grad-y) (+ (.yzw grad-y) (.yxz simplex-points))))
           (dist-sq (+ (* grad-x grad-x) (* grad-y grad-y)))
           (temp (min (.xy dist-sq) (.zw dist-sq))))
    (min (.x temp) (.y temp))))

(defun simplex-cellular ((point :vec2))
  (simplex-cellular point
                    (lambda ((x :vec2))
                      (hash:fast32/2-per-corner x))))

;;; 3D Simplex Cellular noise

(defun simplex-cellular ((point :vec3)
                         (hash-fn
                          (function (:vec3 :vec3 :vec3)
                           (:vec4 :vec4 :vec4))))
  (mvlet* ((cell1 cell2 cell3 corners-x corners-y corners-z
                  (simplex/get-corner-vectors point))
           (hash-x hash-y hash-z (funcall hash-fn cell1 cell2 cell3))
           (jitter-window (* 0.059786577 +simplex-3d/inverse-pyramid-height+))
           (hash-x (* (cellular-weight-samples hash-x) jitter-window))
           (hash-y (* (cellular-weight-samples hash-y) jitter-window))
           (hash-z (* (cellular-weight-samples hash-z) jitter-window))
           (corners-x (+ (* corners-x (vec4 +simplex-3d/inverse-pyramid-height+))
                         hash-x))
           (corners-y (+ (* corners-y (vec4 +simplex-3d/inverse-pyramid-height+))
                         hash-y))
           (corners-z (+ (* corners-z (vec4 +simplex-3d/inverse-pyramid-height+))
                         hash-z))
           (dist-sq (+ (* corners-x corners-x)
                       (* corners-y corners-y)
                       (* corners-z corners-z))))
    (min (min (.x dist-sq) (.y dist-sq))
         (min (.z dist-sq) (.w dist-sq)))))

(defun simplex-cellular ((point :vec3))
  (simplex-cellular point
                    (lambda ((x :vec3) (y :vec3) (z :vec3))
                      (hash:fast32/3-per-corner x y z))))

;;; 2D Simplex Polka-dot noise

(defun simplex-polkadot ((point :vec2)
                         (radius :float)
                         (max-dimness :float)
                         (hash-fn (function (:vec2) :vec4)))
  (let* ((simplex-points (vec3 (- 1 +simplex-2d/unskew-factor+)
                               (- +simplex-2d/unskew-factor+)
                               (- 1 (* +simplex-2d/unskew-factor+ 2))))
         (point (* point +simplex-2d/triangle-height+))
         (cell (floor (+ point (dot point (vec2 +simplex-2d/skew-factor+)))))
         (v0 (- cell (dot cell (vec2 +simplex-2d/unskew-factor+)) point))
         (hash (funcall hash-fn cell))
         (radius (/ +simplex-2d/inverse-triangle-half-edge-length+ radius))
         (corners-x (* (+ (vec4 0 (.xyz simplex-points)) (.x v0)) radius))
         (corners-y (* (+ (vec4 0 (.yxz simplex-points)) (.y v0)) radius))
         (point-distance (max (vec4 0) (- 1 (+ (* corners-x corners-x)
                                               (* corners-y corners-y))))))
    (dot (- 1 (* hash max-dimness)) (expt point-distance (vec4 3)))))

(defun simplex-polkadot ((point :vec2)
                         (radius :float)
                         (max-dimness :float))
  (simplex-polkadot point radius max-dimness (lambda ((x :vec2))
                                               (hash:fast32 x))))

;;; 3D Simplex Polka-dot noise

(defun simplex-polkadot ((point :vec3)
                         (radius :float)
                         (max-dimness :float)
                         (hash-fn
                          (function (:vec3 :vec3 :vec3)
                           :vec4)))
  (mvlet* ((cell1 cell2 cell3 corners-x corners-y corners-z
                  (simplex/get-corner-vectors point))
           (hash (funcall hash-fn cell1 cell2 cell3))
           (radius (/ +simplex-3d/inverse-triangle-half-edge-length+ radius))
           (vx (* corners-x radius))
           (vy (* corners-y radius))
           (vz (* corners-z radius))
           (point-distance (max (vec4 0) (- 1 (+ (* vx vx)
                                                 (* vy vy)
                                                 (* vz vz))))))
    (setf point-distance (* point-distance point-distance point-distance))
    (dot (- 1 (* hash max-dimness)) point-distance)))

(defun simplex-polkadot ((point :vec3)
                         (radius :float)
                         (max-dimness :float))
  (simplex-polkadot point radius max-dimness
                    (lambda ((x :vec3) (y :vec3) (z :vec3))
                      (hash:fast32 x y z))))
