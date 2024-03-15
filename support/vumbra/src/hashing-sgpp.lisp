(in-package #:vumbra.hashing)

;;;; Hashing functions
;;;; Permutation polynomial
;;;; Stefan Gustavson and Ian McEwan
;;;; http://github.com/ashima/webgl-noise
;;;; http://www.itn.liu.se/~stegu/GLSL-cellular

(defun sgpp/coord-prepare ((x :vec3))
  (- x (* (floor (* x (/ 289.0))) 289.0)))

(defun sgpp/coord-prepare ((x :vec4))
  (- x (* (floor (* x (/ 289.0))) 289.0)))

(defun sgpp/permute ((x :vec4))
  (* (fract (* x (+ (* 0.11764706 x) (/ 289.0)))) 289.0))

(defun sgpp/resolve ((x :vec4))
  (fract (* x 0.024305556)))

(defun sgpp ((grid-cell :vec2))
  (let ((hash-coord (sgpp/coord-prepare (vec4 grid-cell (1+ grid-cell)))))
    (sgpp/resolve
     (sgpp/permute
      (+ (sgpp/permute (.xzxz hash-coord))
         (.yyww hash-coord))))))

(defun sgpp/2-per-corner ((grid-cell :vec2))
  (let* ((hash-coord (sgpp/coord-prepare
                      (vec4 (.xy grid-cell) (1+ (.xy grid-cell)))))
         (hash0 (sgpp/permute
                 (+ (sgpp/permute (.xzxz hash-coord)) (.yyww hash-coord)))))
    (values (sgpp/resolve hash0)
            (sgpp/resolve (sgpp/permute hash0)))))

(defun sgpp ((grid-cell :vec3))
  (let* ((grid-cell (sgpp/coord-prepare grid-cell))
         (grid-cell-inc1 (* (step grid-cell (vec3 287.5)) (1+ grid-cell)))
         (x (.xyxy (vec2 (.x grid-cell) (.x grid-cell-inc1))))
         (y (.xxyy (vec2 (.y grid-cell) (.y grid-cell-inc1))))
         (high-z (+ (sgpp/permute (+ (sgpp/permute x) y))))
         (low-z (sgpp/resolve (sgpp/permute (+ high-z (.z grid-cell))))))
    (setf high-z (sgpp/resolve (sgpp/permute (+ high-z (.z grid-cell-inc1)))))
    (values low-z high-z)))

(defun sgpp/3-per-corner ((grid-cell :vec3)
                          (v1-mask :vec3)
                          (v2-mask :vec3))
  (let* ((coords0 (- grid-cell (* (floor (* grid-cell (/ 289.0))) 289.0)))
         (coords3 (* (step coords0 (vec3 287.5)) (1+ coords0)))
         (coords1 (mix coords0 coords3 v1-mask))
         (coords2 (mix coords0 coords3 v2-mask))
         (hash1 (sgpp/permute
                 (sgpp/permute
                  (+ (sgpp/permute (vec4 (.x coords0)
                                         (.x coords1)
                                         (.x coords2)
                                         (.x coords3)))
                     (vec4 (.y coords0) (.y coords1) (.y coords2) (.y coords3))
                     (vec4 (.z coords0)
                           (.z coords1)
                           (.z coords2)
                           (.z coords3))))))
         (hash2 (sgpp/permute hash1)))
    (values (sgpp/resolve hash1)
            (sgpp/resolve hash2)
            (sgpp/resolve (sgpp/permute hash2)))))
