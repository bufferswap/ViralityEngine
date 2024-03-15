(in-package #:vumbra.hashing)

;;;; Hashing functions
;;;; Blum Blum Shub
;;;; Marc Olano
;;;; http://www.cs.umbc.edu/~olano/papers/mNoise.pdf

(defun blum-blum-shub/coord-prepare ((x :vec4))
  (- x (* (floor (* x (/ 61.0))) 61.0)))

(defun blum-blum-shub/permute-and-resolve ((x :vec4))
  (fract (* x x (/ 61.0))))

(defun blum-blum-shub/permute ((x :vec4))
  (* (blum-blum-shub/permute-and-resolve x) 61.0))

(defun blum-blum-shub ((grid-cell :vec2))
  (let* ((hash-coord (blum-blum-shub/coord-prepare
                      (vec4 grid-cell (1+ grid-cell))))
         (hash (blum-blum-shub/permute (* (.xzxz hash-coord) 7.0))))
    (blum-blum-shub/permute-and-resolve (+ hash (.yyww hash-coord)))))

(defun blum-blum-shub/hq ((grid-cell :vec2))
  (let* ((hash-coord (blum-blum-shub/coord-prepare
                      (vec4 grid-cell (1+ grid-cell))))
         (hash (blum-blum-shub/permute (* (.xzxz hash-coord) 7.0))))
    (blum-blum-shub/permute-and-resolve
     (+ (blum-blum-shub/permute (+ hash (.yyww hash-coord)))
        (.xzxz hash-coord)))))

(defun blum-blum-shub ((grid-cell :vec3))
  (decf (.xyz grid-cell) (* (floor (* (.xyz grid-cell) (/ 60.0))) 60.0))
  (let* ((grid-cell-inc1 (* (step grid-cell (vec3 58.5)) (1+ grid-cell)))
         (p (blum-blum-shub/permute
             (+ (blum-blum-shub/permute
                 (.xyxy (vec2 (.x grid-cell) (.x grid-cell-inc1))))
                (.xxyy (vec2 (.y grid-cell) (.y grid-cell-inc1))))))
         (low-z (blum-blum-shub/permute-and-resolve (+ p (.z grid-cell))))
         (high-z (blum-blum-shub/permute-and-resolve
                  (+ p (.z grid-cell-inc1)))))
    (values low-z high-z)))
