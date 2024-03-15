(in-package #:vorigin.dmat4)

(u:fn-> = (mat mat &key (:rel u:f64) (:abs u:f64)) boolean)
(declaim (inline =))
(defun = (mat1 mat2 &key (rel 1d-7) (abs rel))
  (com:cwcmp 16 (mat1 mat2) (com:= mat1 mat2 rel abs)))

(u:fn-> zero! (mat) mat)
(declaim (inline zero!))
(defun zero! (mat)
  (declare (optimize speed))
  (com:cwset 16 mat nil 0d0)
  mat)

(u:fn-> zero () mat)
(declaim (inline zero))
(defun zero ()
  (declare (optimize speed))
  (mat 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0 0d0))

(u:fn-> zero-p (mat) boolean)
(declaim (inline zero-p))
(defun zero-p (mat)
  (declare (optimize speed))
  (= mat +zero+))

(u:fn-> id! (mat) mat)
(declaim (inline id!))
(defun id! (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (psetf m00 1d0 m01 0d0 m02 0d0 m03 0d0
           m10 0d0 m11 1d0 m12 0d0 m13 0d0
           m20 0d0 m21 0d0 m22 1d0 m23 0d0
           m30 0d0 m31 0d0 m32 0d0 m33 1d0))
  mat)

(u:fn-> id () mat)
(declaim (inline id))
(defun id ()
  (declare (optimize speed))
  (id! (zero)))

(u:fn-> id-p (mat) boolean)
(declaim (inline id-p))
(defun id-p (mat)
  (declare (optimize speed))
  (= mat +id+))

(u:fn-> random! (mat u:f64 u:f64) mat)
(declaim (inline random!))
(defun random! (out min max)
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (com:cwset 16 out nil (cl:+ min (cl:random diff))))
  out)

(u:fn-> random (u:f64 u:f64) mat)
(declaim (inline random))
(defun random (min max)
  (declare (optimize speed))
  (random! (zero) min max))

(u:fn-> copy! (mat mat) mat)
(declaim (inline copy!))
(defun copy! (out mat)
  (declare (optimize speed))
  (com:cwset 16 out mat mat)
  out)

(u:fn-> copy (mat) mat)
(declaim (inline copy))
(defun copy (mat)
  (declare (optimize speed))
  (copy! (zero) mat))

(u:fn-> clamp! (mat mat mat mat) mat)
(declaim (inline clamp!))
(defun clamp! (out mat min max)
  "Modify matrix OUT to have its components represent the components of matrix
MAT, bounded by the components of matrices MIN and MAX."
  (declare (optimize speed))
  (com:cwset 16 out (mat min max) (u:clamp mat min max))
  out)

(u:fn-> clamp (mat mat mat) mat)
(declaim (inline clamp))
(defun clamp (mat min max)
  "Construct a fresh matrix that has the components of matrix MAT bounded by the
  components of matrices MIN and MAX."
  (declare (optimize speed))
  (clamp! (zero) mat min max))

(u:fn-> clamp-range! (mat mat u:f64 u:f64) mat)
(declaim (inline clamp-range!))
(defun clamp-range! (out mat min max)
  (declare (optimize speed))
  (com:cwset 16 out mat (u:clamp mat min max))
  out)

(u:fn-> clamp-range (mat u:f64 u:f64) mat)
(declaim (inline clamp-range))
(defun clamp-range (mat min max)
  (declare (optimize speed))
  (clamp-range! (zero) mat min max))

(u:fn-> +! (mat mat mat) mat)
(declaim (inline +!))
(defun +! (out mat1 mat2)
  (declare (optimize speed))
  (com:cwset 16 out (mat1 mat2) (cl:+ mat1 mat2))
  out)

(u:fn-> + (mat mat) mat)
(declaim (inline +))
(defun + (mat1 mat2)
  (declare (optimize speed))
  (+! (zero) mat1 mat2))

(u:fn-> -! (mat mat mat) mat)
(declaim (inline -!))
(defun -! (out mat1 mat2)
  (declare (optimize speed))
  (com:cwset 16 out (mat1 mat2) (cl:- mat1 mat2))
  out)

(u:fn-> - (mat mat) mat)
(declaim (inline -))
(defun - (mat1 mat2)
  (declare (optimize speed))
  (-! (zero) mat1 mat2))

(u:fn-> *! (mat mat mat) mat)
(declaim (inline *!))
(defun *! (out mat1 mat2)
  (declare (optimize speed))
  (with-components ((o out) (a mat1) (b mat2))
    (psetf o00 (cl:+ (cl:* a00 b00) (cl:* a01 b10) (cl:* a02 b20) (cl:* a03 b30))
           o10 (cl:+ (cl:* a10 b00) (cl:* a11 b10) (cl:* a12 b20) (cl:* a13 b30))
           o20 (cl:+ (cl:* a20 b00) (cl:* a21 b10) (cl:* a22 b20) (cl:* a23 b30))
           o30 (cl:+ (cl:* a30 b00) (cl:* a31 b10) (cl:* a32 b20) (cl:* a33 b30))
           o01 (cl:+ (cl:* a00 b01) (cl:* a01 b11) (cl:* a02 b21) (cl:* a03 b31))
           o11 (cl:+ (cl:* a10 b01) (cl:* a11 b11) (cl:* a12 b21) (cl:* a13 b31))
           o21 (cl:+ (cl:* a20 b01) (cl:* a21 b11) (cl:* a22 b21) (cl:* a23 b31))
           o31 (cl:+ (cl:* a30 b01) (cl:* a31 b11) (cl:* a32 b21) (cl:* a33 b31))
           o02 (cl:+ (cl:* a00 b02) (cl:* a01 b12) (cl:* a02 b22) (cl:* a03 b32))
           o12 (cl:+ (cl:* a10 b02) (cl:* a11 b12) (cl:* a12 b22) (cl:* a13 b32))
           o22 (cl:+ (cl:* a20 b02) (cl:* a21 b12) (cl:* a22 b22) (cl:* a23 b32))
           o32 (cl:+ (cl:* a30 b02) (cl:* a31 b12) (cl:* a32 b22) (cl:* a33 b32))
           o03 (cl:+ (cl:* a00 b03) (cl:* a01 b13) (cl:* a02 b23) (cl:* a03 b33))
           o13 (cl:+ (cl:* a10 b03) (cl:* a11 b13) (cl:* a12 b23) (cl:* a13 b33))
           o23 (cl:+ (cl:* a20 b03) (cl:* a21 b13) (cl:* a22 b23) (cl:* a23 b33))
           o33 (cl:+ (cl:* a30 b03) (cl:* a31 b13) (cl:* a32 b23) (cl:* a33 b33))))
  out)

(u:fn-> * (mat mat) mat)
(declaim (inline *))
(defun * (mat1 mat2)
  (declare (optimize speed))
  (*! (zero) mat1 mat2))

(u:fn-> copy-rotation! (mat mat) mat)
(declaim (inline copy-rotation!))
(defun copy-rotation! (out mat)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (psetf o00 m00 o01 m01 o02 m02
           o10 m10 o11 m11 o12 m12
           o20 m20 o21 m21 o22 m22))
  out)

(u:fn-> copy-rotation (mat) mat)
(declaim (inline copy-rotation))
(defun copy-rotation (mat)
  (declare (optimize speed))
  (copy-rotation! (id) mat))

(u:fn-> rotation-to-mat3! (dm3:mat mat) dm3:mat)
(declaim (inline rotation-to-mat3!))
(defun rotation-to-mat3! (out mat)
  (declare (optimize speed))
  (dm3:with-components ((o out))
    (with-components ((m mat))
      (psetf o00 m00 o01 m01 o02 m02
             o10 m10 o11 m11 o12 m12
             o20 m20 o21 m21 o22 m22)))
  out)

(u:fn-> rotation-to-mat3 (mat) dm3:mat)
(declaim (inline rotation-to-mat3))
(defun rotation-to-mat3 (mat)
  (declare (optimize speed))
  (rotation-to-mat3! (dm3:id) mat))

(u:fn-> rotation-axis-to-vec3! (dv3:vec mat keyword) dv3:vec)
(declaim (inline rotation-axis-to-vec3!))
(defun rotation-axis-to-vec3! (out mat axis)
  (declare (optimize speed))
  (dv3:with-components ((v out))
    (with-components ((m mat))
      (ecase axis
        (:x (psetf vx m00 vy m10 vz m20))
        (:y (psetf vx m01 vy m11 vz m21))
        (:z (psetf vx m02 vy m12 vz m22)))))
  out)

(u:fn-> rotation-axis-to-vec3 (mat keyword) dv3:vec)
(declaim (inline rotation-axis-to-vec3))
(defun rotation-axis-to-vec3 (mat axis)
  (declare (optimize speed))
  (rotation-axis-to-vec3! (dv3:zero) mat axis))

(u:fn-> rotation-axis-from-vec3! (mat dv3:vec keyword) mat)
(declaim (inline rotation-axis-from-vec3!))
(defun rotation-axis-from-vec3! (out vec axis)
  (declare (optimize speed))
  (with-components ((o out))
    (dv3:with-components ((v vec))
      (ecase axis
        (:x (psetf o00 vx o10 vy o20 vz))
        (:y (psetf o01 vx o11 vy o21 vz))
        (:z (psetf o02 vx o12 vy o22 vz)))))
  out)

(u:fn-> rotation-axis-from-vec3 (mat dv3:vec keyword) mat)
(declaim (inline rotation-axis-from-vec3))
(defun rotation-axis-from-vec3 (mat vec axis)
  (declare (optimize speed))
  (rotation-axis-from-vec3! (copy mat) vec axis))

(u:fn-> rotation-x-from-angle! (mat u:f64) mat)
(declaim (inline rotation-x-from-angle!))
(defun rotation-x-from-angle! (out angle)
  (declare (optimize speed))
  (let ((s (sin angle))
        (c (cos angle)))
    (with-components ((o out))
      (psetf o00 1d0
             o10 0d0
             o20 0d0
             o30 0d0
             o01 0d0
             o11 c
             o21 s
             o31 0d0
             o02 0d0
             o12 (cl:- s)
             o22 c
             o32 0d0
             o03 0d0
             o13 0d0
             o23 0d0
             o33 1d0))
    out))

(u:fn-> rotation-x-from-angle (u:f64) mat)
(declaim (inline rotation-x-from-angle))
(defun rotation-x-from-angle (angle)
  (declare (optimize speed))
  (rotation-x-from-angle! (zero) angle))

(u:fn-> rotation-y-from-angle! (mat u:f64) mat)
(declaim (inline rotation-y-from-angle!))
(defun rotation-y-from-angle! (out angle)
  (declare (optimize speed))
  (let ((s (sin angle))
        (c (cos angle)))
    (with-components ((o out))
      (psetf o00 c
             o10 0d0
             o20 (cl:- s)
             o30 0d0
             o01 0d0
             o11 1d0
             o21 0d0
             o31 0d0
             o02 s
             o12 0d0
             o22 c
             o32 0d0
             o03 0d0
             o13 0d0
             o23 0d0
             o33 1d0))
    out))

(u:fn-> rotation-y-from-angle (u:f64) mat)
(declaim (inline rotation-y-from-angle))
(defun rotation-y-from-angle (angle)
  (declare (optimize speed))
  (rotation-y-from-angle! (zero) angle))

(u:fn-> rotation-z-from-angle! (mat u:f64) mat)
(declaim (inline rotation-z-from-angle!))
(defun rotation-z-from-angle! (out angle)
  (declare (optimize speed))
  (let ((s (sin angle))
        (c (cos angle)))
    (with-components ((o out))
      (psetf o00 c
             o10 s
             o20 0d0
             o30 0d0
             o01 (cl:- s)
             o11 c
             o21 0d0
             o31 0d0
             o02 0d0
             o12 0d0
             o22 1d0
             o32 0d0
             o03 0d0
             o13 0d0
             o23 0d0
             o33 1d0))
    out))

(u:fn-> rotation-z-from-angle (u:f64) mat)
(declaim (inline rotation-z-from-angle))
(defun rotation-z-from-angle (angle)
  (declare (optimize speed))
  (rotation-z-from-angle! (zero) angle))

(u:fn-> normalize-rotation! (mat mat) mat)
(declaim (inline normalize-rotation!))
(defun normalize-rotation! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (let ((x (dv3:vec m00 m10 m20))
          (y (dv3:vec m01 m11 m21))
          (z (dv3:vec m02 m12 m22)))
      (declare (dynamic-extent x y z))
      (dv3:normalize! x x)
      (dv3:normalize! y y)
      (dv3:normalize! z z)
      (rotation-axis-from-vec3! out x :x)
      (rotation-axis-from-vec3! out x :x)
      (rotation-axis-from-vec3! out x :x)))
  out)

(u:fn-> normalize-rotation (mat) mat)
(declaim (inline normalize-rotation))
(defun normalize-rotation (mat)
  (normalize-rotation! (copy mat) mat))

(u:fn-> get-column! (dv4:vec mat (integer 0 3)) dv4:vec)
(declaim (inline get-column!))
(defun get-column! (out mat index)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv4:with-components ((o out))
      (ecase index
        (0 (psetf ox m00 oy m10 oz m20 ow m30))
        (1 (psetf ox m01 oy m11 oz m21 ow m31))
        (2 (psetf ox m02 oy m12 oz m22 ow m32))
        (3 (psetf ox m03 oy m13 oz m23 ow m33)))))
  out)

(u:fn-> get-column (mat (integer 0 3)) dv4:vec)
(declaim (inline get-column))
(defun get-column (mat index)
  (declare (optimize speed))
  (get-column! (dv4:zero) mat index))

(u:fn-> set-column! (mat mat dv4:vec (integer 0 3)) mat)
(declaim (inline set-column!))
(defun set-column! (out mat vec index)
  (declare (optimize speed))
  (with-components ((o out))
    (dv4:with-components ((v vec))
      (copy! out mat)
      (ecase index
        (0 (psetf o00 vx o10 vy o20 vz o30 vw))
        (1 (psetf o01 vx o11 vy o21 vz o31 vw))
        (2 (psetf o02 vx o12 vy o22 vz o32 vw))
        (3 (psetf o03 vx o13 vy o23 vz o33 vw)))))
  out)

(u:fn-> set-column (mat dv4:vec (integer 0 3)) mat)
(declaim (inline set-column))
(defun set-column (mat vec index)
  (declare (optimize speed))
  (set-column! (id) mat vec index))

(u:fn-> get-translation! (dv3:vec mat) dv3:vec)
(declaim (inline get-translation!))
(defun get-translation! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv3:with-components ((o out))
      (psetf ox m03 oy m13 oz m23)))
  out)

(u:fn-> get-translation (mat) dv3:vec)
(declaim (inline get-translation))
(defun get-translation (mat)
  (declare (optimize speed))
  (get-translation! (dv3:zero) mat))

(u:fn-> set-translation! (mat mat dv3:vec) mat)
(declaim (inline set-translation!))
(defun set-translation! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv3:with-components ((v vec))
      (copy-rotation! out mat)
      (psetf o03 vx o13 vy o23 vz o33 m33)))
  out)

(u:fn-> set-translation (mat dv3:vec) mat)
(declaim (inline set-translation))
(defun set-translation (mat vec)
  (declare (optimize speed))
  (set-translation! (copy mat) mat vec))

(u:fn-> translate! (mat mat dv3:vec) mat)
(declaim (inline translate!))
(defun translate! (out mat vec)
  "Compute a translation matrix with identity rotation T from VEC, returning the matrix
multiplication of MAT * T."
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv3:with-components ((v vec))
      (psetf o00 m00
             o01 m01
             o02 m02
             o03 (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz) m03)
             o10 m10
             o11 m11
             o12 m12
             o13 (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz) m13)
             o20 m20
             o21 m21
             o22 m22
             o23 (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz) m23)
             o30 m30
             o31 m31
             o32 m32
             o33 (cl:+ (cl:* m30 vx) (cl:* m31 vy) (cl:* m32 vz) m33))))
  out)

(u:fn-> translate (mat dv3:vec) mat)
(declaim (inline translate))
(defun translate (mat vec)
  (declare (optimize speed))
  (translate! (id) mat vec))

(u:fn-> rotate! (mat mat dv3:vec &key (:space keyword)) mat)
(defun rotate! (out mat vec &key (space :local))
  (declare (optimize speed))
  (dv3:with-components ((v vec))
    (let ((x (rotation-x-from-angle vx))
          (y (rotation-y-from-angle vy))
          (z (rotation-y-from-angle vz)))
      (declare (dynamic-extent x y z))
      (copy! out mat)
      (ecase space
        (:local
         (*! out out x)
         (*! out out z)
         (*! out out y))
        (:world
         (*! out x out)
         (*! out z out)
         (*! out y out)))))
  out)

(u:fn-> rotate (mat dv3:vec) mat)
(declaim (inline rotate))
(defun rotate (mat vec)
  (declare (optimize speed))
  (rotate! (id) mat vec))

(u:fn-> get-scale! (dv3:vec mat) dv3:vec)
(declaim (inline get-scale!))
(defun get-scale! (out mat)
  (declare (optimize speed))
  (dv3:with-components ((o out))
    (psetf ox (dv3:length (rotation-axis-to-vec3 mat :x))
           oy (dv3:length (rotation-axis-to-vec3 mat :y))
           oz (dv3:length (rotation-axis-to-vec3 mat :z))))
  out)

(u:fn-> get-scale (mat) dv3:vec)
(declaim (inline get-scale))
(defun get-scale (mat)
  (declare (optimize speed))
  (get-scale! (dv3:zero) mat))

(u:fn-> set-scale! (mat mat dv3:vec) mat)
(declaim (inline set-scale!))
(defun set-scale! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out))
    (dv3:with-components ((v vec))
      (copy! out mat)
      (psetf o00 vx o11 vy o22 vz)))
  out)

(u:fn-> set-scale (mat dv3:vec) mat)
(declaim (inline set-scale))
(defun set-scale (mat vec)
  (declare (optimize speed))
  (set-scale! (copy mat) mat vec))

(u:fn-> scale! (mat mat dv3:vec) mat)
(declaim (inline scale!))
(defun scale! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out) (m mat))
    (dv3:with-components ((v vec))
      (psetf o00 (cl:* m00 vx)
             o01 (cl:* m01 vx)
             o02 (cl:* m02 vx)
             o03 (cl:* m03 vx)
             o10 (cl:* m10 vy)
             o11 (cl:* m11 vy)
             o12 (cl:* m12 vy)
             o13 (cl:* m13 vy)
             o20 (cl:* m20 vz)
             o21 (cl:* m21 vz)
             o22 (cl:* m22 vz)
             o23 (cl:* m23 vz)
             o30 m30
             o31 m31
             o32 m32
             o33 m33)))
  out)

(u:fn-> scale (mat dv3:vec) mat)
(declaim (inline scale))
(defun scale (mat vec)
  (declare (optimize speed))
  (scale! (id) mat vec))

(u:fn-> *v4! (dv4:vec mat dv4:vec) dv4:vec)
(declaim (inline *v4!))
(defun *v4! (out mat vec)
  (declare (optimize speed))
  (dv4:with-components ((v vec) (o out))
    (with-components ((m mat))
      (psetf ox (cl:+ (cl:* m00 vx) (cl:* m01 vy) (cl:* m02 vz) (cl:* m03 vw))
             oy (cl:+ (cl:* m10 vx) (cl:* m11 vy) (cl:* m12 vz) (cl:* m13 vw))
             oz (cl:+ (cl:* m20 vx) (cl:* m21 vy) (cl:* m22 vz) (cl:* m23 vw))
             ow (cl:+ (cl:* m30 vx) (cl:* m31 vy) (cl:* m32 vz) (cl:* m33 vw)))))
  out)

(u:fn-> *v4 (mat dv4:vec) dv4:vec)
(declaim (inline *v4))
(defun *v4 (mat vec)
  (declare (optimize speed))
  (*v4! (dv4:zero) mat vec))

(u:fn-> transpose! (mat mat) mat)
(declaim (inline transpose!))
(defun transpose! (out mat)
  (declare (optimize speed))
  (with-components ((o (copy! out mat)))
    (rotatef o01 o10)
    (rotatef o02 o20)
    (rotatef o03 o30)
    (rotatef o12 o21)
    (rotatef o13 o31)
    (rotatef o23 o32))
  out)

(u:fn-> transpose (mat) mat)
(declaim (inline transpose))
(defun transpose (mat)
  (declare (optimize speed))
  (transpose! (id) mat))

(u:fn-> orthogonal-p (mat) boolean)
(declaim (inline orthogonal-p))
(defun orthogonal-p (mat)
  (declare (optimize speed))
  (= (* mat (transpose mat)) +id+))

(u:fn-> orthonormalize! (mat mat) mat)
(defun orthonormalize! (out mat)
  (declare (optimize speed))
  (let* ((x (rotation-axis-to-vec3 mat :x))
         (y (rotation-axis-to-vec3 mat :y))
         (z (rotation-axis-to-vec3 mat :z)))
    (dv3:normalize! x x)
    (dv3:normalize! y (dv3:- y (dv3:scale x (dv3:dot y x))))
    (dv3:cross! z x y)
    (rotation-axis-from-vec3! out x :x)
    (rotation-axis-from-vec3! out y :y)
    (rotation-axis-from-vec3! out z :z))
  out)

(u:fn-> orthonormalize (mat) mat)
(declaim (inline orthonormalize))
(defun orthonormalize (mat)
  (declare (optimize speed))
  (orthonormalize! (id) mat))

(u:fn-> trace (mat) u:f64)
(declaim (inline trace))
(defun trace (mat)
  (with-components ((m mat))
    (cl:+ m00 m11 m22 m33)))

(u:fn-> diagonal-p (mat) boolean)
(declaim (inline diagonal-p))
(defun diagonal-p (mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (cl:= 0d0 m10 m20 m30 m01 m21 m31 m02 m12 m32 m03 m13 m23)))

(u:fn-> main-diagonal! (dv4:vec mat) dv4:vec)
(declaim (inline main-diagonal!))
(defun main-diagonal! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv4:with-components ((v out))
      (psetf vx m00 vy m11 vz m22 vw m33)))
  out)

(u:fn-> main-diagonal (mat) dv4:vec)
(declaim (inline main-diagonal))
(defun main-diagonal (mat)
  (declare (optimize speed))
  (main-diagonal! (dv4:zero) mat))

(u:fn-> anti-diagonal! (dv4:vec mat) dv4:vec)
(declaim (inline anti-diagonal!))
(defun anti-diagonal! (out mat)
  (declare (optimize speed))
  (with-components ((m mat))
    (dv4:with-components ((v out))
      (psetf vx m03 vy m12 vz m21 vw m30)))
  out)

(u:fn-> anti-diagonal (mat) dv4:vec)
(declaim (inline anti-diagonal))
(defun anti-diagonal (mat)
  (declare (optimize speed))
  (anti-diagonal! (dv4:zero) mat))

(u:fn-> set-diagonal! (mat mat dv4:vec) mat)
(declaim (inline set-diagonal!))
(defun set-diagonal! (out mat vec)
  (declare (optimize speed))
  (with-components ((o out))
    (dv4:with-components ((v vec))
      (copy! out mat)
      (psetf o00 vx o11 vy o22 vz o33 vw)))
  out)

(u:fn-> set-diagonal (mat dv4:vec) mat)
(declaim (inline set-diagonal))
(defun set-diagonal (mat vec)
  (declare (optimize speed))
  (set-diagonal! (copy mat) mat vec))

(u:fn-> determinant (mat) u:f64)
(declaim (inline determinant))
(defun determinant (mat)
  (with-components ((m mat))
    (cl:- (cl:+ (cl:* m00 m11 m22 m33) (cl:* m00 m12 m23 m31) (cl:* m00 m13 m21 m32)
                (cl:* m01 m10 m23 m32) (cl:* m01 m12 m20 m33) (cl:* m01 m13 m22 m30)
                (cl:* m02 m10 m21 m33) (cl:* m02 m11 m23 m30) (cl:* m02 m13 m20 m31)
                (cl:* m03 m10 m22 m31) (cl:* m03 m11 m20 m32) (cl:* m03 m12 m21 m30))
          (cl:* m00 m11 m23 m32) (cl:* m00 m12 m21 m33) (cl:* m00 m13 m22 m31)
          (cl:* m01 m10 m22 m33) (cl:* m01 m12 m23 m30) (cl:* m01 m13 m20 m32)
          (cl:* m02 m10 m23 m31) (cl:* m02 m11 m20 m33) (cl:* m02 m13 m21 m30)
          (cl:* m03 m10 m21 m32) (cl:* m03 m11 m22 m30) (cl:* m03 m12 m20 m31))))

(u:fn-> invert-orthogonal! (mat mat) mat)
(declaim (inline inver-orthogonal!))
(defun invert-orthogonal! (out mat)
  (declare (optimize speed))
  (copy! out mat)
  (with-components ((o out))
    (rotatef o10 o01)
    (rotatef o20 o02)
    (rotatef o21 o12)
    (psetf o03 (cl:+ (cl:* o00 (cl:- o03)) (cl:* o01 (cl:- o13)) (cl:* o02 (cl:- o23)))
           o13 (cl:+ (cl:* o10 (cl:- o03)) (cl:* o11 (cl:- o13)) (cl:* o12 (cl:- o23)))
           o23 (cl:+ (cl:* o20 (cl:- o03)) (cl:* o21 (cl:- o13)) (cl:* o22 (cl:- o23)))))
  out)

(u:fn-> invert-orthogonal (mat) mat)
(declaim (inline invert-orthogonal))
(defun invert-orthogonal (mat)
  (declare (optimize speed))
  (invert-orthogonal! (id) mat))

(u:fn-> %invert/generic! (mat mat) (values mat boolean))
(defun %invert/generic! (out mat)
  (declare (optimize speed))
  (macrolet ((% (a1 a2 a3 b1 b2 b3 c1 c2 c3 d1 d2 d3 e1 e2 e3 f1 f2 f3)
               `(/ (cl:- (cl:+ (cl:* ,a1 ,a2 ,a3) (cl:* ,b1 ,b2 ,b3) (cl:* ,c1 ,c2 ,c3))
                         (cl:* ,d1 ,d2 ,d3) (cl:* ,e1 ,e2 ,e3) (cl:* ,f1 ,f2 ,f3))
                   det)))
    (let ((det (determinant mat)))
      (when (com:= det 0.0 1d-15 1d-15)
        (warn "Skipped inverting matrix because it has a determinant of zero:~%~s" mat)
        (return-from %invert/generic! (values (copy! out mat) nil)))
      (with-components ((o out) (m mat))
        (psetf o00 (% m11 m22 m33 m12 m23 m31 m13 m21 m32 m11 m23 m32 m12 m21 m33 m13 m22 m31)
               o01 (% m01 m23 m32 m02 m21 m33 m03 m22 m31 m01 m22 m33 m02 m23 m31 m03 m21 m32)
               o02 (% m01 m12 m33 m02 m13 m31 m03 m11 m32 m01 m13 m32 m02 m11 m33 m03 m12 m31)
               o03 (% m01 m13 m22 m02 m11 m23 m03 m12 m21 m01 m12 m23 m02 m13 m21 m03 m11 m22)
               o10 (% m10 m23 m32 m12 m20 m33 m13 m22 m30 m10 m22 m33 m12 m23 m30 m13 m20 m32)
               o11 (% m00 m22 m33 m02 m23 m30 m03 m20 m32 m00 m23 m32 m02 m20 m33 m03 m22 m30)
               o12 (% m00 m13 m32 m02 m10 m33 m03 m12 m30 m00 m12 m33 m02 m13 m30 m03 m10 m32)
               o13 (% m00 m12 m23 m02 m13 m20 m03 m10 m22 m00 m13 m22 m02 m10 m23 m03 m12 m20)
               o20 (% m10 m21 m33 m11 m23 m30 m13 m20 m31 m10 m23 m31 m11 m20 m33 m13 m21 m30)
               o21 (% m00 m23 m31 m01 m20 m33 m03 m21 m30 m00 m21 m33 m01 m23 m30 m03 m20 m31)
               o22 (% m00 m11 m33 m01 m13 m30 m03 m10 m31 m00 m13 m31 m01 m10 m33 m03 m11 m30)
               o23 (% m00 m13 m21 m01 m10 m23 m03 m11 m20 m00 m11 m23 m01 m13 m20 m03 m10 m21)
               o30 (% m10 m22 m31 m11 m20 m32 m12 m21 m30 m10 m21 m32 m11 m22 m30 m12 m20 m31)
               o31 (% m00 m21 m32 m01 m22 m30 m02 m20 m31 m00 m22 m31 m01 m20 m32 m02 m21 m30)
               o32 (% m00 m12 m31 m01 m10 m32 m02 m11 m30 m00 m11 m32 m01 m12 m30 m02 m10 m31)
               o33 (% m00 m11 m22 m01 m12 m20 m02 m10 m21 m00 m12 m21 m01 m10 m22 m02 m11 m20))
        (values out t)))))

(u:fn-> invert! (mat mat) (values mat boolean))
(defun invert! (out mat)
  (macrolet ((% (a b c d)
               `(cl:/ (cl:- (cl:* ,a ,b) (cl:* ,c ,d)) det)))
    (flet ((sub-determinant (matrix)
             (with-components ((m matrix))
               (cl:- (cl:+ (cl:* m00 m11 m22) (cl:* m10 m21 m02) (cl:* m20 m01 m12))
                     (cl:* m00 m21 m12)
                     (cl:* m20 m11 m02)
                     (cl:* m10 m01 m22)))))
      (declare (inline sub-determinant))
      (with-components ((m mat) (o out))
        (zero! out)
        (when (id-p mat)
          (return-from invert! (id! out)))
        (unless (and (com:= m30 0d0) (com:= m31 0d0) (com:= m32 0d0) (com:= m33 1d0))
          (return-from invert! (%invert/generic! out mat)))
        (let ((det (sub-determinant mat)))
          (if (and (com:= det 0d0 1d-15 1d-15)
                   (or (not (com:= m00 0d0)) (not (com:= m01 0d0)) (not (com:= m02 0d0))
                       (not (com:= m10 0d0)) (not (com:= m11 0d0)) (not (com:= m12 0d0))
                       (not (com:= m20 0d0)) (not (com:= m21 0d0)) (not (com:= m22 0d0))))
              (return-from invert! (%invert/generic! out mat))
              (setf o00 (% m11 m22 m12 m21)
                    o01 (% m02 m21 m01 m22)
                    o02 (% m01 m12 m02 m11)
                    o10 (% m12 m20 m10 m22)
                    o11 (% m00 m22 m02 m20)
                    o12 (% m02 m10 m00 m12)
                    o20 (% m10 m21 m11 m20)
                    o21 (% m01 m20 m00 m21)
                    o22 (% m00 m11 m01 m10)))
          (setf o03 (cl:- (cl:+ (cl:* m03 o00) (cl:* m13 o01) (cl:* m23 o02)))
                o13 (cl:- (cl:+ (cl:* m03 o10) (cl:* m13 o11) (cl:* m23 o12)))
                o23 (cl:- (cl:+ (cl:* m03 o20) (cl:* m13 o21) (cl:* m23 o22)))
                o33 1d0)
          out)))))

(u:fn-> invert (mat) mat)
(declaim (inline invert))
(defun invert (mat)
  (declare (optimize speed))
  (invert! (id) mat))

(u:fn-> look-at! (mat dv3:vec dv3:vec dv3:vec) mat)
(defun look-at! (out eye target up)
  (declare (optimize speed))
  (with-components ((o out))
    (let* ((z (dv3:- target eye))
           (x (dv3:cross z up))
           (y (dv3:cross x z)))
      (declare (dynamic-extent x y z))
      (dv3:with-components ((x x) (y y) (z z))
        (dv3:normalize! x x)
        (dv3:normalize! y y)
        (dv3:normalize! z z)
        (psetf o00 xx
               o01 xy
               o02 xz
               o03 (dv3:dot x eye)
               o10 yx
               o11 yy
               o12 yz
               o13 (dv3:dot y eye)
               o20 (cl:- zx)
               o21 (cl:- zy)
               o22 (cl:- zz)
               o23 (dv3:dot z eye)
               o30 0d0
               o31 0d0
               o32 0d0
               o33 1d0))))
  out)

(u:fn-> look-at (dv3:vec dv3:vec dv3:vec) mat)
(declaim (inline look-at))
(defun look-at (eye target up)
  (declare (optimize speed))
  (look-at! (id) eye target up))

(u:fn-> ortho! (mat u:f64 u:f64 u:f64 u:f64 u:f64 u:f64) mat)
(defun ortho! (out left right bottom top near far)
  (declare (optimize speed))
  (let ((right-left (cl:- right left))
        (top-bottom (cl:- top bottom))
        (far-near (cl:- far near)))
    (with-components ((m (id! out)))
      (psetf m00 (/ 2d0 right-left)
             m03 (cl:- (/ (cl:+ right left) right-left))
             m11 (/ 2d0 top-bottom)
             m13 (cl:- (/ (cl:+ top bottom) top-bottom))
             m22 (/ -2d0 far-near)
             m23 (cl:- (/ (cl:+ far near) far-near))))
    out))

(u:fn-> ortho (u:f64 u:f64 u:f64 u:f64 u:f64 u:f64) mat)
(declaim (inline ortho))
(defun ortho (left right bottom top near far)
  (declare (optimize speed))
  (ortho! (id) left right bottom top near far))

(u:fn-> perspective! (mat u:f64 u:f64 u:f64 u:f64) mat)
(defun perspective! (out fov aspect near far)
  (declare (optimize speed))
  (let ((f (/ (tan (/ fov 2d0))))
        (z (cl:- near far)))
    (with-components ((m (zero! out)))
      (psetf m00 (cl:* f (cl:/ aspect))
             m11 f
             m22 (/ (cl:+ near far) z)
             m23 (/ (cl:* 2 near far) z)
             m32 -1d0)))
  out)

(u:fn-> perspective (u:f64 u:f64 u:f64 u:f64) mat)
(declaim (inline perspective))
(defun perspective (fov aspect near far)
  (declare (optimize speed))
  (perspective! (id) fov aspect near far))
