(in-package #:vorigin.test)

(define-test m4/identity
  (let ((m (m4:zero))
        (r (m4:mat 1f0 0f0 0f0 0f0
                   0f0 1f0 0f0 0f0
                   0f0 0f0 1f0 0f0
                   0f0 0f0 0f0 1f0)))
    (is m4:= m4:+id+ r)
    (is m4:= (m4:id) r)
    (true (m4:id-p (m4:id)))
    (true (m4:id-p (m4:id! m)))))

(define-test m4/copy
  (let ((m m4:+id+)
        (o (m4:zero)))
    (is m4:= (m4:copy! o m) m4:+id+)
    (is m4:= o m4:+id+)
    (is m4:= (m4:copy m) m4:+id+)
    (isnt eq m (m4:copy m))))

(define-test m4/clamp
  (let ((m (m4:mat 1f0 -2f0 3f0 -4f0
                   5f0 -6f0 7f0 -8f0
                   9f0 -10f0 11f0 -12f0
                   13f0 -14f0 15f0 -16f0))
        (o (m4:zero))
        (r (m4:mat 1f0 -1f0 1f0 -1f0
                   1f0 -1f0 1f0 -1f0
                   1f0 -1f0 1f0 -1f0
                   1f0 -1f0 1f0 -1f0)))
    (is m4:= (m4:clamp-range! o m -1f0 1f0) r)
    (is m4:= o r)
    (is m4:= (m4:clamp-range m -1f0 1f0) r)
    (is m4:= (m4:clamp-range m
                             most-negative-single-float
                             most-positive-single-float)
        m)))

(define-test m4/add
  (let ((m1 (m4:mat 8f0 2f0 9f0 1f0
                    0f0 4f0 8f0 4f0
                    8f0 3f0 0f0 1f0
                    1f0 8f0 3f0 8f0))
        (m2 (m4:mat 8f0 6f0 6f0 1f0
                    9f0 3f0 8f0 4f0
                    7f0 0f0 1f0 1f0
                    8f0 3f0 6f0 7f0))
        (o (m4:zero))
        (r (m4:mat 16f0 8f0 15f0 2f0
                   9f0 7f0 16f0 8f0
                   15f0 3f0 1f0 2f0
                   9f0 11f0 9f0 15f0)))
    (is m4:= (m4:+! o m1 m2) r)
    (is m4:= o r)
    (is m4:= (m4:+ m1 m2) r)
    (is m4:= (m4:+ m1 m4:+zero+) m1)
    (is m4:= (m4:+ m2 m4:+zero+) m2)))

(define-test m4/subtract
  (let ((m1 (m4:mat 8f0 2f0 9f0 1f0
                    0f0 4f0 8f0 4f0
                    8f0 3f0 0f0 1f0
                    1f0 8f0 3f0 8f0))
        (m2 (m4:mat 8f0 6f0 6f0 1f0
                    9f0 3f0 8f0 4f0
                    7f0 0f0 1f0 1f0
                    8f0 3f0 6f0 7f0))
        (o (m4:zero))
        (r (m4:mat 0f0 -4f0 3f0 0f0
                   -9f0 1f0 0f0 0f0
                   1f0 3f0 -1f0 0f0
                   -7f0 5f0 -3f0 1f0)))
    (is m4:= (m4:-! o m1 m2) r)
    (is m4:= o r)
    (is m4:= (m4:- m1 m2) r)
    (is m4:= (m4:- m1 m4:+zero+) m1)
    (is m4:= (m4:- m2 m4:+zero+) m2)))

(define-test m4/multiply
  (let ((m1 (m4:mat 1f0 5f0 9f0 13f0
                    2f0 6f0 10f0 14f0
                    3f0 7f0 11f0 15f0
                    4f0 8f0 12f0 16f0))
        (m2 (m4:mat 10f0 50f0 90f0 130f0
                    20f0 60f0 100f0 140f0
                    30f0 70f0 110f0 150f0
                    40f0 80f0 120f0 160f0))
        (m3 m4:+id+)
        (r (m4:mat 90f0 202f0 314f0 426f0
                   100f0 228f0 356f0 484f0
                   110f0 254f0 398f0 542f0
                   120f0 280f0 440f0 600f0))
        (rot-x (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (rot-y (m4:rotate m4:+id+ (v3:vec 0f0 const:pi/4 0f0)))
        (rot-xy (m4:rotate m4:+id+ (v3:vec const:pi/3 const:pi/4 0f0)))
        (tr1 (m4:translate m4:+id+ (v3:vec 5f0 10f0 15f0)))
        (tr2 (m4:translate m4:+id+ (v3:vec 10f0 20f0 30f0)))
        (o (m4:zero)))
    (is m4:= (m4:*! o m1 m1) r)
    (is m4:= o r)
    (is m4:= (m4:* m1 m3) m1)
    (is m4:= (m4:* m3 m1) m1)
    (is m4:= (m4:* m1 m2) (m4:* m2 m1))
    (is m4:= (m4:* rot-x rot-y) rot-xy)
    (isnt m4:= (m4:* rot-x rot-y) (m4:* rot-y rot-x))
    (is v3:= (m4:get-translation (m4:* tr1 rot-xy)) (m4:get-translation tr1))
    (is v3:= (m4:get-translation (m4:* tr1 tr2)) (v3:vec 15f0 30f0 45f0))
    (is v3:= (m4:get-translation (m4:* tr2 tr1)) (v3:vec 15f0 30f0 45f0))))

(define-test m4/translation-convert
  (let ((m (m4:mat 1f0 2f0 3f0 4f0
                   5f0 6f0 7f0 8f0
                   9f0 10f0 11f0 12f0
                   13f0 14f0 15f0 16f0))
        (rm (m4:mat 1f0 0f0 0f0 0f0
                    0f0 1f0 0f0 0f0
                    0f0 0f0 1f0 0f0
                    5f0 10f0 15f0 1f0))
        (om (m4:id))
        (v (v3:vec 5f0 10f0 15f0))
        (rv (v3:vec 13f0 14f0 15f0))
        (ov (v3:zero)))
    (is m4:= (m4:set-translation! om om v) rm)
    (is m4:= om rm)
    (is m4:= (m4:set-translation om v) rm)
    (is v3:= (m4:get-translation! ov m) rv)
    (is v3:= ov rv)
    (is v3:= (m4:get-translation m) rv)))

(define-test m4/translate
  (let ((m (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (o (m4:id))
        (r (m4:mat 1f0 0f0 0f0 0f0 0f0 0.49999997f0 0.86602545f0 0f0 0f0 -0.86602545f0 0.49999997f0
                   0f0 5f0 -7.9903817f0 16.160254f0 1f0))
        (v (v3:vec 5f0 10f0 15f0))
        (ov (v3:vec 5f0 -7.9903817f0 16.160254f0)))
    (true (m4:= (m4:translate! o m v) r))
    (true (m4:= o r))
    (is v3:= (m4:get-translation (m4:translate m4:+id+ v)) v)
    (is v3:= (m4:get-translation (m4:translate m v)) ov)))

(define-test m4/rotation-copy
  (let ((m (m4:mat 1f0 2f0 3f0 4f0
                   5f0 6f0 7f0 8f0
                   9f0 10f0 11f0 12f0
                   13f0 14f0 15f0 16f0))
        (r (m4:mat 1f0 2f0 3f0 0f0
                   5f0 6f0 7f0 0f0
                   9f0 10f0 11f0 0f0
                   0f0 0f0 0f0 1f0))
        (o (m4:id)))
    (is m4:= (m4:copy-rotation! o m) r)
    (is m4:= o r)
    (is m4:= (m4:copy-rotation m) r)))

(define-test m4/rotation-convert
  (let ((m (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (rmx m4:+id+)
        (rmy (m4:mat 1f0 0f0 0f0 0f0
                     0f0 0.5f0 0.86602545f0 0f0
                     0f0 0f0 1f0 0f0
                     0f0 0f0 0f0 1f0))
        (rmz (m4:mat 1f0 0f0 0f0 0f0
                     0f0 1f0 0f0 0f0
                     0f0 -0.86602545f0 0.5f0 0f0
                     0f0 0f0 0f0 1f0))
        (omx (m4:id))
        (omy (m4:id))
        (omz (m4:id))
        (rvx (v3:vec 1f0 0f0 0f0))
        (rvy (v3:vec 0f0 0.5f0 0.86602545f0))
        (rvz (v3:vec 0f0 -0.86602545f0 0.5f0))
        (ovx (v3:zero))
        (ovy (v3:zero))
        (ovz (v3:zero)))
    (true (v3:= (m4:rotation-axis-to-vec3! ovx m :x) rvx))
    (true (v3:= (m4:rotation-axis-to-vec3! ovy m :y) rvy))
    (true (v3:= (m4:rotation-axis-to-vec3! ovz m :z) rvz))
    (true (v3:= ovx rvx))
    (true (v3:= ovy rvy))
    (true (v3:= ovz rvz))
    (true (v3:= (m4:rotation-axis-to-vec3 m :x) rvx))
    (true (v3:= (m4:rotation-axis-to-vec3 m :y) rvy))
    (true (v3:= (m4:rotation-axis-to-vec3 m :z) rvz))
    (true (m4:= (m4:rotation-axis-from-vec3! omx rvx :x) rmx))
    (true (m4:= (m4:rotation-axis-from-vec3! omy rvy :y) rmy))
    (true (m4:= (m4:rotation-axis-from-vec3! omz rvz :z) rmz))
    (true (m4:= omx rmx))
    (true (m4:= omy rmy))
    (true (m4:= omz rmz))
    (true (m4:= (m4:rotation-axis-from-vec3 m4:+id+ rvx :x) rmx))
    (true (m4:= (m4:rotation-axis-from-vec3 m4:+id+ rvy :y) rmy))
    (true (m4:= (m4:rotation-axis-from-vec3 m4:+id+ rvz :z) rmz))))

(define-test m4/rotation
  (let ((omx (m4:id))
        (omy (m4:id))
        (omz (m4:id))
        (rmx (m4:mat 1f0 0f0 0f0 0f0
                     0f0 0.5f0 0.86602545f0 0f0
                     0f0 -0.86602545f0 0.5f0 0f0
                     0f0 0f0 0f0 1f0))
        (rmy (m4:mat 0.5f0 0f0 -0.86602545f0 0f0
                     0f0 1f0 0f0 0f0
                     0.86602545f0 0f0 0.5f0 0f0
                     0f0 0f0 0f0 1f0))
        (rmz (m4:mat 0.5f0 0.86602545f0 0f0 0f0
                     -0.86602545f0 0.5f0 0f0 0f0
                     0f0 0f0 1f0 0f0
                     0f0 0f0 0f0 1f0))
        (vx (v3:vec const:pi/3 0f0 0f0))
        (vy (v3:vec 0f0 const:pi/3 0f0))
        (vz (v3:vec 0f0 0f0 const:pi/3)))
    (true (m4:= (m4:rotate! omx m4:+id+ vx) rmx))
    (true (m4:= (m4:rotate! omy m4:+id+ vy) rmy))
    (true (m4:= (m4:rotate! omz m4:+id+ vz) rmz))
    (true (m4:= omx rmx))
    (true (m4:= omy rmy))
    (true (m4:= omz rmz))
    (true (m4:= (m4:rotate m4:+id+ vx) rmx))
    (true (m4:= (m4:rotate m4:+id+ vy) rmy))
    (true (m4:= (m4:rotate m4:+id+ vz) rmz))))

(define-test m4/scale
  (let ((m (m4:mat 10f0 0f0 0f0 0f0
                   0f0 20f0 0f0 0f0
                   0f0 0f0 30f0 0f0
                   0f0 0f0 0f0 2f0))
        (o (m4:id))
        (s (m4:mat 10f0 0f0 0f0 0f0
                   0f0 40f0 0f0 0f0
                   0f0 0f0 90f0 0f0
                   0f0 0f0 0f0 2f0))
        (v (v3:vec 1f0 2f0 3f0)))
    (true (m4:= (m4:scale! o m v) s))
    (true (m4:= o s))
    (is v3:= (m4:get-scale (m4:scale m4:+id+ v)) v)))

(define-test m4/vec4-multiply
  (let ((m (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (v (v4:vec 1f0 2f0 3f0 4f0))
        (o (v4:zero))
        (rv (v4:vec 1f0 -1.5980763f0 3.232051f0 4f0)))
    (is v4:= (m4:*v4! o m v) rv)
    (is v4:= o rv)
    (is v4:= (m4:*v4 m v) rv)
    (is v4:= (m4:*v4 m4:+id+ v) v)
    (is v4:= (m4:*v4 m4:+id+ v4:+zero+) v4:+zero+)))

(define-test m4/transpose
  (let ((m (m4:mat 1f0 5f0 9f0 13f0
                   2f0 6f0 10f0 14f0
                   3f0 7f0 11f0 15f0
                   4f0 8f0 12f0 16f0))
        (r (m4:mat 1f0 2f0 3f0 4f0
                   5f0 6f0 7f0 8f0
                   9f0 10f0 11f0 12f0
                   13f0 14f0 15f0 16f0))
        (o (m4:id)))
    (is m4:= (m4:transpose! o m) r)
    (is m4:= o r)
    (is m4:= (m4:transpose m) r)
    (is m4:= (m4:transpose m4:+id+) m4:+id+)))

(define-test m4/orthogonal-predicate
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec const:pi 0f0 0f0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec const:pi/2 0f0 0f0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec const:pi/4 0f0 0f0))))
  (true (m4:orthogonal-p (m4:rotate m4:+id+ (v3:vec const:pi/6 0f0 0f0)))))

(define-test m4/orthgonalize
  (let ((m (m4:mat 0f0 0f0 1f0 0f0
                   1f0 0f0 0f0 0f0
                   -0.12988785f0 0.3997815f0 0.5468181f0 0f0
                   1.0139829f0 -0.027215311f0 0.18567966f0 0f0))
        (o (m4:id))
        (r (m4:mat 0f0 0f0 1f0 0f0
                   1f0 0f0 0f0 0f0
                   0f0 1f0 0f0 0f0
                   0f0 0f0 0f0 1f0)))
    (is m4:= (m4:orthonormalize! o m) r)
    (is m4:= o r)
    (is m4:= (m4:orthonormalize m) r)))

(define-test m4/trace
  (is = (m4:trace (m4:zero)) 0f0)
  (is = (m4:trace m4:+id+) 4f0)
  (is = (m4:trace (m4:mat 1f0 2f0 3f0 4f0
                          5f0 6f0 7f0 8f0
                          9f0 10f0 11f0 12f0
                          13f0 14f0 15f0 16f0))
      34f0))

(define-test m4/diagonal
  (let ((m (m4:mat 1f0 2f0 3f0 4f0
                   5f0 6f0 7f0 8f0
                   9f0 10f0 11f0 12f0
                   13f0 14f0 15f0 16f0))
        (r1 (v4:vec 1f0 6f0 11f0 16f0))
        (r2 (v4:vec 13f0 10f0 7f0 4f0))
        (o (v4:zero)))
    (true (m4:diagonal-p (m4:id)))
    (true (not (m4:diagonal-p m)))
    (is v4:= (m4:main-diagonal! o m) r1)
    (is v4:= o r1)
    (is v4:= (m4:main-diagonal m) r1)
    (is v4:= (m4:anti-diagonal! o m) r2)
    (is v4:= o r2)
    (is v4:= (m4:anti-diagonal m) r2)))

(define-test m4/determinant
  (is = (m4:determinant (m4:mat 1f0 5f0 9f0 13f0
                                2f0 6f0 10f0 14f0
                                3f0 7f0 11f0 15f0
                                4f0 8f0 12f0 16f0))
      0f0)
  (is = (m4:determinant m4:+id+) 1f0)
  (is = (m4:determinant (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0))) 1f0)
  (is = (m4:determinant (m4:mat 1f0 0f0 0f0 0f0
                                0f0 0f0 1f0 0f0
                                0f0 1f0 0f0 0f0
                                0f0 0f0 0f0 1f0))
      -1f0))

(define-test m4/invert
  (let ((m (m4:rotate m4:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (r (m4:rotate m4:+id+ (v3:vec (- const:pi/3) 0f0 0f0)))
        (o (m4:id)))
    (is m4:= (m4:invert! o m) r)
    (is m4:= o r)
    (is m4:= (m4:invert m) r)
    (is m4:= (m4:invert m4:+id+) m4:+id+)
    (is-values (m4:invert (m4:mat 1f0 5f0 9f0 13f0
                                  2f0 6f0 10f0 14f0
                                  3f0 7f0 11f0 15f0
                                  4f0 8f0 12f0 16f0))
      (m4:= (m4:mat 1f0 5f0 9f0 13f0
                    2f0 6f0 10f0 14f0
                    3f0 7f0 11f0 15f0
                    4f0 8f0 12f0 16f0))
      (eq nil))))

(define-test m4/look-at
  (let ((o (m4:id))
        (r (m4:mat -0.7071068f0 0f0 0.7071068f0 0f0
                   0f0 1f0 0f0 0f0
                   -0.7071068f0 0f0 -0.7071068f0 0f0
                   0.7071068f0 0f0 -0.7071068f0 1f0))
        (o2 (m4:id))
        (r2 (m4:mat 0.9622504f0 0.05143445f0 0.26726124f0 0f0
                    -0.19245008f0 0.8229512f0 0.5345225f0 0f0
                    -0.19245008f0 -0.5657789f0 0.80178374f0 0f0
                    -2.3841858f-7 -9.536743f-7 -18.708286f0 1f0)))
    (true (m4:= (m4:look-at! o
                             (v3:vec 1f0 0f0 0f0)
                             (v3:vec 0f0 0f0 1f0)
                             (v3:vec 0f0 1f0 0f0))
              r))
    (true (m4:= o r))
    (true (m4:= (m4:look-at (v3:vec 1f0 0f0 0f0)
                            (v3:vec 0f0 0f0 1f0)
                            (v3:vec 0f0 1f0 0f0))
              r))
    (true (m4:= (m4:look-at! o2
                             (v3:vec 5f0 10f0 15f0)
                             (v3:vec 0f0 0f0 0f0)
                             (v3:vec 0f0 1f0 -1f0))
              r2))
    (true (m4:= o2 r2))
    (true (m4:= (m4:look-at (v3:vec 5f0 10f0 15f0)
                            (v3:vec 0f0 0f0 0f0)
                            (v3:vec 0f0 1f0 -1f0))
              r2))))

(define-test m4/ortho
  (let ((r (m4:mat 0.05f0 0f0 0f0 0f0
                   0f0 0.1f0 0f0 0f0
                   0f0 0f0 -0.002f0 0f0
                   0f0 0f0 -1f0 1f0))
        (o (m4:id)))
    (is m4:= (m4:ortho! o -20f0 20f0 -10f0 10f0 0f0 1000f0) r)
    (is m4:= o r)
    (is m4:= (m4:ortho -20f0 20f0 -10f0 10f0 0f0 1000f0) r)))

(define-test m4/perspective
  (let ((r (m4:mat 0.9742786f0 0f0 0f0 0f0
                   0f0 1.7320509f0 0f0 0f0
                   0f0 0f0 -1.002002f0 -1f0
                   0f0 0f0 -2.002002f0 0f0))
        (o (m4:id)))
    (is m4:= (m4:perspective! o const:pi/3 (/ 16f0 9f0) 1f0 1000f0)
        r)
    (is m4:= o r)
    (is m4:= (m4:perspective const:pi/3 (/ 16f0 9f0) 1f0 1000f0) r)))
