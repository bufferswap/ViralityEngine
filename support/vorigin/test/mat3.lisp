(in-package #:vorigin.test)

(define-test m3/identity
  (let ((m (m3:zero))
        (r (m3:mat 1f0 0f0 0f0
                   0f0 1f0 0f0
                   0f0 0f0 1f0)))
    (is m3:= m3:+id+ r)
    (is m3:= (m3:id) r)
    (true (m3:id-p (m3:id)))
    (true (m3:id-p (m3:id! m)))))

(define-test m3/copy
  (let ((m m3:+id+)
        (o (m3:zero)))
    (is m3:= (m3:copy! o m) m3:+id+)
    (is m3:= o m3:+id+)
    (is m3:= (m3:copy m) m3:+id+)
    (isnt eq m (m3:copy m))))

(define-test m3/clamp
  (let ((m (m3:mat 1f0 -2f0 3f0
                   -4f0 5f0 -6f0
                   7f0 -8f0 9f0))
        (o (m3:zero))
        (r (m3:mat 1f0 -1f0 1f0
                   -1f0 1f0 -1f0
                   1f0 -1f0 1f0)))
    (is m3:= (m3:clamp-range! o m -1f0 1f0) r)
    (is m3:= o r)
    (is m3:= (m3:clamp-range m -1f0 1f0) r)
    (is m3:= (m3:clamp-range m
                             most-negative-single-float
                             most-positive-single-float)
        m)))

(define-test m3/add
  (let ((m1 (m3:mat 1f0 9f0 4f0
                    2f0 7f0 9f0
                    1f0 5f0 2f0))
        (m2 (m3:mat 9f0 2f0 3f0
                    8f0 3f0 8f0
                    1f0 0f0 1f0))
        (o (m3:zero))
        (r (m3:mat 10f0 11f0 7f0
                   10f0 10f0 17f0
                   2f0 5f0 3f0)))
    (is m3:= (m3:+! o m1 m2) r)
    (is m3:= o r)
    (is m3:= (m3:+ m1 m2) r)
    (is m3:= (m3:+ m1 m3:+zero+) m1)
    (is m3:= (m3:+ m2 m3:+zero+) m2)))

(define-test m3/subtract
  (let ((m1 (m3:mat 1f0 9f0 4f0
                    2f0 7f0 9f0
                    1f0 5f0 2f0))
        (m2 (m3:mat 9f0 2f0 3f0
                    8f0 3f0 8f0
                    1f0 0f0 1f0))
        (o (m3:zero))
        (r (m3:mat -8f0 7f0 1f0
                   -6f0 4f0 1f0
                   0f0 5f0 1f0)))
    (is m3:= (m3:-! o m1 m2) r)
    (is m3:= o r)
    (is m3:= (m3:- m1 m2) r)
    (is m3:= (m3:- m1 m3:+zero+) m1)
    (is m3:= (m3:- m2 m3:+zero+) m2)))

(define-test m3/multiply
  (let ((m1 (m3:mat 1f0 5f0 9f0
                    2f0 6f0 10f0
                    3f0 7f0 11f0))
        (m2 (m3:mat 10f0 50f0 90f0
                    20f0 60f0 100f0
                    30f0 70f0 110f0))
        (m3 m3:+id+)
        (r (m3:mat 38f0 98f0 158f0
                   44f0 116f0 188f0
                   50f0 134f0 218f0))
        (rot-z (m3:rotate m3:+id+ const:pi/3))
        (tr1 (m3:translate m3:+id+ (v2:vec 5f0 10f0)))
        (tr2 (m3:translate m3:+id+ (v2:vec 10f0 20f0)))
        (o (m3:zero)))
    (is m3:= (m3:*! o m1 m1) r)
    (is m3:= o r)
    (is m3:= (m3:* m1 m3) m1)
    (is m3:= (m3:* m3 m1) m1)
    (is m3:= (m3:* m1 m2) (m3:* m2 m1))
    (isnt m3:= (m3:* m1 rot-z) (m3:* rot-z m1))
    (is v2:= (m3:get-translation (m3:* tr1 rot-z)) (m3:get-translation tr1))
    (is v2:= (m3:get-translation (m3:* tr1 tr2)) (v2:vec 15f0 30f0))
    (is v2:= (m3:get-translation (m3:* tr2 tr1)) (v2:vec 15f0 30f0))))

(define-test m3/translation-convert
  (let ((m (m3:mat 1f0 2f0 3f0
                   5f0 6f0 7f0
                   9f0 10f0 11f0))
        (rm (m3:mat 1f0 0f0 0f0
                    0f0 1f0 0f0
                    9f0 10f0 1f0))
        (om (m3:id))
        (v (v2:vec 9f0 10f0))
        (rv (v2:vec 9f0 10f0))
        (ov (v2:zero)))
    (is m3:= (m3:set-translation! om om v) rm)
    (is m3:= om rm)
    (is m3:= (m3:set-translation om v) rm)
    (is v2:= (m3:get-translation! ov m) rv)
    (is v2:= ov rv)
    (is v2:= (m3:get-translation m) rv)))

(define-test m3/translate
  (let ((m (m3:rotate m3:+id+ const:pi/3))
        (v (v2:vec 5f0 10f0))
        (ov (v2:vec -6.1602545f0 9.330127f0)))
    (is v2:= (m3:get-translation (m3:translate m3:+id+ v)) v)
    (is v2:= (m3:get-translation (m3:translate m v)) ov)))

(define-test m3/rotation-copy
  (let ((m (m3:mat 1f0 5f0 0f0
                   2f0 6f0 0f0
                   0f0 0f0 1f0))
        (r (m3:mat 1f0 5f0 0f0
                   2f0 6f0 0f0
                   0f0 0f0 1f0))
        (o (m3:id)))
    (is m3:= (m3:copy-rotation! o m) r)
    (is m3:= o r)
    (is m3:= (m3:copy-rotation m) r)))

(define-test m3/rotation-convert
  (let ((rmx m3:+id+)
        (omx (m3:id))
        (rvx (v2:vec 1f0 0f0)))
    (is m3:= (m3:rotation-axis-from-vec2! omx rvx :x) rmx)
    (true (m3:= omx rmx))
    (is m3:= (m3:rotation-axis-from-vec2 m3:+id+ rvx :x) rmx)))

(define-test m3/rotate
  (let ((omz (m3:id))
        (rmz (m3:mat 0.5f0 0.86602545f0 0f0
                     -0.86602545f0 0.5f0 0f0
                     0f0 0f0 1f0)))
    (true (m3:= (m3:rotate! omz m3:+id+ const:pi/3) rmz))
    (true (m3:= omz rmz))
    (true (m3:= (m3:rotate m3:+id+ const:pi/3) rmz))))

(define-test m3/scale
  (let ((m (m3:mat 10f0 0f0 0f0
                   0f0 20f0 0f0
                   0f0 0f0 2f0))
        (o (m3:id))
        (s (m3:mat 10f0 0f0 0f0
                   0f0 40f0 0f0
                   0f0 0f0 2f0))
        (v (v2:vec 1f0 2f0)))
    (true (m3:= (m3:scale! o m v) s))
    (true (m3:= o s))
    (is v2:= (m3:get-scale (m3:scale m3:+id+ v)) v)))

(define-test m3/vec3-multiply
  (let ((m (m3:rotate m3:+id+ const:pi/3))
        (v (v3:vec 1f0 2f0 3f0))
        (o (v3:zero))
        (rv (v3:vec -1.2320509f0 1.8660254f0 3f0)))
    (is v3:= (m3:*v3! o m v) rv)
    (is v3:= o rv)
    (is v3:= (m3:*v3 m v) rv)
    (is v3:= (m3:*v3 m3:+id+ v) v)
    (is v3:= (m3:*v3 m3:+id+ v3:+zero+) v3:+zero+)))

(define-test m3/transpose
  (let ((m (m3:mat 1f0 5f0 9f0
                   2f0 6f0 10f0
                   3f0 7f0 11f0))
        (r (m3:mat 1f0 2f0 3f0
                   5f0 6f0 7f0
                   9f0 10f0 11f0))
        (o (m3:id)))
    (is m3:= (m3:transpose! o m) r)
    (is m3:= o r)
    (is m3:= (m3:transpose m) r)
    (is m3:= (m3:transpose m3:+id+) m3:+id+)))

(define-test m3/orthogonal-predicate
  (true (m3:orthogonal-p (m3:rotate m3:+id+ const:pi)))
  (true (m3:orthogonal-p (m3:rotate m3:+id+ const:pi/2)))
  (true (m3:orthogonal-p (m3:rotate m3:+id+ const:pi/3))))

(define-test m3/trace
  (is = (m3:trace (m3:zero)) 0f0)
  (is = (m3:trace m3:+id+) 3f0)
  (is = (m3:trace (m3:mat 1f0 2f0 3f0
                          4f0 5f0 6f0
                          7f0 8f0 9f0))
      15f0))

(define-test m3/diagonal
  (let ((m (m3:mat 1f0 2f0 3f0
                   4f0 5f0 6f0
                   7f0 8f0 9f0))
        (r1 (v3:vec 1f0 5f0 9f0))
        (r2 (v3:vec 7f0 5f0 3f0))
        (o (v3:zero)))
    (true (m3:diagonal-p (m3:id)))
    (true (not (m3:diagonal-p m)))
    (is v3:= (m3:main-diagonal! o m) r1)
    (is v3:= o r1)
    (is v3:= (m3:main-diagonal m) r1)
    (is v3:= (m3:anti-diagonal! o m) r2)
    (is v3:= o r2)
    (is v3:= (m3:anti-diagonal m) r2)))
