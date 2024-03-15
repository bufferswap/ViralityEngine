(in-package #:vorigin.test)

(define-test q/identity
  (let ((q (q:id))
        (r (q:id)))
    (is q:= q:+id+ r)
    (is q:= (q:id) r)
    (true (q:id-p (q:quat 1f0 0f0 0f0 0f0)))
    (true (q:id-p (q:id! q)))))

(define-test q/equality
  (let ((q1 (q:quat 0.25889468f0 -0.4580922f0 0.6231675f0 0.34003425f0))
        (q2 (q:quat 1f0 1e-8 1e-8 1e-8))
        (r (q:quat 1.2588947f0 -0.4580922f0 0.6231675f0 0.34003425f0)))
    (true (q:= q1 q1))
    (true (q:= (q:+ q1 q2) r))
    (true (q:= q2 (q:id)))))

(define-test q/copy
  (let ((q (q:quat 0.34003425f0 -0.4920528f0 0.8754709f0 0.6535034f0))
        (o (q:id)))
    (is q:= (q:copy! o q) q)
    (is q:= o q)
    (is q:= (q:copy q) q)
    (isnt eq q (q:copy q))))

(define-test q/add
  (let ((q1 (q:quat -0.11586404f0 -0.47056317f0 0.23266816f0 -0.6098385f0))
        (q2 (q:quat -0.81111765f0 0.11399269f0 -0.24647212f0 -0.812474f0))
        (r (q:quat -0.9269817f0 -0.35657048f0 -0.013803959f0 -1.4223125f0))
        (o (q:id)))
    (is q:= (q:+! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:+ q1 q2) r)))

(define-test q/subtract
  (let ((q1 (q:quat 0.1688292f0 0.5137224f0 0.83796954f0 -0.9853494f0))
        (q2 (q:quat -0.3770373f0 0.19171429f0 -0.8571534f0 0.4451759f0))
        (r (q:quat 0.5458665f0 0.32200813f0 1.695123f0 -1.4305253f0))
        (o (q:id)))
    (is q:= (q:-! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:- q1 q2) r)))

(define-test q/multiply
  (let ((q1 (q:quat 1f0 2f0 3f0 4f0))
        (q2 (q:quat 10f0 20f0 30f0 40f0))
        (q3 q:+id+)
        (r (q:quat -280f0 40f0 60f0 80f0))
        (rot-x (q:rotate-euler q:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (rot-y (q:rotate-euler q:+id+ (v3:vec 0f0 const:pi/4 0f0)))
        (rot-xy (q:rotate-euler q:+id+ (v3:vec const:pi/3 const:pi/4 0f0)))
        (o (q:id)))
    (is q:= (q:*! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:* q1 q3) q1)
    (is q:= (q:* q3 q1) q1)
    (is q:= (q:* q1 q2) (q:* q2 q1))
    (is q:= (q:* rot-x rot-y) rot-xy)
    (isnt q:= (q:* rot-x rot-y) (q:* rot-y rot-x))))

(define-test q/scalar-product
  (let ((q (q:quat 0.25889468f0 -0.4580922f0 0.6231675f0 0.34003425f0))
        (r (q:quat -0.12738985f0 0.22540556f0 -0.30663133f0 -0.1673148f0))
        (o (q:id)))
    (is q:= (q:scale! o q -0.4920528f0) r)
    (is q:= o r)
    (is q:= (q:scale q -0.4920528f0) r)))

(define-test q/cross-product
  (let ((q1 (q:quat 0.8660254f0 0.5f0 0f0 0f0))
        (q2 (q:quat 0.8660254f0 0f0 0.5f0 0f0))
        (r (q:quat 0.75f0 0f0 0.4330127f0 0.25f0))
        (o (q:id)))
    (is q:= (q:cross! o q1 q2) r)
    (is q:= o r)
    (is q:= (q:cross q1 q2) r)))

(define-test q/dot-product
  (let ((q1 (q:quat -0.55014205f0 0.66294193f0 -0.44094658f0 0.1688292f0))
        (q2 (q:quat 0.5137224f0 0.83796954f0 -0.9853494f0 -0.3770373f0)))
    (is = (q:dot q1 q2) 0.64373636f0)))

(define-test q/conjugate
  (let ((q (q:quat 0.8754709f0 0.6535034f0 -0.11586404f0 -0.47056317f0))
        (r (q:quat 0.8754709f0 -0.6535034f0 0.11586404f0 0.47056317f0))
        (o (q:id)))
    (is q:= (q:conjugate! o q) r)
    (is q:= o r)
    (is q:= (q:conjugate q) r)))

(define-test q/length
  (is = (q:length q:+id+) 1f0)
  (is = (q:length (q:quat 0.23266816f0 -0.6098385f0 -0.81111765f0 0.11399269f0))
      1.0473508f0))

(define-test q/normalize
  (let ((q (q:quat -0.24647212f0 -0.812474f0 0.9715252f0 0.8300271f0))
        (r (q:quat -0.16065533f0 -0.52958643f0 0.6332591f0 0.5410279f0))
        (o (q:id)))
    (is q:= (q:normalize! o q) r)
    (is q:= o r)
    (is q:= (q:normalize q) r)
    (is q:= (q:normalize (q:quat 2f0 0f0 0f0 0f0)) q:+id+)))

(define-test q/negate
  (let ((q (q:quat 0.9858451f0 0.85955405f0 0.8707795f0 -0.36954784f0))
        (r (q:quat -0.9858451f0 -0.85955405f0 -0.8707795f0 0.36954784f0))
        (o (q:id)))
    (is q:= (q:negate! o q) r)
    (is q:= o r)
    (is q:= (q:negate q) r)))

(define-test q/inverse)
(let ((q (q:quat 0.19171429f0 -0.8571534f0 0.4451759f0 0.39651704f0))
      (r (q:quat 0.17012934f0 0.76064724f0 -0.39505392f0 -0.35187355f0))
      (o (q:id)))
  (is q:= (q:inverse! o q) r)
  (is q:= o r)
  (is q:= (q:inverse q) r))

(define-test q/rotate-euler
  (let ((oqx (q:id))
        (oqy (q:id))
        (oqz (q:id))
        (rqx (q:quat 0.86602545f0 0.5f0 0f0 0f0))
        (rqy (q:quat 0.86602545f0 0f0 0.5f0 0f0))
        (rqz (q:quat 0.86602545f0 0f0 0f0 0.5f0))
        (vx (v3:vec const:pi/3 0f0 0f0))
        (vy (v3:vec 0f0 const:pi/3 0f0))
        (vz (v3:vec 0f0 0f0 const:pi/3)))
    (true (q:= (q:rotate-euler! oqx q:+id+ vx) rqx))
    (true (q:= (q:rotate-euler! oqy q:+id+ vy) rqy))
    (true (q:= (q:rotate-euler! oqz q:+id+ vz) rqz))
    (true (q:= oqx rqx))
    (true (q:= oqy rqy))
    (true (q:= oqz rqz))
    (true (q:= (q:rotate-euler q:+id+ vx) rqx))
    (true (q:= (q:rotate-euler q:+id+ vy) rqy))
    (true (q:= (q:rotate-euler q:+id+ vz) rqz))))

(define-test q/mat4-convert
  (let ((q (q:rotate-euler q:+id+ (v3:vec const:pi/3 0f0 0f0)))
        (qo (q:id))
        (r (m4:mat 1f0 0f0 0f0 0f0
                   0f0 0.5f0 0.86602545f0 0f0
                   0f0 -0.86602545f0 0.5f0 0f0
                   0f0 0f0 0f0 1f0))
        (mo (m4:id)))
    (true (m4:= (q:to-mat4! mo q) r))
    (true (m4:= mo r))
    (true (m4:= (q:to-mat4 q) r))
    (true (q:= (q:from-mat4! qo r) q))
    (true (q:= qo q))
    (true (q:= (q:from-mat4 r) q))))

(define-test q/slerp
  (let ((q1 (q:quat -0.15230274f0 0.7359729f0 -0.27456188f0 -0.28505945f0))
        (q2 (q:quat 0.594954f0 0.030960321f0 -0.037411213f0 -0.02747035f0))
        (r (q:quat -0.5157237f0 0.4865686f0 -0.16367096f0 -0.17777666f0))
        (o (q:id)))
    (is q:= (q:slerp! o q1 q2 0.5f0) r)
    (is q:= o r)
    (is q:= (q:slerp q1 q2 0.5f0) r)))
