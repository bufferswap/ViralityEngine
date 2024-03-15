(in-package #:vorigin.test)

(define-test v3/sign
  (let ((o (v3:zero)))
    (is v3:= (v3:sign (v3:zero)) (v3:zero))
    (is v3:= (v3:sign (v3:vec 10f0 10f0 10f0)) (v3:vec 1f0 1f0 1f0))
    (is v3:= (v3:sign (v3:vec -10f0 -10f0 -10f0)) (v3:vec -1f0 -1f0 -1f0))
    (v3:sign! o (v3:zero))
    (is v3:= o (v3:zero))
    (v3:sign! o (v3:vec 10f0 10f0 10f0))
    (is v3:= o (v3:vec 1f0 1f0 1f0))
    (v3:sign! o (v3:vec -10f0 -10f0 -10f0))
    (is v3:= o (v3:vec -1f0 -1f0 -1f0))))

(define-test v3/fract
  (let ((o (v3:zero))
        (v (v3:vec 10.42f0 -10.42f0 0f0))
        (r (v3:vec 0.42f0 0.58f0 0f0)))
    (is v3:= (v3:fract (v3:zero)) (v3:zero))
    (is v3:= (v3:fract v) r)
    (v3:fract! o (v3:zero))
    (is v3:= o (v3:zero))
    (v3:fract! o v)
    (is v3:= o r)))

(define-test v3/copy
  (let ((v (v3:vec 1f0 2f0 3f0))
        (o (v3:zero)))
    (is v3:= (v3:copy! o v) v)
    (is v3:= o v)
    (is v3:= (v3:copy v) v)
    (isnt eq v (v3:copy v))))

(define-test v3/clamp
  (let ((v (v3:vec -1.5185602f0 0.3374052f0 1.5218115f0))
        (r (v3:vec -1f0 0.3374052f0 1f0))
        (o (v3:zero)))
    (is v3:= (v3:clamp-range! o v -1f0 1f0) r)
    (is v3:= o r)
    (is v3:= (v3:clamp-range v -1f0 1f0) r)
    (is v3:= (v3:clamp-range v
                             most-negative-single-float
                             most-positive-single-float)
        v)))

(define-test v3/zero
  (let ((v (v3:vec -0.72470546f0 0.57963276f0 0.8775625f0)))
    (is v3:= (v3:zero! v) v3:+zero+)
    (is v3:= v v3:+zero+)
    (is v3:= (v3:zero) v3:+zero+)))

(define-test v3/equality
  (let ((v1 (v3:vec 0.8598654f0 -0.4803753f0 -0.3822465f0))
        (v2 (v3:vec 1f-8 1f-8 1f-8)))
    (true (v3:= v1 v1))
    (true (v3:= (v3:+ v1 v2) v1))
    (true (v3:= v2 v3:+zero+))))

(define-test v3/add
  (let ((v1 (v3:vec 0.4110496f0 -0.87680984f0 -0.62870455f0))
        (v2 (v3:vec 0.1166687f0 0.42538047f0 0.7360425f0))
        (r (v3:vec 0.5277183f0 -0.45142937f0 0.10733795f0))
        (o (v3:zero)))
    (is v3:= (v3:+! o v1 v2) r)
    (is v3:= o r)
    (is v3:= (v3:+ v1 v2) r)
    (is v3:= (v3:+ v1 v3:+zero+) v1)
    (is v3:= (v3:+ v3:+zero+ v2) v2)))

(define-test v3/subtract
  (let ((v1 (v3:vec -0.16772795f0 -0.7287135f0 -0.8905144f0))
        (v2 (v3:vec -0.69658303f0 0.6168339f0 -0.7841997f0))
        (r (v3:vec 0.5288551f0 -1.3455474f0 -0.10631466f0))
        (o (v3:zero)))
    (is v3:= (v3:-! o v1 v2) r)
    (is v3:= o r)
    (is v3:= (v3:- v1 v2) r)
    (is v3:= (v3:- v1 v3:+zero+) v1)))

(define-test v3/hadamard-product
  (let ((v1 (v3:vec -0.6219859f0 -0.80110574f0 -0.06880522f0))
        (v2 (v3:vec 0.6687746f0 -0.21906853f0 0.14335585f0))
        (r (v3:vec -0.4159684f0 0.17549706f0 -0.00986363f0))
        (o (v3:zero)))
    (is v3:= (v3:*! o v1 v2) r)
    (is v3:= o r)
    (is v3:= (v3:* v1 v2) r)
    (is v3:= (v3:* v1 v3:+zero+) v3:+zero+)
    (is v3:= (v3:* v3:+zero+ v2) v3:+zero+)))

(define-test v3/hadamard-quotient
  (let ((v1 (v3:vec 0.9498384f0 0.4066379f0 -0.72961855f0))
        (v2 (v3:vec 0.32331443f0 0.17439032f0 -0.65894365f0))
        (r (v3:vec 2.9378164f0 2.3317688f0 1.1072549f0))
        (o (v3:zero)))
    (is v3:= (v3:/! o v1 v2) r)
    (is v3:= o r)
    (is v3:= (v3:/ v1 v2) r)
    (is v3:= (v3:/ v1 v3:+zero+) v3:+zero+)
    (is v3:= (v3:/ v3:+zero+ v2) v3:+zero+)))

(define-test v3/scalar-product
  (let ((v (v3:vec 0.82007027f0 -0.53582144f0 0.11559081f0))
        (r (v3:vec 0.7762602f0 -0.5071966f0 0.10941568f0))
        (o (v3:zero)))
    (is v3:= (v3:scale! o v 0.94657767f0) r)
    (is v3:= o r)
    (is v3:= (v3:scale v 0.94657767f0) r)))

(define-test v3/dot-product
  (is = (v3:dot (v3:vec -0.21361923f0 0.39387107f0 0.0043354034f0)
                (v3:vec -0.13104868f0 0.399935f0 0.62945867f0))
      0.1882463f0)
  (is = (v3:dot (v3:vec 1f0 0f0 0f0) (v3:vec 0f0 1f0 0f0)) 0f0)
  (is = (v3:dot (v3:vec 1f0 0f0 0f0) (v3:vec 0f0 0f0 1f0)) 0f0)
  (is = (v3:dot (v3:vec 0f0 1f0 0f0) (v3:vec 0f0 0f0 1f0)) 0f0)
  (is = (v3:dot (v3:vec 1f0 0f0 0f0) (v3:vec 1f0 0f0 0f0)) 1f0)
  (is = (v3:dot (v3:vec 1f0 0f0 0f0) (v3:vec -1f0 0f0 0f0)) -1f0))

(define-test v3/cross-product
  (let ((v1 (v3:vec 1f0 0f0 0f0))
        (v2 (v3:vec 0f0 1f0 0f0))
        (o (v3:zero)))
    (is v3:= (v3:cross! o v1 v2) (v3:vec 0f0 0f0 1f0))
    (is v3:= o (v3:vec 0f0 0f0 1f0))
    (is v3:= (v3:cross (v3:vec 1f0 0f0 0f0)
                       (v3:vec 0f0 1f0 0f0))
        (v3:vec 0f0 0f0 1f0))
    (is v3:= (v3:cross (v3:vec 1f0 0f0 0f0)
                       (v3:vec 0f0 0f0 1f0))
        (v3:vec 0f0 -1f0 0f0))
    (is v3:= (v3:cross (v3:vec 0f0 1f0 0f0)
                       (v3:vec 1f0 0f0 0f0))
        (v3:vec 0f0 0f0 -1f0))
    (is v3:= (v3:cross (v3:vec 0f0 1f0 0f0)
                       (v3:vec 0f0 0f0 1f0))
        (v3:vec 1f0 0f0 0f0))
    (is v3:= (v3:cross (v3:vec 0f0 0f0 1f0)
                       (v3:vec 1f0 0f0 0f0))
        (v3:vec 0f0 1f0 0f0))
    (is v3:= (v3:cross (v3:vec 0f0 0f0 1f0)
                       (v3:vec 0f0 1f0 0f0))
        (v3:vec -1f0 0f0 0f0))))

(define-test v3/length
  (is = (v3:length v3:+zero+) 0f0)
  (is = (v3:length (v3:vec 1f0 0f0 0f0)) 1f0)
  (is = (v3:length (v3:vec 0.32979298f0 0.2571392f0 0.19932675f0))
      0.46326572f0))

(define-test v3/normalize
  (let ((v (v3:vec -0.6589291f0 0.23270178f0 -0.1047523f0))
        (r (v3:vec -0.9325095f0 0.3293171f0 -0.14824435f0))
        (o (v3:zero)))
    (is v3:= (v3:normalize! o v) r)
    (is v3:= o r)
    (is v3:= (v3:normalize v) r)
    (is v3:= (v3:normalize (v3:vec 2f0 0f0 0f0)) (v3:vec 1f0 0f0 0f0))
    (is v3:= (v3:normalize (v3:vec 0f0 2f0 0f0)) (v3:vec 0f0 1f0 0f0))
    (is v3:= (v3:normalize (v3:vec 0f0 0f0 2f0)) (v3:vec 0f0 0f0 1f0))))

(define-test v3/round
  (let ((v (v3:vec -0.70498157f0 0.3615427f0 0.50702953f0))
        (r (v3:vec -1f0 0f0 1f0))
        (o (v3:zero)))
    (is v3:= (v3:round! o v) r)
    (is v3:= o r)
    (is v3:= (v3:round v) r)))

(define-test v3/abs
  (let ((v (v3:vec -0.4241562f0 -0.52400947f0 0.82413125f0))
        (r (v3:vec 0.4241562f0 0.52400947f0 0.82413125f0))
        (o (v3:zero)))
    (is v3:= (v3:abs! o v) r)
    (is v3:= o r)
    (is v3:= (v3:abs v) r)))

(define-test v3/negate
  (let ((v (v3:vec 0.7823446f0 0.95027566f0 -0.4147482f0))
        (r (v3:vec -0.7823446f0 -0.95027566f0 0.4147482f0))
        (o (v3:zero)))
    (is v3:= (v3:negate! o v) r)
    (is v3:= o r)
    (is v3:= (v3:negate v) r)))

(define-test v3/angle
  (let ((angle (v3:angle (v3:vec 0f0 1f0 0f0) (v3:vec 1f0 0f0 1f0))))
    (true (<= (abs (- angle const:pi/2)) 1f-5)))
  (let ((angle (v3:angle (v3:vec 1f0 1f0 0f0) (v3:vec 1f0 0f0 1f0))))
    (true (<= (abs (- angle const:pi/3)) 1f-5)))
  (let ((angle (v3:angle (v3:vec 1f0 0f0 0f0) (v3:vec 1f0 1f0 0f0))))
    (true (<= (abs (- angle const:pi/4)) 1f-5))))

(define-test v3/zero-predicate
  (true (v3:zero-p v3:+zero+))
  (true (v3:zero-p (v3:vec 0f0 0f0 0f0))))

(define-test v3/direction-equality
  (true (v3:direction= (v3:vec 0.0073252916f0 0f0 0f0)
                       (v3:vec 0.31148136f0 0f0 0f0)))
  (true (v3:direction= (v3:vec 0f0 0.6982585f0 0f0)
                       (v3:vec 0f0 0.72258794f0 0f0)))
  (true (v3:direction= (v3:vec 0f0 0f0 0.86798644f0)
                       (v3:vec 0f0 0f0 42384863f0))))

(define-test v3/parallel-predicate
  (true (v3:parallel-p (v3:vec 0.6883507f0 0f0 0f0)
                       (v3:vec -0.37808847f0 0f0 0f0)))
  (true (v3:parallel-p (v3:vec 0f0 -0.31525326f0 0f0)
                       (v3:vec 0f0 0.20765233f0 0f0)))
  (true (v3:parallel-p (v3:vec 0f0 0f0 0.18911958f0)
                       (v3:vec 0f0 0f0 -0.17581582f0))))

(define-test v3/lerp
  (let ((v1 (v3:vec 0.74485755f0 0.092342734f0 0.2982279f0))
        (v2 (v3:vec 0.19426346f0 0.9881369f0 0.64691556f0))
        (r (v3:vec 0.4695605f0 0.5402398f0 0.47257173f0))
        (o (v3:zero)))
    (is v3:= (v3:lerp! o v1 v2 0.5f0) r)
    (is v3:= o r)
    (is v3:= (v3:lerp v1 v2 0.5f0) r)
    (is v3:= (v3:lerp v1 v2 0f0) v1)
    (is v3:= (v3:lerp v1 v2 1f0) v2)))

(define-test v3/compare
  (let ((v1 (v3:vec 0.34003425f0 -0.4920528f0 0.8754709f0))
        (v2 (v3:vec 0.6535034f0 -0.11586404f0 -0.47056317f0))
        (v3 (v3:vec 0.9715252f0 0.8300271f0 0.9858451f0))
        (v4 (v3:vec 1f0 2f0 3f0))
        (v5 (v3:vec 2f0 3f0 4f0)))
    (true (v3:< v2 v3))
    (true (v3:<= v4 v4))
    (true (v3:<= v4 v5))
    (true (v3:> v3 v1))
    (true (v3:>= v4 v4))
    (true (v3:>= v5 v4))))

(define-test v3/min
  (let* ((v1 (v3:vec 0.98117805f0 0.06889212f0 0.32721102f0))
         (v2 (v3:vec 0.8774886f0 0.25179327f0 0.76311684f0))
         (r (v3:vec (aref v2 0) (aref v1 1) (aref v1 2)))
         (o (v3:zero)))
    (is v3:= (v3:min! o v1 v2) r)
    (is v3:= o r)
    (is v3:= (v3:min v1 v2) r)))

(define-test v3/max
  (let* ((v1 (v3:vec 0.64380646f0 0.38965714f0 0.2503655f0))
         (v2 (v3:vec 0.6341989f0 0.5274999f0 0.90044403f0))
         (r (v3:vec (aref v1 0) (aref v2 1) (aref v2 2)))
         (o (v3:zero)))
    (is v3:= (v3:max! o v1 v2) r)
    (is v3:= o r)
    (is v3:= (v3:max v1 v2) r)))
