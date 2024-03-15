(in-package #:vorigin.test)

(define-test v2/copy
  (let ((v (v2:vec 1f0 2f0))
        (o (v2:zero)))
    (is v2:= (v2:copy! o v) v)
    (is v2:= o v)
    (is v2:= (v2:copy v) v)
    (isnt eq v (v2:copy v))))

(define-test v2/sign
  (let ((o (v2:zero)))
    (is v2:= (v2:sign (v2:zero)) (v2:zero))
    (is v2:= (v2:sign (v2:vec 10f0 10f0)) (v2:vec 1f0 1f0))
    (is v2:= (v2:sign (v2:vec -10f0 -10f0)) (v2:vec -1f0 -1f0))
    (v2:sign! o (v2:zero))
    (is v2:= o (v2:zero))
    (v2:sign! o (v2:vec 10f0 10f0))
    (is v2:= o (v2:vec 1f0 1f0))
    (v2:sign! o (v2:vec -10f0 -10f0))
    (is v2:= o (v2:vec -1f0 -1f0))))

(define-test v2/fract
  (let ((o (v2:zero))
        (v (v2:vec 10.42f0 -10.42f0))
        (r (v2:vec 0.42f0 0.58f0)))
    (is v2:= (v2:fract (v2:zero)) (v2:zero))
    (is v2:= (v2:fract v) r)
    (v2:fract! o (v2:zero))
    (is v2:= o (v2:zero))
    (v2:fract! o v)
    (is v2:= o r)))

(define-test v2/clamp
  (let ((v (v2:vec -1.5185602f0 0.3374052f0))
        (r (v2:vec -1f0 0.3374052f0))
        (o (v2:zero)))
    (is v2:= (v2:clamp-range! o v -1f0 1f0) r)
    (is v2:= o r)
    (is v2:= (v2:clamp-range v -1f0 1f0) r)
    (is v2:= (v2:clamp-range v
                             most-negative-single-float
                             most-positive-single-float)
        v)))

(define-test v2/zero
  (let ((v (v2:vec -0.72470546f0 0.57963276f0)))
    (is v2:= (v2:zero! v) v2:+zero+)
    (is v2:= v v2:+zero+)
    (is v2:= (v2:zero) v2:+zero+)))

(define-test v2/equality
  (let ((v1 (v2:vec 0.8598654f0 -0.4803753f0))
        (v2 (v2:vec 1f-8 1f-8)))
    (true (v2:= v1 v1))
    (true (v2:= (v2:+ v1 v2) v1))
    (true (v2:= v2 v2:+zero+))))

(define-test v2/add
  (let ((v1 (v2:vec 0.4110496f0 -0.87680984f0))
        (v2 (v2:vec 0.1166687f0 0.42538047f0))
        (r (v2:vec 0.5277183f0 -0.45142937f0))
        (o (v2:zero)))
    (is v2:= (v2:+! o v1 v2) r)
    (is v2:= o r)
    (is v2:= (v2:+ v1 v2) r)
    (is v2:= (v2:+ v1 v2:+zero+) v1)
    (is v2:= (v2:+ v2:+zero+ v2) v2)))

(define-test v2/subtract
  (let ((v1 (v2:vec -0.16772795f0 -0.7287135f0))
        (v2 (v2:vec -0.69658303f0 0.6168339f0))
        (r (v2:vec 0.5288551f0 -1.3455474f0))
        (o (v2:zero)))
    (is v2:= (v2:-! o v1 v2) r)
    (is v2:= o r)
    (is v2:= (v2:- v1 v2) r)
    (is v2:= (v2:- v1 v2:+zero+) v1)))

(define-test v2/hadamard-product
  (let ((v1 (v2:vec -0.6219859f0 -0.80110574f0))
        (v2 (v2:vec 0.6687746f0 -0.21906853f0))
        (r (v2:vec -0.4159684f0 0.17549706f0))
        (o (v2:zero)))
    (is v2:= (v2:*! o v1 v2) r)
    (is v2:= o r)
    (is v2:= (v2:* v1 v2) r)
    (is v2:= (v2:* v1 v2:+zero+) v2:+zero+)
    (is v2:= (v2:* v2:+zero+ v2) v2:+zero+)))

(define-test v2/hadamard-quotient
  (let ((v1 (v2:vec 0.9498384f0 0.4066379f0))
        (v2 (v2:vec 0.32331443f0 0.17439032f0))
        (r (v2:vec 2.9378164f0 2.3317688f0))
        (o (v2:zero)))
    (is v2:= (v2:/! o v1 v2) r)
    (is v2:= o r)
    (is v2:= (v2:/ v1 v2) r)
    (is v2:= (v2:/ v1 v2:+zero+) v2:+zero+)
    (is v2:= (v2:/ v2:+zero+ v2) v2:+zero+)))

(define-test v2/scalar-product
  (let ((v (v2:vec 0.82007027f0 -0.53582144f0))
        (r (v2:vec 0.7762602f0 -0.5071966f0))
        (o (v2:zero)))
    (is v2:= (v2:scale! o v 0.94657767f0) r)
    (is v2:= o r)
    (is v2:= (v2:scale v 0.94657767f0) r)))

(define-test v2/dot-product
  (is = (v2:dot (v2:vec -0.21361923f0 0.39387107f0)
                (v2:vec -0.13104868f0 0.399935f0))
      0.18551734f0)
  (is = (v2:dot (v2:vec 1f0 0f0) (v2:vec 0f0 1f0)) 0f0)
  (is = (v2:dot (v2:vec 1f0 0f0) (v2:vec 1f0 0f0)) 1f0)
  (is = (v2:dot (v2:vec 1f0 0f0) (v2:vec -1f0 0f0)) -1f0))

(define-test v2/length
  (is = (v2:length v2:+zero+) 0f0)
  (is = (v2:length (v2:vec 1f0 0f0)) 1f0)
  (is = (v2:length (v2:vec 0.32979298f0 0.2571392f0)) 0.4181913f0))

(define-test v2/normalize
  (let ((v (v2:vec -0.6589291f0 0.23270178f0))
        (r (v2:vec -0.942928f0 0.3329964f0))
        (o (v2:zero)))
    (is v2:= (v2:normalize! o v) r)
    (is v2:= o r)
    (is v2:= (v2:normalize v) r)
    (is v2:= (v2:normalize (v2:vec 2f0 0f0)) (v2:vec 1f0 0f0))
    (is v2:= (v2:normalize (v2:vec 0f0 2f0)) (v2:vec 0f0 1f0))
    (is v2:= (v2:normalize (v2:vec 0f0 0f0)) (v2:vec 0f0 0f0))))

(define-test v2/round
  (let ((v (v2:vec -0.70498157f0 0.3615427f0))
        (r (v2:vec -1f0 0f0))
        (o (v2:zero)))
    (is v2:= (v2:round! o v) r)
    (is v2:= o r)
    (is v2:= (v2:round v) r)))

(define-test v2/abs
  (let ((v (v2:vec -0.4241562f0 -0.52400947f0))
        (r (v2:vec 0.4241562f0 0.52400947f0))
        (o (v2:zero)))
    (is v2:= (v2:abs! o v) r)
    (is v2:= o r)
    (is v2:= (v2:abs v) r)))

(define-test v2/negate
  (let ((v (v2:vec 0.7823446f0 0.95027566f0))
        (r (v2:vec -0.7823446f0 -0.95027566f0))
        (o (v2:zero)))
    (is v2:= (v2:negate! o v) r)
    (is v2:= o r)
    (is v2:= (v2:negate v) r)))

(define-test v2/angle
  (let ((angle (v2:angle (v2:vec 0f0 1f0) (v2:vec 1f0 0f0))))
    (true (<= (abs (- angle const:pi/2)) 1f-7)))
  (let ((angle (v2:angle (v2:vec 1f0 0f0) (v2:vec 1f0 1f0))))
    (true (<= (abs (- angle const:pi/4)) 1f-7))))

(define-test v2/zero-predicate
  (true (v2:zero-p v2:+zero+))
  (true (v2:zero-p (v2:vec 0f0 0f0))))

(define-test v2/direction-equality
  (true (v2:direction= (v2:vec 0.0073252916f0 0f0) (v2:vec 0.31148136f0 0f0)))
  (true (v2:direction= (v2:vec 0f0 0.6982585f0) (v2:vec 0f0 0.72258794f0))))

(define-test v2/lerp
  (let ((v1 (v2:vec 0.74485755f0 0.092342734f0))
        (v2 (v2:vec 0.19426346f0 0.9881369f0))
        (r (v2:vec 0.4695605f0 0.5402398f0))
        (o (v2:zero)))
    (is v2:= (v2:lerp! o v1 v2 0.5f0) r)
    (is v2:= o r)
    (is v2:= (v2:lerp v1 v2 0.5f0) r)
    (is v2:= (v2:lerp v1 v2 0f0) v1)
    (is v2:= (v2:lerp v1 v2 1f0) v2)))

(define-test v2/compare
  (let ((v1 (v2:vec 0.34003425f0 -0.4920528f0))
        (v2 (v2:vec 0.6535034f0 -0.11586404f0))
        (v3 (v2:vec 0.9715252f0 0.8300271f0))
        (v4 (v2:vec 1f0 2f0))
        (v5 (v2:vec 3f0 4f0)))
    (true (v2:< v2 v3))
    (true (v2:<= v4 v4))
    (true (v2:<= v4 v5))
    (true (v2:> v3 v1))
    (true (v2:>= v4 v4))
    (true (v2:>= v5 v4))))

(define-test v2/min
  (let* ((v1 (v2:vec 0.98117805f0 0.06889212f0))
         (v2 (v2:vec 0.8774886f0 0.25179327f0))
         (r (v2:vec (aref v2 0) (aref v1 1)))
         (o (v2:zero)))
    (is v2:= (v2:min! o v1 v2) r)
    (is v2:= o r)
    (is v2:= (v2:min v1 v2) r)))

(define-test v2/max
  (let* ((v1 (v2:vec 0.64380646f0 0.38965714f0))
         (v2 (v2:vec 0.6341989f0 0.5274999f0))
         (r (v2:vec (aref v1 0) (aref v2 1)))
         (o (v2:zero)))
    (is v2:= (v2:max! o v1 v2) r)
    (is v2:= o r)
    (is v2:= (v2:max v1 v2) r)))
