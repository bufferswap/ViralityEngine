(in-package #:vorigin.test)

(define-test v4/copy
  (let ((v (v4:vec 1f0 2f0 3f0 4f0))
        (o (v4:zero)))
    (is v4:= (v4:copy! o v) v)
    (is v4:= o v)
    (is v4:= (v4:copy v) v)
    (isnt eq v (v4:copy v))))

(define-test v4/sign
  (let ((o (v4:zero)))
    (is v4:= (v4:sign (v4:zero)) (v4:zero))
    (is v4:= (v4:sign (v4:vec 10f0 10f0 10f0 10f0)) (v4:vec 1f0 1f0 1f0 1f0))
    (is v4:= (v4:sign (v4:vec -10f0 -10f0 -10f0 -10f0))
        (v4:vec -1f0 -1f0 -1f0 -1f0))
    (v4:sign! o (v4:zero))
    (is v4:= o (v4:zero))
    (v4:sign! o (v4:vec 10f0 10f0 10f0 10f0))
    (is v4:= o (v4:vec 1f0 1f0 1f0 1f0))
    (v4:sign! o (v4:vec -10f0 -10f0 -10f0 -10f0))
    (is v4:= o (v4:vec -1f0 -1f0 -1f0 -1f0))))

(define-test v4/fract
  (let ((o (v4:zero))
        (v (v4:vec 10.42f0 -10.42f0 0f0 0f0))
        (r (v4:vec 0.42f0 0.58f0 0f0 0f0)))
    (is v4:= (v4:fract (v4:zero)) (v4:zero))
    (is v4:= (v4:fract v) r)
    (v4:fract! o (v4:zero))
    (is v4:= o (v4:zero))
    (v4:fract! o v)
    (is v4:= o r)))

(define-test v4/clamp
  (let ((v (v4:vec -1.5185602f0 0.3374052f0 1.5218115f0 1.8188539f0))
        (r (v4:vec -1f0 0.3374052f0 1f0 1f0))
        (o (v4:zero)))
    (is v4:= (v4:clamp-range! o v -1f0 1f0) r)
    (is v4:= o r)
    (is v4:= (v4:clamp-range v -1f0 1f0) r)
    (is v4:= (v4:clamp-range v
                             most-negative-single-float
                             most-positive-single-float)
        v)))

(define-test v4/zero
  (let ((v (v4:vec -0.72470546f0 0.57963276f0 0.8775625f0 0.44206798f0)))
    (is v4:= (v4:zero! v) v4:+zero+)
    (is v4:= v v4:+zero+)
    (is v4:= (v4:zero) v4:+zero+)))

(define-test v4/equality
  (let ((v1 (v4:vec 0.8598654f0 -0.4803753f0 -0.3822465f0 0.2647184f0))
        (v2 (v4:vec 1f-8 1f-8 1f-8 1f-8)))
    (true (v4:= v1 v1))
    (true (v4:= (v4:+ v1 v2) v1))
    (true (v4:= v2 v4:+zero+))))

(define-test v4/add
  (let ((v1 (v4:vec 0.4110496f0 -0.87680984f0 -0.62870455f0 0.6163341f0))
        (v2 (v4:vec 0.1166687f0 0.42538047f0 0.7360425f0 0.19508076f0))
        (r (v4:vec 0.5277183f0 -0.45142937f0 0.10733795f0 0.81141484f0))
        (o (v4:zero)))
    (is v4:= (v4:+! o v1 v2) r)
    (is v4:= o r)
    (is v4:= (v4:+ v1 v2) r)
    (is v4:= (v4:+ v1 v4:+zero+) v1)
    (is v4:= (v4:+ v4:+zero+ v2) v2)))

(define-test v4/subtract
  (let ((v1 (v4:vec -0.16772795f0 -0.7287135f0 -0.8905144f0 0.55699535f0))
        (v2 (v4:vec -0.69658303f0 0.6168339f0 -0.7841997f0 0.094441175f0))
        (r (v4:vec 0.5288551f0 -1.3455474f0 -0.10631466f0 0.46255416f0))
        (o (v4:zero)))
    (is v4:= (v4:-! o v1 v2) r)
    (is v4:= o r)
    (is v4:= (v4:- v1 v2) r)
    (is v4:= (v4:- v1 v4:+zero+) v1)))

(define-test v4/hadamard-product
  (let ((v1 (v4:vec -0.6219859f0 -0.80110574f0 -0.06880522f0 0.37676394f0))
        (v2 (v4:vec 0.6687746f0 -0.21906853f0 0.14335585f0 0.093762994f0))
        (r (v4:vec -0.4159684f0 0.17549706f0 -0.00986363f0 0.035326514f0))
        (o (v4:zero)))
    (is v4:= (v4:*! o v1 v2) r)
    (is v4:= o r)
    (is v4:= (v4:* v1 v2) r)
    (is v4:= (v4:* v1 v4:+zero+) v4:+zero+)
    (is v4:= (v4:* v4:+zero+ v2) v4:+zero+)))

(define-test v4/hadamard-quotient
  (let ((v1 (v4:vec 0.9498384f0 0.4066379f0 -0.72961855f0 0.9857626f0))
        (v2 (v4:vec 0.32331443f0 0.17439032f0 -0.65894365f0 0.91501355f0))
        (r (v4:vec 2.9378164f0 2.3317688f0 1.1072549f0 1.0773202f0))
        (o (v4:zero)))
    (is v4:= (v4:/! o v1 v2) r)
    (is v4:= o r)
    (is v4:= (v4:/ v1 v2) r)
    (is v4:= (v4:/ v1 v4:+zero+) v4:+zero+)
    (is v4:= (v4:/ v4:+zero+ v2) v4:+zero+)))

(define-test v4/scalar-product
  (let ((v (v4:vec 0.82007027f0 -0.53582144f0 0.11559081f0 0.31522608f0))
        (r (v4:vec 0.7762602f0 -0.5071966f0 0.10941568f0 0.29838598f0))
        (o (v4:zero)))
    (is v4:= (v4:scale! o v 0.94657767f0) r)
    (is v4:= o r)
    (is v4:= (v4:scale v 0.94657767f0) r)))

(define-test v4/dot-product
  (is = (v4:dot (v4:vec -0.21361923f0 0.39387107f0 0.0043354034f0 0.8267517f0)
                (v4:vec -0.13104868f0 0.399935f0 0.62945867f0 0.44206798f0))
      0.55372673f0)
  (is = (v4:dot (v4:vec 1f0 0f0 0f0 0f0) (v4:vec 0f0 1f0 0f0 0f0)) 0f0)
  (is = (v4:dot (v4:vec 1f0 0f0 0f0 0f0) (v4:vec 0f0 0f0 1f0 0f0)) 0f0)
  (is = (v4:dot (v4:vec 0f0 1f0 0f0 0f0) (v4:vec 0f0 0f0 1f0 0f0)) 0f0)
  (is = (v4:dot (v4:vec 1f0 0f0 0f0 0f0) (v4:vec 1f0 0f0 0f0 0f0)) 1f0)
  (is = (v4:dot (v4:vec 1f0 0f0 0f0 0f0) (v4:vec -1f0 0f0 0f0 0f0)) -1f0))

(define-test v4/length
  (is = (v4:length v4:+zero+) 0f0)
  (is = (v4:length (v4:vec 1f0 0f0 0f0 0f0)) 1f0)
  (is = (v4:length (v4:vec 0.32979298f0 0.2571392f0 0.19932675f0 0.2647184f0))
      0.5335644f0))

(define-test v4/normalize
  (let ((v (v4:vec -0.6589291f0 0.23270178f0 -0.1047523f0 0.6163341f0))
        (r (v4:vec -0.70274895f0 0.24817683f0 -0.1117185f0 0.6573213f0))
        (o (v4:zero)))
    (is v4:= (v4:normalize! o v) r)
    (is v4:= o r)
    (is v4:= (v4:normalize v) r)
    (is v4:= (v4:normalize (v4:vec 2f0 0f0 0f0 0f0)) (v4:vec 1f0 0f0 0f0 0f0))
    (is v4:= (v4:normalize (v4:vec 0f0 2f0 0f0 0f0)) (v4:vec 0f0 1f0 0f0 0f0))
    (is v4:= (v4:normalize (v4:vec 0f0 0f0 2f0 0f0)) (v4:vec 0f0 0f0 1f0 0f0))))

(define-test v4/round
  (let ((v (v4:vec -0.70498157f0 0.3615427f0 0.50702953f0 0.19508076f0))
        (r (v4:vec -1f0 0f0 1f0 0f0))
        (o (v4:zero)))
    (is v4:= (v4:round! o v) r)
    (is v4:= o r)
    (is v4:= (v4:round v) r)))

(define-test v4/abs
  (let ((v (v4:vec -0.4241562f0 -0.52400947f0 0.82413125f0 -0.094441175f0))
        (r (v4:vec 0.4241562f0 0.52400947f0 0.82413125f0 0.094441175f0))
        (o (v4:zero)))
    (is v4:= (v4:abs! o v) r)
    (is v4:= o r)
    (is v4:= (v4:abs v) r)))

(define-test v4/negate
  (let ((v (v4:vec 0.7823446f0 0.95027566f0 -0.4147482f0 0.55699635f0))
        (r (v4:vec -0.7823446f0 -0.95027566f0 0.4147482f0 -0.55699635f0))
        (o (v4:zero)))
    (is v4:= (v4:negate! o v) r)
    (is v4:= o r)
    (is v4:= (v4:negate v) r)))

(define-test v4/zero-predicate
  (true (v4:zero-p v4:+zero+))
  (true (v4:zero-p (v4:vec 0f0 0f0 0f0 0f0))))

(define-test v4/lerp
  (let ((v1 (v4:vec 0.74485755f0 0.092342734f0 0.2982279f0 0.093762994f0))
        (v2 (v4:vec 0.19426346f0 0.9881369f0 0.64691556f0 0.9857626f0))
        (r (v4:vec 0.4695605f0 0.5402398f0 0.47257173f0 0.5397628f0))
        (o (v4:zero)))
    (is v4:= (v4:lerp! o v1 v2 0.5f0) r)
    (is v4:= o r)
    (is v4:= (v4:lerp v1 v2 0.5f0) r)
    (is v4:= (v4:lerp v1 v2 0f0) v1)
    (is v4:= (v4:lerp v1 v2 1f0) v2)))

(define-test v4/compare
  (let ((v1 (v4:vec 0.34003425f0 -0.4920528f0 0.8754709f0 0.91501355f0))
        (v2 (v4:vec 0.6535034f0 -0.11586404f0 -0.47056317f0 0.91292254f0))
        (v3 (v4:vec 0.9715252f0 0.8300271f0 0.9858451f0 0.929777f0))
        (v4 (v4:vec 1f0 2f0 3f0 4f0))
        (v5 (v4:vec 2f0 3f0 4f0 5f0)))
    (true (v4:< v2 v3))
    (true (v4:<= v4 v4))
    (true (v4:<= v4 v5))
    (true (v4:> v3 v1))
    (true (v4:>= v4 v4))
    (true (v4:>= v5 v4))))

(define-test v4/min
  (let* ((v1 (v4:vec 0.98117805f0 0.06889212f0 0.32721102f0 0.93538976f0))
         (v2 (v4:vec 0.8774886f0 0.25179327f0 0.76311684f0 0.31522608f0))
         (r (v4:vec (aref v2 0) (aref v1 1) (aref v1 2) (aref v2 3)))
         (o (v4:zero)))
    (is v4:= (v4:min! o v1 v2) r)
    (is v4:= o r)
    (is v4:= (v4:min v1 v2) r)))

(define-test v4/max
  (let* ((v1 (v4:vec 0.64380646f0 0.38965714f0 0.2503655f0 0.45167792f0))
         (v2 (v4:vec 0.6341989f0 0.5274999f0 0.90044403f0 0.9411855f0))
         (r (v4:vec (aref v1 0) (aref v2 1) (aref v2 2) (aref v2 3)))
         (o (v4:zero)))
    (is v4:= (v4:max! o v1 v2) r)
    (is v4:= o r)
    (is v4:= (v4:max v1 v2) r)))
