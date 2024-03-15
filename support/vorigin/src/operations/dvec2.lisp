(in-package #:vorigin.dvec2)

(u:fn-> = (vec vec &key (:rel u:f64) (:abs u:f64)) boolean)
(declaim (inline =))
(defun = (vec1 vec2 &key (rel 1d-7) (abs rel))
  "Compare vectors VEC1 and VEC2 for equality. REL and ABS are the relative and absolute tolerances
to compare by, and should be tuned specially for the application domain."
  (declare (optimize speed))
  (com:cwcmp 2 (vec1 vec2) (com:= vec1 vec2 rel abs)))

(u:fn-> zero! (vec) vec)
(declaim (inline zero!))
(defun zero! (vec)
  "Modify the vector VEC by setting each of its components to zero."
  (declare (optimize speed))
  (com:cwset 2 vec nil 0d0)
  vec)

(u:fn-> zero () vec)
(declaim (inline zero))
(defun zero ()
  "Construct a fresh vector with each component set to zero."
  (declare (optimize speed))
  (vec 0d0 0d0))

(u:fn-> zero-p (vec) boolean)
(declaim (inline zero-p))
(defun zero-p (vec)
  "Check whether or not the input vector is a zero vector."
  (declare (optimize speed))
  (= vec +zero+))

(u:fn-> ones! (vec) vec)
(declaim (inline ones!))
(defun ones! (vec)
  "Modify the vector VEC by setting each of its components to one."
  (declare (optimize speed))
  (com:cwset 2 vec nil 1d0)
  vec)

(u:fn-> ones () vec)
(declaim (inline ones))
(defun ones ()
  "Construct a fresh vector with each component set to one."
  (declare (optimize speed))
  (vec 1d0 1d0))

(u:fn-> uniform! (vec u:f64) vec)
(declaim (inline uniform!))
(defun uniform! (vec value)
  "Modify the vector VEC by setting each of its components to VALUE."
  (declare (optimize speed))
  (com:cwset 2 vec nil value)
  vec)

(u:fn-> uniform (u:f64) vec)
(declaim (inline uniform))
(defun uniform (value)
  "Construct a fresh vector with each component set to VALUE."
  (declare (optimize speed))
  (vec value value))

(u:fn-> random! (vec u:f64 u:f64) vec)
(declaim (inline random!))
(defun random! (out min max)
  "Modify vector VEC to have a random value for each of its components, The range of each component
is bounded by MIN and MAX."
  (declare (optimize speed))
  (let ((diff (cl:- max min)))
    (com:cwset 2 out nil (cl:+ min (cl:random diff))))
  out)

(u:fn-> random (u:f64 u:f64) vec)
(declaim (inline random))
(defun random (min max)
  "Construct a fresh vector with random elements. The range of each component is bounded by MIN and
MAX."
  (declare (optimize speed))
  (random! (zero) min max))

(u:fn-> copy! (vec vec) vec)
(declaim (inline copy!))
(defun copy! (out vec)
  "Modify vector OUT by copying the components of vector VEC into it."
  (declare (optimize speed))
  (com:cwset 2 out vec vec)
  out)

(u:fn-> copy (vec) vec)
(declaim (inline copy))
(defun copy (vec)
  "Construct a fresh vector that is a copy of vector VEC."
  (declare (optimize speed))
  (copy! (zero) vec))

(u:fn-> sign! (vec vec) vec)
(declaim (inline sign!))
(defun sign! (out vec)
  "Modify vector OUT to have its components represent the sign of each component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (signum vec))
  out)

(u:fn-> sign (vec) vec)
(declaim (inline sign))
(defun sign (vec)
  "Construct a fresh vector that has its components represent the sign of each component of vector
VEC."
  (declare (optimize speed))
  (sign! (zero) vec))

(u:fn-> fract! (vec vec) vec)
(declaim (inline fract!))
(defun fract! (out vec)
  "Modify vector OUT to have its components contain the fractional portion of the components in
vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:- vec (ffloor vec)))
  out)

(u:fn-> fract (vec) vec)
(declaim (inline fract))
(defun fract (vec)
  "Construct a fresh vector that has its components contain the fractional portion of the components
in vector VEC."
  (declare (optimize speed))
  (fract! (zero) vec))

(u:fn-> clamp! (vec vec vec vec) vec)
(declaim (inline clamp!))
(defun clamp! (out vec min max)
  "Modify vector OUT to have its components represent the components of vector VEC, bounded by the
components of vectors MIN and MAX."
  (declare (optimize speed))
  (com:cwset 2 out (vec min max) (u:clamp vec min max))
  out)

(u:fn-> clamp (vec vec vec) vec)
(declaim (inline clamp))
(defun clamp (vec min max)
  "Construct a fresh vector that has the components of vector VEC bounded by the components of
vectors MIN and MAX."
  (declare (optimize speed))
  (clamp! (zero) vec min max))

(u:fn-> clamp-range! (vec vec u:f64 u:f64) vec)
(declaim (inline clamp-range!))
(defun clamp-range! (out vec min max)
  "Modify vector OUT to have its components represent the components of vector VEC, bounded by MIN
and MAX."
  (declare (optimize speed))
  (com:cwset 2 out vec (u:clamp vec min max))
  out)

(u:fn-> clamp-range (vec u:f64 u:f64) vec)
(declaim (inline clamp-range))
(defun clamp-range (vec min max)
  "Construct a fresh vector that has the components of vector VEC bounded by MIN and MAX."
  (declare (optimize speed))
  (clamp-range! (zero) vec min max))

(u:fn-> +! (vec vec vec) vec)
(declaim (inline +!))
(defun +! (out vec1 vec2)
  "Modify vector OUT by performing component-wise addition of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:+ vec1 vec2))
  out)

(u:fn-> + (vec vec) vec)
(declaim (inline +))
(defun + (vec1 vec2)
  "Construct a fresh vector by performing component-wise addition of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (+! (zero) vec1 vec2))

(u:fn-> -! (vec vec vec) vec)
(declaim (inline -!))
(defun -! (out vec1 vec2)
  "Modify vector OUT by performing component-wise subtraction of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:- vec1 vec2))
  out)

(u:fn-> - (vec vec) vec)
(declaim (inline -))
(defun - (vec1 vec2)
  "Construct a fresh vector by performing component-wise substraction of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (-! (zero) vec1 vec2))

(u:fn-> *! (vec vec vec) vec)
(declaim (inline *!))
(defun *! (out vec1 vec2)
  "Modify vector OUT by performing component-wise multiplication of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:* vec1 vec2))
  out)

(u:fn-> * (vec vec) vec)
(declaim (inline *))
(defun * (vec1 vec2)
  "Construct a fresh vector by performing component-wise multiplication of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (*! (zero) vec1 vec2))

(u:fn-> /! (vec vec vec) vec)
(declaim (inline /!))
(defun /! (out vec1 vec2)
  "Modify vector OUT by performing component-wise division of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (if (zerop vec2) 0d0 (cl:/ vec1 vec2)))
  out)

(u:fn-> / (vec vec) vec)
(declaim (inline /))
(defun / (vec1 vec2)
  "Construct a fresh vector by performing component-wise division of vectors VEC1 and VEC2."
  (declare (optimize speed))
  (/! (zero) vec1 vec2))

(u:fn-> scale! (vec vec u:f64) vec)
(declaim (inline scale!))
(defun scale! (out vec scalar)
  "Modify vector OUT by adding the scalar SCALAR to each component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:* vec scalar))
  out)

(u:fn-> scale (vec u:f64) vec)
(declaim (inline scale))
(defun scale (vec scalar)
  "Construct a fresh vector by adding the scalar SCALAR to each component of vector VEC."
  (declare (optimize speed))
  (scale! (zero) vec scalar))

(u:fn-> invert! (vec vec) vec)
(declaim (inline invert!))
(defun invert! (out vec)
  "Modify vector OUT to have each component be the inverted component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (if (zerop vec) 0d0 (cl:/ vec)))
  out)

(u:fn-> invert (vec) vec)
(declaim (inline invert))
(defun invert (vec)
  "Construct a fresh vector with each component being the inverted component of vector VEC."
  (declare (optimize speed))
  (invert! (zero) vec))

(u:fn-> dot (vec vec) u:f64)
(declaim (inline dot))
(defun dot (vec1 vec2)
  "Compute the dot product of vectors VEC1 and VEC2. Returns a scalar."
  (with-components ((v1 vec1) (v2 vec2))
    (cl:+ (cl:* v1x v2x) (cl:* v1y v2y))))

(u:fn-> length-squared (vec) u:f64)
(declaim (inline length-squared))
(defun length-squared (vec)
  "Compute the squared length of vector VEC."
  (with-components ((v vec))
    (cl:+ (cl:expt vx 2) (cl:expt vy 2))))

(u:fn-> length (vec) u:f64)
(declaim (inline length))
(defun length (vec)
  "Compute the length of vector VEC. Returns a scalar."
  (cl:sqrt (length-squared vec)))

(u:fn-> normalize! (vec vec) vec)
(declaim (inline normalize!))
(defun normalize! (out vec)
  "Modify vector OUT to to be the result of normalizing vector VEC to be of unit length."
  (declare (optimize speed))
  (let ((length (length vec)))
    (unless (zerop length)
      (scale! out vec (cl:/ length))))
  out)

(u:fn-> normalize (vec) vec)
(declaim (inline normalize))
(defun normalize (vec)
  "Construct a fresh vector that is the result of normalizing vector VEC to be of unit length."
  (declare (optimize speed))
  (normalize! (zero) vec))

(u:fn-> round! (vec vec) vec)
(declaim (inline round!))
(defun round! (out vec)
  "Modify vector OUT to have its components be the result of rounding the components of vector VEC
to the nearest whole number."
  (com:cwset 2 out vec (fround vec))
  out)

(u:fn-> round (vec) vec)
(declaim (inline round))
(defun round (vec)
  "Construct a fresh vector that is the result of rounding the components of vector VEC to the
nearest whole number."
  (round! (zero) vec))

(u:fn-> abs! (vec vec) vec)
(declaim (inline abs!))
(defun abs! (out vec)
  "Modify vector OUT to have the absolute value of each component of vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:abs vec))
  out)

(u:fn-> abs (vec) vec)
(declaim (inline abs))
(defun abs (vec)
  "Construct a fresh vector to have the absolute value of each component of vector VEC."
  (declare (optimize speed))
  (abs! (zero) vec))

(u:fn-> negate! (vec vec) vec)
(declaim (inline negate!))
(defun negate! (out vec)
  "Modify vector OUT to have the components of vector VEC with their signs negated."
  (declare (optimize speed))
  (scale! out vec -1d0))

(u:fn-> negate (vec) vec)
(declaim (inline negate))
(defun negate (vec)
  "Construct a fresh vector with the components of vector VEC with their signs negated."
  (declare (optimize speed))
  (negate! (zero) vec))

(u:fn-> angle (vec vec) u:f64)
(declaim (inline angle))
(defun angle (vec1 vec2)
  "Compute the angle in radians between the vectors VEC1 and VEC2."
  (let ((dot (dot vec1 vec2))
        (m*m (cl:* (length vec1) (length vec2))))
    (if (zerop m*m)
        0d0
        (cl:acos (the (u:f64 -1d0 1d0) (cl:/ dot m*m))))))

(u:fn-> direction= (vec vec &key (:rel u:f64) (:abs u:f64)) boolean)
(declaim (inline direction=))
(defun direction= (vec1 vec2 &key (rel 1d-7) (abs rel))
  "Check whether or not vectors VEC1 and VEC2 are facing in the same direction."
  (declare (optimize speed))
  (com:= (dot (normalize vec1) (normalize vec2)) 1d0 rel abs))

(u:fn-> parallel-p (vec vec &key (:rel u:f64) (:abs u:f64)) boolean)
(declaim (inline parallel-p))
(defun parallel-p (vec1 vec2 &key (rel 1d-7) (abs rel))
  "Check whether or not vectors VEC1 and VEC2 are parallel to each other."
  (declare (optimize speed))
  (com:= (cl:abs (dot (normalize vec1) (normalize vec2))) 1d0 rel abs))

(u:fn-> lerp! (vec vec vec u:f64) vec)
(declaim (inline lerp!))
(defun lerp! (out vec1 vec2 factor)
  "Modify vector OUT with the result of linearly interpolating between vectors VEC1 and VEC2 by
FACTOR."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (u:lerp factor vec1 vec2))
  out)

(u:fn-> lerp (vec vec u:f64) vec)
(declaim (inline lerp))
(defun lerp (vec1 vec2 factor)
  "Construct a fresh vector that is the result of linearly interpolating between vectors VEC1 and
VEC2 by FACTOR."
  (declare (optimize speed))
  (lerp! (zero) vec1 vec2 factor))

(u:fn-> < (vec vec &optional (member :and :or)) boolean)
(declaim (inline <))
(defun < (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is less than the respective components of
vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:< vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:< vec1 vec2))))

(u:fn-> <= (vec vec &optional (member :and :or)) boolean)
(declaim (inline <=))
(defun <= (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is less than or equal to the respective
components of vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:<= vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:<= vec1 vec2))))

(u:fn-> > (vec vec &optional (member :and :or)) boolean)
(declaim (inline >))
(defun > (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is greater than the respective components of
vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:> vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:> vec1 vec2))))

(u:fn-> >= (vec vec &optional (member :and :or)) boolean)
(declaim (inline >=))
(defun >= (vec1 vec2 &optional (op :and))
  "Check whether or not each component of vector VEC1 is greater than or equal to the respective
components of vector VEC2."
  (declare (optimize speed))
  (if (eq op :and)
      (com:cwcmp 2 (vec1 vec2) (cl:>= vec1 vec2))
      (com:cwcmp-or 2 (vec1 vec2) (cl:>= vec1 vec2))))

(u:fn-> min! (vec vec vec) vec)
(declaim (inline min!))
(defun min! (out vec1 vec2)
  "Modify vector OUT to have the minimum value for each component in vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:min vec1 vec2))
  out)

(u:fn-> min (vec vec) vec)
(declaim (inline min))
(defun min (vec1 vec2)
  "Construct a fresh vector which has the minimum value of each component in vectors VEC1 and VEC2."
  (declare (optimize speed))
  (min! (zero) vec1 vec2))

(u:fn-> max! (vec vec vec) vec)
(declaim (inline max!))
(defun max! (out vec1 vec2)
  "Modify vector OUT to have the maximum value for each component in vectors VEC1 and VEC2."
  (declare (optimize speed))
  (com:cwset 2 out (vec1 vec2) (cl:max vec1 vec2))
  out)

(u:fn-> max (vec vec) vec)
(declaim (inline max))
(defun max (vec1 vec2)
  "Construct a fresh vector which has the maximum value of each component in vectors VEC1 and VEC2."
  (declare (optimize speed))
  (max! (zero) vec1 vec2))

(u:fn-> radians! (vec vec) vec)
(declaim (inline radians!))
(defun radians! (out vec)
  "Modify vector OUT to convert the components in vector VEC, which are assumed to be in degree
units, to radian units."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:* vec const:+deg/double+))
  out)

(u:fn-> radians (vec) vec)
(declaim (inline radians))
(defun radians (vec)
  "Construct a fresh vector with the components in vector VEC, which are assumed to be in degree
units, converted to radian units."
  (declare (optimize speed))
  (radians! (zero) vec))

(u:fn-> degrees! (vec vec) vec)
(declaim (inline degrees!))
(defun degrees! (out vec)
  "Modify vector OUT to convert the components in vector VEC, which are assumed to be in radian
units, to degree units."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:* vec const:+rad/double+))
  out)

(u:fn-> degrees (vec) vec)
(declaim (inline degrees))
(defun degrees (vec)
  "Construct a fresh vector with the components in vector VEC, which are assumed to be in radian
units, converted to degree units."
  (declare (optimize speed))
  (degrees! (zero) vec))

(u:fn-> expt! (vec vec real) vec)
(declaim (inline expt!))
(defun expt! (out vec power)
  "Modify vector OUT to be the components in vector VEC raised to the power of POWER."
  (com:cwset 2 out vec (cl:expt vec power))
  out)

(u:fn-> expt (vec real) vec)
(declaim (inline expt))
(defun expt (vec power)
  "Construct a fresh vector with the components in vector VEC raised to the power of POWER."
  (expt! (zero) vec power))

(u:fn-> sqrt! (vec vec) vec)
(declaim (inline sqrt!))
(defun sqrt! (out vec)
  "Modify vector OUT to have the square root of the components in VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:sqrt (the (u:f64 0d0) vec)))
  out)

(u:fn-> sqrt (vec) vec)
(declaim (inline sqrt))
(defun sqrt (vec)
  "Construct a fresh vector which has the square root of the components in VEC."
  (declare (optimize speed))
  (sqrt! (zero) vec))

(u:fn-> floor! (vec vec &optional u:f64) vec)
(declaim (inline floor!))
(defun floor! (out vec &optional (divisor 1d0))
  "Modify vector OUT to have the nearest integer less than or equal to each component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (ffloor vec divisor))
  out)

(u:fn-> floor (vec &optional u:f64) vec)
(declaim (inline floor))
(defun floor (vec &optional (divisor 1d0))
  "Construct a fresh vector that has the nearest integer less than or equal to each component of
vector VEC."
  (declare (optimize speed))
  (floor! (zero) vec divisor))

(u:fn-> ceiling! (vec vec &optional u:f64) vec)
(declaim (inline ceiling!))
(defun ceiling! (out vec &optional (divisor 1d0))
  "Modify vector OUT to have the nearest integer greater than or equal to each component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (fceiling vec divisor))
  out)

(u:fn-> ceiling (vec &optional u:f64) vec)
(declaim (inline ceiling))
(defun ceiling (vec &optional (divisor 1d0))
  "Construct a fresh vector that has the nearest integer greater than or equal to each component of
vector VEC."
  (declare (optimize speed))
  (ceiling! (zero) vec divisor))

(u:fn-> mod! (vec vec u:f64) vec)
(declaim (inline mod!))
(defun mod! (out vec divisor)
  "Modify vector OUT to have each component of vector VEC modulo DIVISOR."
  (declare (optimize speed))
  (com:cwset 2 out vec (nth-value 1 (ffloor vec divisor)))
  out)

(u:fn-> mod (vec real) vec)
(declaim (inline mod))
(defun mod (vec divisor)
  "Construct a fresh vector that has each component of vector VEC modulo DIVISOR."
  (declare (optimize speed))
  (mod! (zero) vec divisor))

(u:fn-> sin! (vec vec) vec)
(declaim (inline sin!))
(defun sin! (out vec)
  "Modify vector OUT to have the trigonometric sine function applied to each component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:sin vec))
  out)

(u:fn-> sin (vec) vec)
(declaim (inline sin))
(defun sin (vec)
  "Construct a fresh vector which has the trigonometric sine function applied to each component of
vector VEC."
  (declare (optimize speed))
  (sin! (zero) vec))

(u:fn-> cos! (vec vec) vec)
(declaim (inline cos!))
(defun cos! (out vec)
  "Modify vector OUT to have the trigonometric cosine function applied to each component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:cos vec))
  out)

(u:fn-> cos (vec) vec)
(declaim (inline cos))
(defun cos (vec)
  "Construct a fresh vector which has the trigonometric cosine function applied to each component of
vector VEC."
  (declare (optimize speed))
  (cos! (zero) vec))

(u:fn-> tan! (vec vec) vec)
(declaim (inline tan!))
(defun tan! (out vec)
  "Modify vector OUT to have the trigonometric tangent function applied to each component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:tan vec))
  out)

(u:fn-> tan (vec) vec)
(declaim (inline tan))
(defun tan (vec)
  "Construct a fresh vector which has the trigonometric tangent function applied to each component
of vector VEC."
  (declare (optimize speed))
  (tan! (zero) vec))

(u:fn-> asin! (vec vec) vec)
(declaim (inline asin!))
(defun asin! (out vec)
  "Modify vector OUT to have the trigonometric arcsine function applied to each component of vector
VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:asin (the (u:f64 -1d0 1d0) vec)))
  out)

(u:fn-> asin (vec) vec)
(declaim (inline asin))
(defun asin (vec)
  "Construct a fresh vector which has the trigonometric arcsine function applied to each component
of vector VEC."
  (declare (optimize speed))
  (asin! (zero) vec))

(u:fn-> acos! (vec vec) vec)
(declaim (inline acos!))
(defun acos! (out vec)
  "Modify vector OUT to have the trigonometric arccosine function applied to each component of
vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:acos (the (u:f64 -1d0 1d0) vec)))
  out)

(u:fn-> acos (vec) vec)
(declaim (inline acos))
(defun acos (vec)
  "Construct a fresh vector which has the trigonometric arccosine function applied to each component
of vector VEC."
  (declare (optimize speed))
  (acos! (zero) vec))

(u:fn-> atan! (vec vec) vec)
(declaim (inline atan!))
(defun atan! (out vec)
  "Modify vector OUT to have the trigonometric arctangent function applied to each component of
vector VEC."
  (declare (optimize speed))
  (com:cwset 2 out vec (cl:atan vec))
  out)

(u:fn-> atan (vec) vec)
(declaim (inline atan))
(defun atan (vec)
  "Construct a fresh vector which has the trigonometric arctangent function applied to each
component of vector VEC."
  (declare (optimize speed))
  (atan! (zero) vec))

(u:fn-> velocity! (vec vec u:f64) vec)
(declaim (inline velocity!))
(defun velocity! (vec axis rate)
  "Modify vector OUT with components designating a velocity following the right-hand rule, whose
direction is parallel to AXIS and has a magnitude of RATE units per second."
  (declare (optimize speed))
  (copy! vec axis)
  (normalize! vec vec)
  (scale! vec vec rate))

(u:fn-> velocity (vec u:f64) vec)
(declaim (inline velocity))
(defun velocity (axis rate)
  "Construct a fresh vector designating a velocity following the right-hand rule, whose direction is
parallel to AXIS and has a magnitude of RATE units per second."
  (velocity! (zero) axis rate))
