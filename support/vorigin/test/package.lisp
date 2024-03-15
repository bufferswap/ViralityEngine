(in-package #:cl-user)

(defpackage #:vorigin.test
  (:local-nicknames
   (#:const #:vorigin.constants)
   (#:m2 #:vorigin.mat2)
   (#:m3 #:vorigin.mat3)
   (#:m4 #:vorigin.mat4)
   (#:q #:vorigin.quat)
   (#:v2 #:vorigin.vec2)
   (#:v3 #:vorigin.vec3)
   (#:v4 #:vorigin.vec4))
  (:use
   #:cl
   #:parachute))
