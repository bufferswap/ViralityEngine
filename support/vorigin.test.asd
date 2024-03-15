(asdf:defsystem #:vorigin.test
  :description "Tests for origin."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/origin"
  :depends-on
  (#:vorigin
   #:parachute)
  :pathname "vorigin/test"
  :serial t
  :perform (asdf:test-op (o c) 
             (uiop:symbol-call '#:parachute '#:test '#:vorigin.test))
  :components
  ((:file "package")
   (:file "vec2")
   (:file "vec3")
   (:file "vec4")
   (:file "mat2")
   (:file "mat3")
   (:file "mat4")
   (:file "quat")))
