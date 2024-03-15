(asdf:defsystem #:vumbra
  :description "A library of reusable GPU shader functions."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/umbra"
  :encoding :utf-8
  :depends-on
  (#:varjo
   #:vshadow
   #:vutils)
  :pathname "vumbra/src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "common-swizzle")
   (:file "common-vari")
   (:file "common-math")
   (:file "common-structs")
   (:file "color-space")
   (:file "color-grading")
   (:file "graphing")
   (:file "shaping-iq")
   (:file "shaping-levin")
   (:file "shaping-penner")
   (:file "shaping-misc")
   (:file "hashing-bbs")
   (:file "hashing-fast32")
   (:file "hashing-fast32-2")
   (:file "hashing-sgpp")
   (:file "noise-cellular")
   (:file "noise-hermite")
   (:file "noise-perlin")
   (:file "noise-polkadot")
   (:file "noise-simplex")
   (:file "noise-value")
   (:file "noise-misc")
   (:file "noise-utils")
   (:file "sdf-2d")
   (:file "sprite")))
