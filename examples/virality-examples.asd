(asdf:defsystem #:virality-examples
  :description "Virality Engine Examples"
  :author ("Michael Fiano <mail@mfiano.net>"
           "Peter Keller <psilord@cs.wisc.edu>")
  :maintainer ("Michael Fiano <mail@mfiano.net>"
               "Peter Keller <psilord@cs.wisc.edu>")
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:virality)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   ;; (:file "assets")
   (:file "common")
   (:file "shaders/playground")
   (:file "shaders/damaged-helmet")
   (:file "shaders/dynamic-geometry")
   (:file "shaders/graph")
   (:file "shaders/noise")
   (:file "shaders/texture")
   (:file "shaders/font")
   (:file "shaders/matcap-lookup")
   (:file "example-damaged-helmet")
   (:file "example-collision")
   (:file "example-dynamic-geometry")
   (:file "example-geometric-volumes")
   (:file "example-graph")
   (:file "example-isometric-view")
   (:file "example-noise")
   (:file "example-playground")
   (:file "example-sprite")
   (:file "example-texture")
   (:file "example-scale-around")
   (:file "example-font")
   (:file "example-matcap")
   (:file "example-bricked")
   (:module "protect-the-planets"
    :components
    ((:file "hacked-components")
     (:file "common")
     (:file "protect-the-planets")))
   (:file "example-selector") ;; must be last: run any official example.
   (:file "examples")))
