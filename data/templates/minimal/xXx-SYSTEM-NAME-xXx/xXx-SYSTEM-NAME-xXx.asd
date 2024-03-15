;; CL gives much freedom for how to organize your project, but V requires some
;; things to be ordered in the API, so this skeleton project give a suggested
;; method of dependencies to make it easier to deal with both the required
;; dependencies and the freedom CL provides.

(asdf:defsystem #:xXx-SYSTEM-NAME-xXx
  :description "xXx-DESCRIPTION-xXx"
  :author "xXx-AUTHOR-xXx"
  :maintainer "xXx-MAINTAINER-xXx"
  :license "xXx-LICENSE-xXx"
  :version "xXx-VERSION-xXx"
  :depends-on xXx-DEPENDS-ON-xXx
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "engine-config")
   (:file "xXx-SYSTEM-NAME-xXx")))
