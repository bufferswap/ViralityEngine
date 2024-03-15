(asdf:defsystem #:vutils
  :description "A utility library."
  :author "Michael Fiano <mail@mfiano.net>"
  :license "MIT"
  :homepage "https://git.mfiano.net/mfiano/mfiano-utils"
  :version "0.1.0"
  :encoding :utf-8
  :depends-on
  (#:alexandria
   #:serapeum
   #:uiop)
  :pathname "vutils/src"
  :serial t
  :components
  ((:file "package")
   (:file "closure")
   (:file "type")
   (:file "symbol")
   (:file "number")
   (:file "character")
   (:file "array")
   (:file "string")
   (:file "sequence")
   (:file "list")
   (:file "list-alist")
   (:file "list-plist")
   (:file "hash-table")
   (:file "hash-table-nested")
   (:file "macro")
   (:file "filesystem")
   (:file "math")
   (:file "os")
   (:file "random")
   (:file "misc")))
