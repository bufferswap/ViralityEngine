(in-package #:cl-user)

(uiop:define-package #:vutils
  (:use #:cl)
  (:mix-reexport #:serapeum #:alexandria)
  (:shadow
   #:dict
   #:href
   #:make-keyword
   #:mvlet
   #:mvlet*)
  ;; Macros
  (:export
   #:define-package
   #:define-printer
   #:fn->
   #:if-found
   #:mvlet
   #:mvlet*
   #:unless-found
   #:until
   #:when-found
   #:while
   #:with-temp-package)
  ;; Closures
  (:export
   #:define-pfun
   #:dlambda
   #:plambda
   #:self
   #:this
   #:with-pvars)
  ;; Types
  (:export
   #:b8
   #:b8a
   #:b16
   #:b16a
   #:b24
   #:b24a
   #:b32
   #:b32a
   #:b64
   #:b64a
   #:bit-vector
   #:f32
   #:f32a
   #:f64
   #:f64a
   #:fixnum-array
   #:non-null-symbol
   #:ub8
   #:ub8a
   #:ub16
   #:ub16a
   #:ub24
   #:ub24a
   #:ub32
   #:ub32a
   #:ub64
   #:ub64a)
  ;; Symbols
  (:export
   #:collect-external-symbols
   #:collect-symbols
   #:make-keyword
   #:symbol-name=)
  ;; Numbers
  (:export
   #:count-digits)
  ;; Characters
  (:export
   #:ascii-alphanumeric-p
   #:ascii-control-p
   #:ascii-letter-p
   #:ascii-lowercase-p
   #:ascii-number-p
   #:ascii-uppercase-p
   #:hex-char-p
   #:null-char-p)
  ;; Arrays
  (:export
   #:do-array
   #:make-b8-array
   #:make-b16-array
   #:make-b24-array
   #:make-b32-array
   #:make-bit-vector
   #:make-f32-array
   #:make-fixnum-array
   #:make-f64-array
   #:make-ub8-array
   #:make-ub16-array
   #:make-ub24-array
   #:make-ub32-array)
  ;; Strings
  (:export
   #:split-string
   #:string->keyword
   #:string-ends-with-p
   #:string-explode
   #:string-merge
   #:string-starts-with-p
   #:string-trim-whitespace)
  ;; Sequences
  (:export
   #:copy-sequence-tree
   #:do-seq
   #:enumerate
   #:find-all
   #:flatten-tree
   #:flatten-numbers)
  ;; Lists
  (:export
   #:combinations/repetition
   #:interleave
   #:tree-leaves
   #:zip)
  ;; Association lists
  (:export
   #:alist
   #:alist-p
   #:alist-get
   #:alist-keys
   #:alist-remove
   #:alist-removef
   #:alist-rget
   #:alist-values
   #:alist->hash
   #:alist->plist
   #:do-alist
   #:do-alist-keys
   #:do-alist-values
   #:map-alist
   #:map-alist-keys
   #:map-alist-values)
  ;; Property lists
  (:export
   #:do-plist
   #:do-plist-keys
   #:do-plist-values
   #:map-plist
   #:map-plist-keys
   #:map-plist-values
   #:plist
   #:plist-p
   #:plist-get
   #:plist-remove
   #:plist-removef
   #:plist->alist
   #:plist->hash)
  ;; Hash tables
  (:export
   #:dict
   #:do-hash
   #:do-hash-keys
   #:do-hash-values
   #:hash-keys
   #:hash-merge
   #:hash-values
   #:hash->alist
   #:hash->plist
   #:href)
  ;; Hash tables nested
  (:export
   #:ensure-nested-hash-table
   #:make-nested-dict)
  ;; Filesystem
  (:export
   #:file->string
   #:map-files
   #:safe-read-file-form
   #:safe-read-file-forms
   #:string->file
   #:with-binary-input
   #:with-binary-output
   #:with-file-input
   #:with-file-output)
  ;; Math
  (:export
   #:average
   #:degrees->radians
   #:map-domain
   #:radians->degrees)
  ;; OS
  (:export
   #:parse-argv0)
  ;; Random
  (:export
   #:initialize-rng)
  ;; Misc
  (:export
   #:doc
   #:noop))
