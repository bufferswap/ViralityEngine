(in-package #:virality)

(defclass template-replacement ()
  ((%pattern :reader pattern
             :initarg :pattern)
   (%value :accessor value
           :initarg :value)))


(defclass template-parameters ()
  (;; Fields for the system definition.
   (%author :reader author
            :initarg :author)
   (%copyright :reader copyright
               :initarg :copyright)
   (%depends-on :reader depends-on
                :initarg :depends-on)
   (%description :reader description
                 :initarg :description)
   (%license :reader license
             :initarg :license)
   (%maintainer :reader maintainer
                :initarg :maintainer)
   (%system-name :reader system-name
                 :initarg :system-name)
   (%version :reader version
             :initarg :version)

   ;; Information about/for the templating process.
   (%template :reader template
              :initarg :template)
   (%template-directory :reader template-directory
                        :initarg :template-directory)

   ;; The files and types we're allowed to replace content in.
   (%whitelist-types :reader whitelist-types
                     :initarg :whitelist-types)
   (%whitelist-filenames :reader whitelist-filenames
                         :initarg :whitelist-filenames)))
