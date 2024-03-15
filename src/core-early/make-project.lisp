(in-package #:virality)

;;; This code is related to MAKE-PROJECT, a gamedev facing API for the creation
;;; of new virality application projects.

(defun make-template-replacement (pattern value)
  (make-instance 'template-replacement
                 :pattern pattern
                 :value value))

(defun make-template-parameters (&rest init-args)
  (apply #'make-instance 'template-parameters init-args))

;; TODO: The information contained in this function should end up in some RC
;; file for the engine somewhere.
(defun resolve-template-location (template)
  "Return a fully qualified namestring to a template dorectory. If TEMPLATE is
exactly a fully qualified path, then check to see that it exists on disk and if
so return it unchanged. If TEMPLATE is one of these keywords, return the path
to the appropriate template directory:
  :minimal, :base, :example-cube"
  (let* ((template-locations
           '((:minimal "templates/minimal/xXx-SYSTEM-NAME-xXx/")
             (:base "templates/base/xXx-SYSTEM-NAME-xXx/")
             (:example-cube "templates/example-cube/xXx-SYSTEM-NAME-xXx/")))
         (valid-templates (mapcar #'first template-locations)))

    (when (and (or (stringp template)
                   (pathnamep template))
               (probe-file template))
      (return-from resolve-template-location template))

    (unless (and (keywordp template)
                 (member template (mapcar #'first template-locations)))
      (error "The TEMPLATE may only be a namestring to an existing template ~
              directory or one of the keywords: ~(~S~)" valid-templates))

    (resolve-path (second (assoc template template-locations)))))


(defun gen-system-name (target-directory)
  "Take the last part of the TARGET-DIRECTORY and slugify it. This will be the
system name for the newly constructing project, the name used in the asd
filename, and the name of the package in which the new project exists.

Example call:
 (gen-system-name \"/home/psilord/quicklisp/local-projects/my_skel\")
                                                           ^^^^^^^
                                             This is slugified and becomes the
                                             system name of \"my-skel\".
"
  (multiple-value-bind (abs-or-rel dirpath last-component flag)
      (uiop:split-unix-namestring-directory-components target-directory)
    (declare (ignore flag))
    ;; TODO: Support relative stuff later.
    (assert (eq abs-or-rel :absolute))
    (assert (or (not (null dirpath)) (not (null last-component))))
    (cl-slug:slugify (if (null last-component)
                         (car (last dirpath))
                         last-component))))

(defun gen-raw-template-map (template-directory target-directory)
  "Return a list of lists. The FIRST of each list is :DIR or :FILE depending on
what is being copied, the SECOND of each list is the truename of a directory or
file from the TEMPLATE-DIRECTORY and the THIRD is the truename of the same
directory or file as it might be emplaced into the TARGET-DIRECTORY. The
filenames can themselves contain sentinels for replacement--which means the
mapping of the template directories and files to the target directory and files
may not yet be complete!

An example call is:
  (gen-raw-template-map
    \"/home/psilord/templates/example-cube/xXx-SYSTEM-NAME-xXx/\"
    \"/home/psilord/quicklisp/local-projects/my-skel/\")

NOTE: The final / on the pathnames are VERY important and must be present.
"
  (let ((result nil)
        (dirs nil))
    (uiop:collect-sub*directories template-directory
                                  (constantly t)
                                  (constantly t)
                                  (lambda (x) (push (truename x) dirs)))
    (setf dirs (nreverse dirs))

    (dolist (dir dirs)
      (let* ((template-reldir (enough-namestring dir template-directory))
             (target-reldir (merge-pathnames template-reldir
                                             target-directory)))
        (push (list :dir dir target-reldir) result)
        (dolist (file (uiop:directory-files dir))
          (let* ((template-file (enough-namestring file template-directory))
                 (target-file (merge-pathnames template-file
                                               target-directory)))
            (push (list :file file target-file) result)))))
    (nreverse result)))

(defun apply-template (line param)
  "Apply the template replacements specified in PARAM to the LINE and return
the newly substituted line."
  (loop :with line = line
        :for func :in '(author copyright depends-on description license
                        maintainer system-name version)
        :for replacement = (funcall func param)
        :do (setf line
                  (u:string-replace-all
                   (pattern replacement) line (value replacement)))
        :finally (return line)))


(defun perform-file-destination-subs (target param)
  "Apply the template replacements specified in PARAM to the destination paths
in the mapping. Return a new mapping with the substitutions enacted."
  (destructuring-bind (dir-or-file source destination) target
    (list dir-or-file
          source
          (pathname (apply-template (namestring destination) param)))))

(defun copy-file-with-transformer (source destination
                                   &key (transformer (lambda (x) x)))
  "Copy the text-only file line by line from SOURCE to DESTINATION and apply
the TRANSFORMER function to each _line_ of the file."
  (with-open-file (fout destination
                        :direction :output
                        :if-exists :supersede)
    (with-open-file (fin source
                         :direction :input)
      (loop :for line = (read-line fin nil :eof)
            :until (eq line :eof)
            :do (write-line (funcall transformer line) fout))
      t)))

(defun perform-file-copy-subs (template-file target-file param)
  "Copy the contents of the TEMPLATE-FILE to the TARGET-FILE while performing
any template substitutions as denoted in the PARAM object. It is expected that
the TARGET-FILE has been correctly substituted and is now the actually correct
path on the filesystem."
  (multiple-value-bind (abs-or-rel directories basename flag)
      (uiop:split-unix-namestring-directory-components
       (namestring target-file))
    (declare (ignore abs-or-rel directories flag))
    (multiple-value-bind (file-name file-type)
        (uiop:split-name-type basename)
      (if (or (member file-type (whitelist-types param) :test #'string=)
              (and (eq file-type uiop:*unspecific-pathname-type*)
                   (member file-name (whitelist-filenames param)
                           :test #'string=)))
          (copy-file-with-transformer template-file target-file
                                      :transformer
                                      (lambda (str)
                                        (apply-template str param)))
          (uiop:copy-file template-file target-file)))))

(defun gen-depends-on (depends-on)
  "This function may take a single designation package name or symbol or a list
of them and returns a representation string suitable for inclusion in the
:depends-on line in an asd file."
  (let* ((depends-on (cons "VIRALITY" (u:ensure-list depends-on)))
         (raw-package-names
           (remove-duplicates
            (mapcar (lambda (x)
                      (cond
                        ((symbolp x)
                         (symbol-name x))
                        ((stringp x)
                         x)
                        (t
                         (error "Unable to handle depends-on name of ~A" x))))
                    depends-on)
            :test #'string=
            :from-end t))
         (decorated-package-names
           (mapcar (lambda (name)
                     (list (every #'upper-case-p name) (make-symbol name)))
                   raw-package-names))
         (package-string-names
           (mapcar (lambda (decorated-name)
                     (let ((fmt (if (car decorated-name) "~(~S~)" "~S")))
                       (format nil fmt (cadr decorated-name))))
                   decorated-package-names)))

    (format nil "~A" package-string-names)))

(defun verify-system (destination-path defsystem-path)
  "If DESTINATION-PATH is the same as DEFSYSTEM-PATH, return :verified,
otherwise :mismatch"
  (if (string= (namestring (truename (probe-file destination-path)))
               (namestring (truename (probe-file defsystem-path))))
      :verified
      :mismatch))

(defun perform-system-registration (destination-path system-name)
  "Try to register the system for loading and return an ascii string
indicating how to load it. Return three values: the message to display,
one of :verified, :mismatch, and :unknown, and then the path to the
system that would be loaded or NIL if that's not possible."
  (let* ((ql-package
           (find-package "QUICKLISP"))
         (full-path
           (probe-file destination-path))
         (sysname
           (format nil ":~A" system-name))
         (msg-ql-load
           (format nil ";; To load your new system, run:~%;;~%   ~
                        (ql:quickload ~A)~%;;" sysname))
         (msg-asdf-load
           (format nil ";; To load your new system, run:~%;;~%   ~
                        (asdf:load-system ~A)~%;;" sysname))
         (msg-unk-load
           (concatenate
            'string
            (format nil ";; The system ~A has not been found by any~%~
                         ;; defsystem management library's search. Please~%~
                         ;; ensure your new system can be found and then~%~
                         ;; try:~%~
                         ;;~%~
                         ~A~%" sysname msg-asdf-load)
            (when ql-package
              (format nil ";; or~%~A~%;;~%" msg-ql-load))
            (format nil ";; as appropriate to your environment."))))

    ;; FIRST: try quicklisp to see if it can find it.
    (when ql-package
      (let ((register-local-projects-func
              (find-symbol "REGISTER-LOCAL-PROJECTS" ql-package))
            (where-is-system-func
              (find-symbol "WHERE-IS-SYSTEM" ql-package)))
        (funcall register-local-projects-func)
        (u:when-let ((where (funcall where-is-system-func system-name)))
          (return-from perform-system-registration
            (values msg-ql-load (verify-system full-path where) where)))))

    ;; NEXT: Let's see if ASDF finds it
    (u:when-let ((where (asdf:system-source-directory system-name)))
      (return-from perform-system-registration
        (values msg-asdf-load (verify-system full-path where) where)))

    ;; NEXT: Out of luck for finding it, give the user something of value.
    (values msg-unk-load :unknown nil)))


(defun make-project (destination-path
                     &key
                       (author "Your Name <your.name@example.com>")
                       (copyright "Copyright (from-date - to-date), Your Name")
                       (depends-on nil) ;; a list of package names.
                       (description "Specify project description here.")
                       ;; TODO add license keywords or just a string. This
		       ;; would also be related to config file stuff as below.
                       (license "Specify license here.")
                       (maintainer "A Name <a.name@example.com>")
                       (version "0.1.0")
                       (template :base)

                       ;; TODO Make a config file for the template and jam this
                       ;; stuff in there.
                       (author-pattern "xXx-AUTHOR-xXx")
                       (copyright-pattern "xXx-COPYRIGHT-xXx")
                       (depends-on-pattern "xXx-DEPENDS-ON-xXx")
                       (description-pattern "xXx-DESCRIPTION-xXx")
                       (license-pattern "xXx-LICENSE-xXx")
                       (maintainer-pattern "xXx-MAINTAINER-xXx")
                       (system-name-pattern "xXx-SYSTEM-NAME-xXx")
                       (version-pattern "xXx-VERSION-xXx")

                       (whitelist-types '("asd" "lisp" "txt" "md"))
                       (whitelist-filenames '("README" "LICENSE"))

                       (verbose T))
  "Create a new Virality Engine system suitable for developing a new project
whose name and main package is named a slugified version of the last entry in
DESTINATION-PATH. The DESTINATION-PATH must be a directory that does not
already exist. It is expected that these keyword arguments should be
specified, otherwise the specified boring default is chosen.

  :author        \"Your Name <your.name@example.com>\"
  :copyright     \"Copyright (from-date - to-date), Your Name\"
  :depends-on    nil
  :description   \"Specify project description here.\"
  :license       \"Specify license here.\"
  :maintainer    \"A Name <a.name@example.com>\"
  :version       \"0.1.0\"

Note that the value for the :depends-on keyword argument may be a symbol, a
designated string name for a package, or a list of either of those. All of
these packages becomes what the generated system is dependent upon.  Note that
the :virality package will always be included in the :depends-on value
regardless if it is included or not in the :depends-on value.

The template you would like to make is specified with the :template keyword
argument. The default is :base.

The :template keyword argument may take exactly one of:
  :minimal       - An extremely small loadable system with miminal files and
                   code. Generally suited for expert usage.
  :base          - The typical layout of directories and files complete with
                   ordering semantics for how the source should be loaded.
  :example-cube  - Same as :base, but with a small example written in it
                   to demonstrate where you'd put a few things in the base
                   directory structure.
  \"/template/dir/\" - This is a fully qualified directory namestring to a
                       template directory that this function will copy into the
                       destination path while substituting the patterns with
                       their values. The patterns may exist in side of the
                       files or the names of the files themselves, in which
                       case the copied name of the file will be the name after
                       the substitution.

The patterns that the template engine will substitute may be specified
with these keywords. The defaults are shown.

  :author-pattern      \"xXx-AUTHOR-xXx\"
  :copyright-pattern   \"xXx-COPYRIGHT-xXx\"
  :depends-on-pattern  \"xXx-DEPENDS-ON-xXx\"
  :description-pattern \"xXx-DESCRIPTION-xXx\"
  :license-pattern     \"xXx-LICENSE-xXx\"
  :maintainer-pattern  \"xXx-MAINTAINER-xXx\"
  :system-name-pattern \"xXx-SYSTEM-NAME-xXx\"
  :version-pattern     \"xXx-VERSION-xXx\"

Finally, the template engine has a whitelist of file types and specific file
names that are ALLOWED to have the above patterns replaced in them. The
defaults are shown.

  :whitelist-types     '(\"asd\" \"lisp\" \"txt\" \"md\"))
  :whitelist-filenames '(\"README\" \"LICENSE\")))

All files that don't match the whitelist-types or whitelist-filenames are
copied verbatim (though the _path_ to that file still might have had a pattern
replacement applied!)

And finally, you can control if there is textual output from this function
with this keyword argument:

  :verbose T

This function signals an error when the DESTINATION-PATH already exists,
if the DESTINATION-PATH or :template directory namestring (if suppled) are not
actually directories, or if the :template directory doesn't exist.

Return three values:
  The first value is the generated system name in keyword symbol form.

  The second value is one of :verified, :mismatch, or :unknown.
  This represents if the system is loadable and you will load what you expect,
  or that if you load the system it will be something else of the same system
  name (which will be a surprise), or :unknown, meaning the project was
  created but isn't in any search path of the defsystem management libraries.
  This requires the user to fix the search paths until the system is findable.

  The third value is the path to the system that WILL be loaded, or NIL
  if none can be found.
"

  (let ((generated-system-name (gen-system-name destination-path))
        (template-directory (resolve-template-location template)))

    ;; 0. Sanity checks
    (unless (uiop:directory-pathname-p destination-path)
      (error "The DESTINATION-PATH must be a directory! It is not: ~S"
             destination-path))
    (when (probe-file destination-path)
      (error "DESTINATION-PATH: ~A already exists! Doing nothing!"
             destination-path))
    (unless (uiop:directory-pathname-p template-directory)
      (error "The TEMPLATE ~(~S~) must ultimately resolve to a dir! It did not: ~S"
             template template-directory))

    ;; 1. Set up the template parameters object
    (let ((param
            (make-template-parameters
             :author
             (make-template-replacement author-pattern author)
             :copyright
             (make-template-replacement copyright-pattern copyright)
             :depends-on
             (make-template-replacement depends-on-pattern
                                        (gen-depends-on depends-on))
             :description
             (make-template-replacement description-pattern description)
             :license
             (make-template-replacement license-pattern license)
             :maintainer
             (make-template-replacement maintainer-pattern maintainer)
             :system-name
             (make-template-replacement system-name-pattern
                                        generated-system-name)
             :version (make-template-replacement version-pattern version)
             :template template
             :template-directory template-directory
             :whitelist-types whitelist-types
             :whitelist-filenames whitelist-filenames)))

      (let ((mapping
              ;; 2. Compute the raw template-map from the template to the
              ;; destination.
              ;;
              ;; 3. Perform substitutions on the target file names to get a
              ;; final version of the file names names we'll be copying stuff
              ;; into.
              (mapcar (lambda (x)
                        (perform-file-destination-subs x param))
                      (gen-raw-template-map (template-directory param)
                                            destination-path))))

        ;; 4. Walk the mapping and do the copy of the contents, performing the
        ;; substitutions (if applicable) on the file contents.
        (loop :for (kind template-file target-file) :in mapping
              :do (uiop:ensure-all-directories-exist (list target-file))
                  (when (eq kind :file)
                    (perform-file-copy-subs template-file target-file param)))


        ;; 5. Verify the creation and spew out results.
        (multiple-value-bind (msg verification where)
            (perform-system-registration destination-path
                                         generated-system-name)
          (when verbose
            (format t ";; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%")
            (format t ";; Virality Engine Project Created.~%")
            (format t ";; Location: ~A~%" destination-path)
            (format t ";; Template Chosen: ~?~%"
                    (if (keywordp template)
                        "~(~S~)"
                        "~A")
                    (list template))
            (format t ";; System Name: :~A~%" generated-system-name)
            (format t ";; Package name: :~A~%" generated-system-name)
            (format t ";;~%")
            (ecase verification
              (:verified
               (format t ";; The system is loadable!~%"))
              (:mismatch
               (format t ";; WARNING: Loading this system will load~%~
                        ;; this instead: ~A~%" where))
              (:unknown nil))
            (format t "~A~%" msg)
            (format t ";; Once it is loaded, start the project's default~%~
                     ;; config like this:~%;;~%")
            (format t "   (virality:start)~%")
            (format t ";;~%")
            (format t ";; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;~%"))

          (values (intern (string-upcase generated-system-name) :keyword)
                  verification
                  where))))))
