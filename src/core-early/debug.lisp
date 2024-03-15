(in-package #:virality)

(defvar *profile* nil)
(defvar *profile-frames* 600)
(global-vars:define-global-var =profile-frame-counter= 0)

(defmacro with-profiling (core &body body)
  (let ((packages (remove-if-not
                   (lambda (x)
                     (or (u:string-starts-with-p x "VIRALITY")
                         (u:string-starts-with-p x "NET.MFIANO.LISP.ORIGIN")
                         (string= x "NET.MFIANO.LISP.SHADOW")
                         (string= x "NET.MFIANO.LISP.GOLDEN-UTILS")
                         (string= x "SDL2")
                         (string= x "CL-OPENGL")))
                   (mapcar #'package-name (list-all-packages)))))
    `(if *profile*
         (unwind-protect
              (progn
                (setf =profile-frame-counter= 0)
                (sb-profile:unprofile)
                (sb-profile:profile ,@packages)
                (submit-job :profile-watcher
                            (let ((target *profile-frames*))
                              (lambda ()
                                (loop :until (>= =profile-frame-counter=
                                                 target)
                                      :finally (stop ,core)))))
                ,@body)
           (sb-profile:report)
           (sb-profile:unprofile)
           (sb-profile:reset)
           (setf =profile-frame-counter= 0))
         (progn ,@body))))

(defun print-scene-tree (core)
  "Print an ascii representation of the scene tree indented to show children."
  (labels ((map-scene-tree (func parent &optional (level 0))
             (funcall func parent level)
             (let ((parent-transform
                     (component-by-type parent 'comp:transform)))
               (dolist (child (comp::children parent-transform))
                 (map-scene-tree func (actor child) (1+ level))))))
    (map-scene-tree
     (lambda (actor level)
       (let ((prefix-level 5))
         ;; NOTE: prefix-level is used for left justifying the level number to a
         ;; certian number of tens places for each actor. It makes the output
         ;; easier to read.
         (format t "~v@<~d~> ~v,,,v<~>Actor: ~s~%"
                 prefix-level level level #\Space (display-id actor))
         (u:do-hash-values (component (components actor))
           (format t " ~v,,,v<~> + (~(~a~):~(~a~)) [~s]~%"
                   ;; 5 for the left justified
                   (+ prefix-level level) #\space
                   (first
                    (package-nicknames
                     (symbol-package (component-type component))))
                   (component-type component) (display-id component)))))
     (scene-tree core))))
