(in-package #:virality)

(defmacro with-continuable (&body body)
  ;; NOTE: Be very aware of the use of the 'hook' variable and the ,hook
  ;; aspects of it. It is special so sometimes the rebind thast doesn't use it
  ;; could be surprising to look at. INVOKE-DEBUGGER ultimately uses the rebind
  ;; later in the function.
  (u:with-gensyms (debugger-entry-time previous-hook pause-time)
    (let ((hook #+sbcl 'sb-ext:*invoke-debugger-hook*
                #-sbcl '*debugger-hook*)
          ;; TODO: Deal with when somehow *core-debug* is unbound.
          (clock '(clock *core-debug*)))
      `(let* ((,previous-hook ,hook)
              (,hook
                (lambda (condition hook)
                  (declare (ignore hook))
                  (let ((,debugger-entry-time (get-time ,clock))
                        (,hook ,previous-hook))
                    (unwind-protect (invoke-debugger condition)
                      (let ((,pause-time (- (get-time ,clock)
                                            ,debugger-entry-time)))
                        (setf (pause-time ,clock) ,pause-time)
                        (format t "Spent ~3$ seconds in the debugger.~%"
                                ,pause-time)))))))
         (restart-case (progn ,@body)
           (abort ()
             :report "Skip processing the currently executing frame"))))))

(flet ((generate-live-support-functions ()
         (let ((repl-package (find-if #'find-package '(:slynk :swank))))
           (compile
            'setup-repl
            (if (eq repl-package :slynk)
                `(lambda ()
                   (,(find-symbol "SEND-PROMPT" :slynk-mrepl)))
                `(lambda () nil)))
           (compile
            'update-repl
            (case repl-package
              (:slynk
               `(lambda ()
                  (,(find-symbol "PROCESS-REQUESTS" :slynk) t)))
              (:swank
               `(lambda ()
                  (u:when-let ((repl (or ,(find-symbol "*EMACS-CONNECTION*"
                                                       :swank)
                                         (,(find-symbol "DEFAULT-CONNECTION"
                                                        :swank)))))
                    (,(find-symbol "HANDLE-REQUESTS" :swank) repl t))))
              (t `(lambda () nil))))
           (compile
            'send-to-repl
            (if (eq repl-package :slynk)
                `(lambda () nil)
                ;; TODO: uncomment and remove constantly line above when Sly
                ;; merges in send-to-repl to master.
                #++`(lambda (values &key (comment "Sent from Pyx"))
                      (,(find-symbol "COPY-TO-REPL-IN-EMACS" :slynk-mrepl)
                       values :blurb comment :pop-to-buffer nil))
                `(lambda () nil))))))
  (generate-live-support-functions))

(defun recompile-queued-items (core)
  (loop :for ((kind data) found-p) = (multiple-value-list
                                      (pop-queue :recompile))
        :while found-p
        :do (ecase kind
              (:shader
               (shadow:recompile-shaders data))
              ((:texture :material :prefab)
               (funcall data core)))))
