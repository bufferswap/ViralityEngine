(in-package #:virality)

(u:eval-always
  (defun get-config-option-name (key)
    (u:format-symbol :virality "=~a=" key)))

(defun validate-config-options (options)
  (u:do-plist (k v options)
    (u:unless-found (x (u:href =meta/config/default= k))
      (error "~s is not a valid config option. Valid options:~%~{~s~%~}"
             x (u:hash-keys =meta/config/default=)))))

(defun find-config (config-name)
  (when config-name
    (return-from find-config
      (u:href =meta/config/config= config-name)))

  (let ((results nil))
    (u:do-hash-values (options =meta/config/config=)
      (when (u:href options :default)
        (push options results)))
    (cond
      ((> (length results) 1)
       (error "There were two or more configs with :default t in them, there can be only one. The offending configs are: ~A" results))
      ((= (length results) 1)
       (return-from find-config (car results)))
      (t
       (error "Cannot find a default config because no config has a :default t!")))))

(defun load-config (config-name &rest args)
  (u:do-hash (k v (find-config config-name))
    (let ((global (get-config-option-name k)))
      (setf (symbol-value global) v)))
  (u:do-plist (k v args)
    (let ((global (get-config-option-name k)))
      (setf (symbol-value global) v))))

(defmacro define-config (config-name options &body body)
  (declare (ignore options))
  `(progn
     ,@(if (eq config-name :virality)
           `((setf =meta/config/default= (u:dict ,@body))
             ,@(loop :for (k v) :on body :by #'cddr
                     :for name = (get-config-option-name k)
                     :collect `(global-vars:define-global-parameter ,name ,v)))
           `((validate-config-options ',body)
             (when (keywordp ',config-name)
               (error
                "~S is not a valid config name. Keyword names are reserved!"
                ',config-name))
             (setf (u:href =meta/config/config= ',config-name)
                   (u:hash-merge =meta/config/default= (u:dict ,@body)))))))

;; NOTE: All other define-config forms implicitly inherit their keys from this
;; config.
(define-config :virality ()
  :default nil
  :allow-screensaver nil
  :anti-alias-level 4
  :delta (float 1/30 1f0)
  :debug-interval 5
  :initial-scene nil
  :log-level :debug
  :opengl-version "4.3"
  :period-interval 0.25
  :release nil
  :threads 4
  :vsync t
  :window-height 450
  :window-width 800
  :window-title "Virality Engine")
