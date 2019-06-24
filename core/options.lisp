(in-package #:%first-light)

(fl.data:set 'options/default
             (u:dict :title "First-Light Game Project"
                     :window-width 800
                     :window-height 450
                     :delta 1/30
                     :vsync :on
                     :periodic-interval 0.25
                     :debug-interval 5
                     :log-repl-enabled t
                     :log-repl-categories '(:fl)
                     :log-level :debug
                     :anti-alias-level 4
                     :opengl-version "4.3"
                     :initial-scene nil))

(defun load-options (core)
  (setf (options core)
        (u:hash-merge (fl.data:get 'options/default)
                      (or (fl.data:get 'options/project)
                          (u:dict)))))

(defun option (context option-name)
  (u:href (options context) option-name))

(defmacro define-options (() &body body)
  `(fl.data:set 'options/project (u:dict #'eq ,@body)))
