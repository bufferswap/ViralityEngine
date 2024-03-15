(in-package #:vshadow)

(defun find-gpu-function (func-spec)
  (destructuring-bind (name . types) func-spec
    (find types (varjo.internals::get-external-function-by-name name nil)
          :key (lambda (x) (mapcar #'second (varjo.internals:in-args x)))
          :test #'equal)))

(defun get-function-spec (function)
  (cons (varjo:name function)
        (mapcar #'second (varjo.internals:in-args function))))

(defun ensure-function-dependency-tables (spec fn-deps dep-fns)
  (unless (u:href dep-fns spec)
    (setf (u:href dep-fns spec) (u:dict #'equal)))
  (setf (u:href fn-deps spec) (u:dict #'equal)))

(defun store-function-dependencies (spec dependencies)
  (symbol-macrolet ((fn-deps (meta :fn->deps))
                    (dep-fns (meta :dep->fns)))
    (when (u:href fn-deps spec)
      (u:do-hash-keys (k (u:href fn-deps spec))
        (u:when-found (dep-key (u:href dep-fns k))
          (remhash spec dep-key))))
    (ensure-function-dependency-tables spec fn-deps dep-fns)
    (dolist (dep dependencies)
      (let ((dep-spec (get-function-spec dep)))
        (unless (u:href dep-fns dep-spec)
          (setf (u:href dep-fns dep-spec) (u:dict #'equal)))
        (u:do-hash-keys (k (u:href dep-fns spec))
          (setf (u:href fn-deps k dep-spec) dep-spec
                (u:href dep-fns dep-spec k) k))
        (setf (u:href fn-deps spec dep-spec) dep-spec
              (u:href dep-fns dep-spec spec) spec)))))

(defun compute-outdated-programs (spec)
  (let* ((dep->fns (meta :dep->fns))
         (spec-fns (u:href dep->fns spec))
         (programs nil))
    (u:do-hash (k v (meta :stage-fn->programs))
      (when (or (u:href spec-fns k)
                (equal k spec))
        (setf programs (union v programs :test #'equal))))
    programs))
