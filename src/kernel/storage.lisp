(in-package #:virality)

(defun %storage (context component-name namespace &rest keys)
  (let* ((name (qualify-component (core context) component-name))
         (tests (cdr (storage-metadata name namespace)))
         (new-tests (list* 'eq 'eql tests))
         (new-keys (list* name namespace keys)))
    (when (null tests)
      (error "Storage namespace ~s does not exist for component ~s in  package ~
              ~s."
             namespace
             component-name
             (if name
                 (package-name (symbol-package name))
                 "[no package: component does not exist!]")))
    (assert (= (length tests) (length keys)))
    (u:ensure-nested-hash-table (storage context) new-tests new-keys)
    (apply #'u:href (storage context) new-keys)))

(defun (setf %storage) (new-value context component-name namespace &rest keys)
  (let* ((name (qualify-component (core context) component-name))
         (tests (cdr (storage-metadata name namespace)))
         (new-keys (list* name namespace keys)))
    (assert (= (length tests) (length keys)))
    (u:ensure-nested-hash-table (storage context) (list* 'eq 'eql tests) new-keys)
    (apply #'(setf u:href) new-value (storage context) new-keys)))

(defun %generate-storage-get/set (context bindings body)
  "While there are bindings to perform, strip one off, build a lexical
environemt for it that will set it into the storage in CONTEXT, and keep
expanding with further bindings. When done, emit the body in the final and most
dense lexical scope."
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (var found-p lookup-form value-form) (first bindings)
        (let* ((binding-forms (mapcar (lambda (x) `(,(gensym) ,x)) lookup-form))
               (args (mapcar #'first binding-forms)))
          `(let ,binding-forms
             (u:mvlet ((,var ,found-p (%storage ,context ,@args)))
               (unless ,found-p
                 (setf ,var ,value-form (%storage ,context ,@args) ,var))
               ,(%generate-storage-get/set context (rest bindings) body)))))))

(defgeneric storage-metadata (component-name &optional namespace)
  (:method ((component-name symbol) &optional namespace)
    (declare (ignore namespace))))

(defmacro with-storage ((context-var context-form) bindings &body body)
  "Short Form for storage access."
  `(let ((,context-var ,context-form))
     ,(%generate-storage-get/set context-var bindings body)))
