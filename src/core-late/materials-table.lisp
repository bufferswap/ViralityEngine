(in-package #:virality)

;; materials-table impl

;;; Internal Materials-table API
(defun make-materials-table (&rest init-args)
  (apply #'make-instance 'materials-table init-args))

(defun %lookup-material (material-name core)
  "Find a material by its ID in CORE and return a gethash-like values. If the
material isn't there, return the 'x:missing-material. The return value is two
values, the first is a material instance, and the second is T if the material
being looked up was actually found, or NIL if it wasn't (and the missing
material used)."
  (symbol-macrolet ((table (material-table (materials core))))
    (u:mvlet ((material found-p (u:href table material-name)))
      (if found-p
          (values material t)
          (values (u:href table (u:ensure-symbol "MISSING-MATERIAL" :x))
                  nil)))))

(defun %add-material (material core)
  "Add the MATERIAL by its id into CORE."
  (setf (u:href (material-table (materials core)) (id material)) material))

(defun %remove-material (material core)
  "Remove the MATERIAL by its id from CORE."
  (remhash (id material) (material-table (materials core))))

(defun %map-materials (func core)
  "Map the function FUNC, which expects a material, across all materials in
CORE. Return a list of the return values of the FUNC."
  (let (results)
    (u:do-hash-values (v (material-table (materials core)))
      (push (funcall func v) results))
    (nreverse results)))

;; Complete and unify the public API to include the context so when the
;; gameedv makes an initial material they must pass a context to it (so
;; that it can immediately go into core state, etc).

;; export PUBLIC API
(defun lookup-material (id context)
  (%lookup-material id (core context)))

;; export PUBLIC API
;; current-mat-name must exist.
;; new-mat-name must not exist. :)
(defun copy-material (current-mat new-mat-name
                      &key (error-p t) (error-value nil))
  "Copy the material CURRENT-MAT and give the new material the name
NEW-MAT-NAME. The new material name must not already exist. Return the new
material."
  (when (u:href (material-table (materials (core current-mat)))
                new-mat-name)
    (if error-p
        (error "Cannot copy the material ~a to new name ~a because the new ~
                name already exists!"
               (id current-mat) new-mat-name)
        (return-from copy-material error-value)))
  (let ((new-mat (%deep-copy-material current-mat new-mat-name)))
    (%add-material new-mat (core new-mat))
    new-mat))
