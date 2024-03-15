(in-package #:virality)

(defmethod resource-cache-layout (entry-type)
  '(eql))

(defmethod resource-cache-peek (context (entry-type symbol) &rest keys)
  (with-slots (%resource-cache) (core context)
    ;; NOTE: 'eq is for the resource-cache table ;; itself.
    (u:ensure-nested-hash-table %resource-cache
                                (list* 'eq (resource-cache-layout entry-type))
                                (list* entry-type keys))
    (apply #'u:href %resource-cache (list* entry-type keys))))

(defmethod resource-cache-construct (context entry-type &rest keys)
  (declare (ignore context keys))
  (error "resource-cache-construct: Cannot construct unknown entry-type: ~A"
         entry-type))

;; This might call resource-cache-construct if needed.
(defmethod resource-cache-lookup (context (entry-type symbol) &rest keys)
  (with-slots (%resource-cache) (core context)
    ;; NOTE: 'eq is for the resource-cache table itself.
    (u:ensure-nested-hash-table %resource-cache
                                (list* 'eq (resource-cache-layout entry-type))
                                (list* entry-type keys))
    (multiple-value-bind (value found-p)
        (apply #'u:href %resource-cache (list* entry-type keys))
      (unless found-p
        (setf value (apply #'resource-cache-construct context entry-type keys)
              (apply #'u:href %resource-cache (list* entry-type keys)) value))
      value)))

(defmethod resource-cache-dispose (context entry-type removed-value)
  (declare (ignore context removed-value))
  (error "resource-cache-dispose: Cannot dispose unknown entry-type: ~A"
         entry-type))

;; This might call resource-cache-dispose if needed.
(defmethod resource-cache-remove (context (entry-type symbol) &rest keys)
  (with-slots (%resource-cache) (core context)
    (multiple-value-bind (value found-p)
        (apply #'u:href %resource-cache (list* entry-type keys))
      (when found-p
        (remhash (apply #'u:href %resource-cache (list* entry-type keys))
                 %resource-cache)
        (resource-cache-dispose context entry-type value)))))
