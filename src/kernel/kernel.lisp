(in-package #:virality)

;;;; Implementation of datatype KERNEL

;;;; A KERNEL is the base of an ACTOR or COMPONENT type. It it used for bookeeping
;;;; those objects, of which there may be MANY in a game.

;; NOTE: This ensures that all kernels have a core so we don't have to go
;; through the context to get it all over the codebase.
(defmethod initialize-instance :after ((instance kernel) &key)
  (setf (slot-value instance '%core) (core (context instance))))

(u:define-printer (kernel stream)
  (format stream "~a" (display-id kernel)))

;;; Query table management

(defun register-kernel-uuid (kernel)
  (u:if-let ((table (kernels-by-uuid (tables (core kernel))))
             (uuid (uuid kernel)))
    (u:if-found (found (u:href table uuid))
      (error "A UUID collision occured between the following ~
                           kernels:~%~s~%~s."
             found kernel)
      (setf (u:href table uuid) kernel))
    (error "Kernel ~s has no UUID. This is a bug and should be reported."
           kernel)))

(defun register-kernel-id-in-table (kernel table)
  (u:when-let ((id (id kernel)))
    (unless (u:href table id)
      (setf (u:href table id) (u:dict #'eq)))
    (setf (u:href table id kernel) kernel)))

(defgeneric register-kernel-id (kernel))
(defgeneric deregister-kernel-id (kernel))

(defun deregister-kernel-uuid (kernel)
  (remhash (uuid kernel) (kernels-by-uuid (tables (core kernel)))))

(defun deregister-kernel-id-from-table (kernel table)
  (u:when-let ((id (id kernel)))
    (symbol-macrolet ((place (u:href table id)))
      (remhash kernel place)
      (unless (plusp (hash-table-count place))
        (remhash id table)))))

;;; Public API

(defmethod (setf id) (value (kernel kernel))
  "Change the ID of a kernel object."
  (with-slots (%id) kernel
    (when %id
      (deregister-kernel-id kernel))
    (setf %id value)
    (register-kernel-id kernel)))

(defmethod find-by-uuid (context (uuid uuid))
  "Return the kernel instance with the given `UUID` object."
  (let ((table (kernels-by-uuid (tables (core context)))))
    (u:href table uuid)))

(defmethod find-by-uuid (context (uuid string))
  "Return the kernel instance with the given `UUID` string representation."
  (let ((table (kernels-by-uuid (tables (core context)))))
    (u:href table (string->uuid uuid))))

(defun find-actors-by-id (context id)
  "Return a list of all actor instances with the given `ID`."
  (u:when-let* ((table (actors-by-id (tables (core context))))
                (by-id (u:href table id)))
    (u:hash-values by-id)))

(defun find-components-by-id (context id)
  "Return a list of all component instances with the given `ID`."
  (u:when-let* ((table (components-by-id (tables (core context))))
                (by-id (u:href table id)))
    (u:hash-values by-id)))
