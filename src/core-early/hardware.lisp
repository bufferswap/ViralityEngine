(in-package #:virality)

(global-vars:define-global-var =cpu= "Unknown")
(global-vars:define-global-var =cpu-count= 1)
(global-vars:define-global-var =gpu-vendor= "Unknown")
(global-vars:define-global-var =gpu-device= "Unknown")
(global-vars:define-global-var =gpu-make/model= "Unknown")
(global-vars:define-global-var =gpu-version= "Unknown")
(global-vars:define-global-var =max-texture-size= 0)
(global-vars:define-global-var =max-ssbo-bindings= 0)

;;;; TODO: CONSTRUT NEW GPU HARDWARE ABSTRACTION UTIL SYSTEM AND PUT MOST OF
;;;; THIS THERE.


(defun get-gpu-parameter (name)
  (or (handler-case (gl:get* name)
        (error (e)
          (declare (ignore e))
          "Unavailable"))
      "Unavailable"))

(defun get-gpu-vendor-full ()
  (get-gpu-parameter :vendor))

(defun get-gpu-vendor ()
  (let ((vendor (get-gpu-vendor-full)))
    (cond
      ((search "Intel" vendor) :intel)
      ((search "NVIDIA" vendor) :nvidia)
      ((search "AMD" vendor) :amd)
      ((search "ATI" vendor) :amd)
      (t :unavailable))))

(defun get-gpu-device ()
  (get-gpu-parameter :renderer))

(defun get-gpu-make/model ()
  (let ((vendor (get-gpu-vendor-full)))
    (case vendor
      (:unavailable "Unknown")
      (t (format nil "~a - ~a" vendor (get-gpu-device))))))

(defun get-gpu-version ()
  (get-gpu-parameter :version))

(defun get-gpu/max-texture-size ()
  (get-gpu-parameter :max-texture-size))

(defun get-gpu/max-ssbo-bindings ()
  (get-gpu-parameter :max-shader-storage-buffer-bindings))


;;;; TODO: This part stays in the engine code cause it uses the abstraction.

(defun load-hardware-info ()
  (setf =cpu= (machine-version)
        =cpu-count= (cl-cpus:get-number-of-processors)
        =gpu-vendor= (get-gpu-vendor)
        =gpu-device= (get-gpu-device)
        =gpu-make/model= (get-gpu-make/model)
        =gpu-version= (get-gpu-version)
        =max-texture-size= (get-gpu/max-texture-size)
        =max-ssbo-bindings= (get-gpu/max-ssbo-bindings)))

(defun get-hardware-info (key)
  (let ((global (u:format-symbol :virality "=~a=" key)))
    (when (boundp global)
      (symbol-value global))))
