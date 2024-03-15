(in-package #:virality)

;;;; Implementation of IMAGE structure for HDR images
;;;; Uses HDR-IMAGE-BUFFER structure

(defun hdr-image-buffer-empty-p (buffer)
  (>= (buffer-position buffer)
      (end buffer)))

(defun refill-hdr-image-buffer (buffer)
  (when (hdr-image-buffer-empty-p buffer)
    (setf (buffer-position buffer) 0
          (end buffer) (read-sequence (data buffer) (buffer-stream buffer)))))

(defun hdr-image-eof-p (buffer)
  (refill-hdr-image-buffer buffer)
  (hdr-image-buffer-empty-p buffer))

(declaim (inline read-byte))
(defun read-hdr-image-byte (buffer)
  (if (hdr-image-eof-p buffer)
      (read-byte (buffer-stream buffer))
      (aref (data buffer) (1- (incf (buffer-position buffer))))))

(defun peek-hdr-image-byte (buffer)
  (if (hdr-image-eof-p buffer)
      (read-byte (buffer-stream buffer))
      (aref (data buffer) (buffer-position buffer))))

(defun read-hdr-image-line (buffer)
  (let ((n nil)
        (next nil))
    (prog1 (babel:octets-to-string
            (coerce (loop :until (or (member
                                      (setf n (peek-hdr-image-byte buffer))
                                      '(10 13))
                                     (hdr-image-eof-p buffer))
                          :collect (read-hdr-image-byte buffer))
                    '(vector u:ub8)))
      (unless (hdr-image-eof-p buffer)
        (loop :do (read-hdr-image-byte buffer)
              :while (and (not (hdr-image-eof-p buffer))
                          (not (eql n (setf next (peek-hdr-image-byte buffer))))
                          (member next '(10 13))))))))

(defun read-hdr-image-header (buffer)
  (labels ((trim (string)
             (string-trim '(#\space #\newline #\tab) string))
           (parse-key (line)
             (when (and line
                        (string/= line "")
                        (char/= #\# (char line 0)))
               (let* ((delimiter (position #\= line))
                      (key (u:make-keyword
                            (string-upcase (trim (subseq line 0 delimiter)))))
                      (value (trim (subseq line (1+ delimiter)))))
                 (when (eq key :format)
                   (let ((format (u:make-keyword
                                  (subseq (string-upcase value) 11 14))))
                     (unless (eq format :rgb)
                       (error "Unsupported HDR color space: ~a." format))
                     (list key format))))))
           (parse-xy (line)
             (destructuring-bind (axis1 dimension1 axis2 dimension2)
                 (split-sequence:split-sequence
                  #\space line :remove-empty-subseqs t)
               (unless (and (string= axis1 "-Y")
                            (string= axis2 "+X"))
                 (error "Unsupported HDR orientation: ~s." line))
               (list :width (parse-integer dimension2)
                     :height (parse-integer dimension1)))))
    (loop :for line = (read-hdr-image-line buffer)
          :for (k v) = (parse-key (trim line))
          :unless line
            :do (error "Invalid HDR header.")
          :until (zerop (length line))
          :if k
            :collect k :into kv
            :and
              :collect v :into kv
          :finally (return (append (parse-xy (read-hdr-image-line buffer))
                                   kv)))))

(defun read-hdr-image-scanline (buffer length destination &key (offset 0))
  (declare (optimize speed)
           (fixnum length offset))
  (let ((stream (buffer-stream buffer))
        (data (data buffer))
        (pos (buffer-position buffer))
        (end (end buffer)))
    (declare (type (simple-array u:ub32 (*)) destination)
             (type (simple-array u:ub8 (*)) data)
             (type u:ub24 pos end))
    (labels ((%read-byte ()
               (when (= pos end)
                 (setf pos 0
                       end (read-sequence data stream))
                 (when (zerop end)
                   (read-byte stream)))
               (aref data (1- (incf pos))))
             (read-pixel ()
               (values (%read-byte) (%read-byte) (%read-byte) (%read-byte)))
             (old-rle-p (r g b e)
               (when (= r g b 1)
                 e))
             (new-rle-p (r g b e)
               (when (and (= r g 2) (< b 127))
                 (dpb b (byte 7 8) e)))
             (write-pixel (p r g b e)
               (let ((w 0))
                 (setf (ldb (byte 8 1) w) r
                       (ldb (byte 8 10) w) g
                       (ldb (byte 8 19) w) b
                       (ldb (byte 5 27) w) (min 31 (max 0 (- e 113))))
                 (setf (aref destination (+ offset p)) w)))
             (write-component (p c v)
               (declare (type u:ub8 v))
               (let ((i (aref destination (+ offset p))))
                 (ecase c
                   (0 (setf (ldb (byte 8 1) i) v))
                   (1 (setf (ldb (byte 8 10) i) v))
                   (2 (setf (ldb (byte 8 19) i) v))
                   (3 (setf (ldb (byte 5 27) i) (min 31 (max 0 (- v 113)))))))))
      (declare (inline read-pixel old-rle-p new-rle-p write-pixel
                       write-component))
      (loop :with p :of-type u:ub24 = 0
            :with rle :of-type (or null u:ub16) = 0
            :with lr :of-type u:ub8 = 0
            :with lg :of-type u:ub8 = 0
            :with lb :of-type u:ub8 = 0
            :with le :of-type u:ub8 = 0
            :while (< p length)
            :do (u:mvlet ((r g b e (read-pixel)))
                  (declare (type u:ub8 r g b e))
                  (cond
                    ((setf rle (old-rle-p r g b e))
                     (loop :repeat rle
                           :do (write-pixel p r g b e))
                     (incf p rle))
                    ((setf rle (new-rle-p r g b e))
                     (loop :for c :below 4
                           :for p2 :of-type u:ub16 = 0
                           :do (loop :for r2 :of-type u:ub8 = 0
                                     :while (< p2 rle)
                                     :do (setf r2 (%read-byte))
                                         (if (> r2 128)
                                             (loop :with v = (%read-byte)
                                                   :repeat (ldb (byte 7 0) r2)
                                                   :do (write-component p2 c v)
                                                       (incf p2))
                                             (loop :repeat r2
                                                   :do (write-component
                                                        p2 c (%read-byte))
                                                       (incf p2)))))
                     (incf p rle))
                    (t
                     (write-pixel p r g b e)
                     (incf p)))
                  (setf lr r lg g lb b le e)))
      (setf (buffer-position buffer) pos
            (end buffer) end))))

(defmethod %load-image ((type (eql :hdr)) path &key flip-y)
  (u:with-binary-input (in path)
    (let* ((data (make-array 8192 :element-type 'u:ub8 :initial-element 0))
           (buffer (make-instance 'hdr-image-buffer
                                  :stream in
                                  :data data))
           (header (read-hdr-image-header buffer))
           (width (getf header :width))
           (height (getf header :height))
           (data (make-array (* width height)
                             :element-type 'u:ub32
                             :initial-element #xffffffff)))
      (if flip-y
          (loop :for y :from (1- height) :downto 0
                :do (read-hdr-image-scanline
                     buffer width data
                     :offset (* y width)))
          (loop :for y :below height
                :do (read-hdr-image-scanline
                     buffer width data :offset (* y width))))
      (make-instance 'image
                     :path path
                     :width width
                     :height height
                     :pixel-format :rgb
                     :pixel-type :unsigned-int-5-9-9-9-rev
                     :internal-format :rgb9-e5
                     :data data))))
