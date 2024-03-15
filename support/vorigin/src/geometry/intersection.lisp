(in-package #:vorigin.geometry)

(u:fn-> point2d/point2d (point2d:point point2d:point) boolean)
(declaim (inline point2d/point2d))
(defun point2d/point2d (point1 point2)
  "Test if two 2D points intersect."
  (declare (optimize speed))
  (v2:= point1 point2))

(u:fn-> %point2d/line2d (point2d:point line2d:line) boolean)
(declaim (inline %point2d/line2d))
(defun %point2d/line2d (point line)
  "Helper function that does the work for POINT2D/LINE2D and LINE2D/POINT2D."
  (declare (optimize speed))
  (v2:with-components ((p point)
                       (s (line2d:start line))
                       (e (line2d:end line)))
    (assert (/= sx ex))
    (let* ((dy (- ey sy))
           (dx (- ex sx))
           (m (/ dy dx))
           (b (- sy (* m sx))))
      (com:= py (+ (* m px) b)))))

(u:fn-> point2d/line2d (point2d:point line2d:line) boolean)
(defun point2d/line2d (point line)
  "Test if a 2D point is on a 2D line."
  (declare (optimize speed))
  (%point2d/line2d point line))

(u:fn-> line2d/point2d (line2d:line point2d:point) boolean)
(defun line2d/point2d (line point)
  "Test if a 2D line contains a 2D point."
  (declare (optimize speed))
  (%point2d/line2d point line))

(u:fn-> %point2d/circle (point2d:point circle:circle) boolean)
(declaim (inline %point2d/circle))
(defun %point2d/circle (point circle)
  "Helper function that does the work for POINT2D/CIRCLE and CIRCLE/POINT2D."
  (declare (optimize speed))
  (let ((line (line2d:line :start point :end (circle:origin circle))))
    (declare (dynamic-extent line))
    (< (line2d:length-squared line) (expt (circle:radius circle) 2))))

(u:fn-> point2d/circle (point2d:point circle:circle) boolean)
(defun point2d/circle (point circle)
  "Test if a 2D point is contained within a circle."
  (declare (optimize speed))
  (%point2d/circle point circle))

(u:fn-> circle/point2d (circle:circle point2d:point) boolean)
(defun circle/point2d (circle point)
  "Test if a circle contains a 2D point."
  (declare (optimize speed))
  (%point2d/circle point circle))

(u:fn-> %point2d/rect (point2d:point rect:rect) boolean)
(declaim (inline %point2d/rect))
(defun %point2d/rect (point rect)
  "Helper function that does the work for POINT2D/RECT and RECT/POINT2D."
  (declare (optimize speed))
  (and (v2:<= (rect:min rect) point)
       (v2:<= point (rect:max rect))))

(u:fn-> point2d/rect (point2d:point rect:rect) boolean)
(defun point2d/rect (point rect)
  "Test if a 2D point is contained within a rect."
  (declare (optimize speed))
  (%point2d/rect point rect))

(u:fn-> rect/point2d (rect:rect point2d:point) boolean)
(defun rect/point2d (rect point)
  "Test if a rect contains a 2D point."
  (declare (optimize speed))
  (%point2d/rect point rect))

(u:fn-> %point2d/oriented-rect (point2d:point orect:rect) boolean)
(declaim (inline %point2d/oriented-rect))
(defun %point2d/oriented-rect (point rect)
  "Helper function that does the work for POINT2D/ORIENTED-RECT and ORIENTED-RECT/POINT2D."
  (declare (optimize speed))
  (let ((vector (v2:- point (orect:origin rect)))
        (angle (- (orect:angle rect))))
    (declare (dynamic-extent vector))
    (m2:*v2! vector (m2:rotation-from-angle angle) vector)
    (v2:+! vector vector (orect:half-extents rect))
    (point2d/rect vector (rect:rect :size (v2:scale (orect:half-extents rect) 2f0)))))

(u:fn-> point2d/oriented-rect (point2d:point orect:rect) boolean)
(defun point2d/oriented-rect (point rect)
  "Test if a 2D point is contained within an oriented rect. See POINT2D/RECT for a less expensive
test if the rect is axis-aligned."
  (declare (optimize speed))
  (%point2d/oriented-rect point rect))

(u:fn-> oriented-rect/point2d (orect:rect point2d:point) boolean)
(defun oriented-rect/point2d (rect point)
  "Test if an oriented rect contains a 2D point. See RECT/POINT2D for a less expensive test if the
rect is axis-aligned."
  (declare (optimize speed))
  (%point2d/oriented-rect point rect))

(u:fn-> %point2d/shape-set-2d (point2d:point shape-set-2d:shape-set) boolean)
(declaim (inline %point2d/shape-set-2d))
(defun %point2d/shape-set-2d (point shape-set)
  "Helper function that does the work for POINT2D/SHAPE-SET-2D and SHAPE-SET-2D/POINT2D."
  (declare (optimize speed))
  (let ((circles (shape-set-2d:circles shape-set))
        (rects (shape-set-2d:rects shape-set))
        (oriented-rects (shape-set-2d:oriented-rects shape-set)))
    (dotimes (i (length circles))
      (when (point2d/circle point (svref circles i))
        (return-from %point2d/shape-set-2d t)))
    (dotimes (i (length rects))
      (when (point2d/rect point (svref rects i))
        (return-from %point2d/shape-set-2d t)))
    (dotimes (i (length oriented-rects))
      (when (point2d/oriented-rect point (svref oriented-rects i))
        (return-from %point2d/shape-set-2d t)))))

(u:fn-> point2d/shape-set-2d (point2d:point shape-set-2d:shape-set) boolean)
(defun point2d/shape-set-2d (point shape-set)
  "Test if a 2D point is contained within any of a 2D shape set's shapes."
  (declare (optimize speed))
  (%point2d/shape-set-2d point shape-set))

(u:fn-> shape-set-2d/point2d (shape-set-2d:shape-set point2d:point) boolean)
(defun shape-set-2d/point2d (shape-set point)
  "Test if any of a 2D shape set's shapes contains a 2D point."
  (declare (optimize speed))
  (%point2d/shape-set-2d point shape-set))

(u:fn-> %line2d/circle (line2d:line circle:circle) boolean)
(declaim (inline %line2d/circle))
(defun %line2d/circle (line circle)
  "Helper function that does the work for LINE2D/CIRCLE and CIRCLE/LINE2D."
  (declare (optimize speed))
  ;; Here, we find the closest point to the center of the circle on the line. Then we draw a line
  ;; from the center of the circle to this closest point. If the squared length of that line is less
  ;; than the squared radius of the circle, there is an intersection.
  (let* ((line-start (line2d:start line))
         (circle-origin (circle:origin circle))
         (ab (v2:- (line2d:end line) line-start))
         (direction (v2:- circle-origin line-start))
         (t-param (/ (v2:dot direction ab) (v2:length-squared ab))))
    (declare (dynamic-extent ab direction))
    (when (<= 0f0 t-param 1f0)
      (< (line2d:length-squared
          (line2d:line :start circle-origin :end (v2:+ line-start (v2:scale ab t-param))))
         (expt (circle:radius circle) 2)))))

(u:fn-> line2d/circle (line2d:line circle:circle) boolean)
(defun line2d/circle (line circle)
  "Test if a 2D line intersects a circle."
  (declare (optimize speed))
  (%line2d/circle line circle))

(u:fn-> circle/line2d (circle:circle line2d:line) boolean)
(defun circle/line2d (circle line)
  "Test if a circle intersects a 2D line."
  (declare (optimize speed))
  (%line2d/circle line circle))

(u:fn-> %line2d/rect (line2d:line rect:rect) boolean)
(declaim (inline %line2d/rect))
(defun %line2d/rect (line rect)
  "Helper function that does the work for LINE/RECT and RECT/LINE."
  (declare (optimize speed))
  (let ((start (line2d:start line)))
    ;; First, check if either end point of the line is contained in the rect. If yes, we have an
    ;; intersection and don't need to continue with the more expensive test.
    (when (or (point2d/rect start rect)
              (point2d/rect (line2d:end line) rect))
      (return-from %line2d/rect t))
    ;; Perform a raycast against the rect with a ray constructed from the line. If the ray hits,
    ;; and the length of the ray is less than the length of the line, there is an interesection.
    (let ((inv-dir (v2:invert (line2d:direction line))))
      (declare (dynamic-extent inv-dir))
      (v2:with-components ((min- (v2:* (v2:- (rect:min rect) start) inv-dir))
                           (max- (v2:* (v2:- (rect:max rect) start) inv-dir)))
        (let ((t-min (max (min min-x max-x) (min min-y max-y)))
              (t-max (min (max min-x max-x) (max min-y max-y))))
          (when (and (plusp t-max)
                     (<= t-min t-max))
            (let ((t-param (if (minusp t-min) t-max t-min)))
              (and (plusp t-param)
                   (< (expt t-param 2) (line2d:length-squared line))))))))))

(u:fn-> line2d/rect (line2d:line rect:rect) boolean)
(defun line2d/rect (line rect)
  "Test if a 2D line intersects a rect."
  (declare (optimize speed))
  (%line2d/rect line rect))

(u:fn-> rect/line2d (rect:rect line2d:line) boolean)
(defun rect/line2d (rect line)
  "Test if a rect intersects a 2D line."
  (declare (optimize speed))
  (%line2d/rect line rect))

(u:fn-> %line2d/oriented-rect (line2d:line orect:rect) boolean)
(declaim (inline %line2d/oriented-rect))
(defun %line2d/oriented-rect (line rect)
  "Helper function that does the work for LINE/ORIENTED-RECT and ORIENTED-RECT/LINE."
  (declare (optimize speed))
  ;; Here, we construct a line that is in the local space of the oriented rect. In its local space,
  ;; the oriented rect is just an axis-aligned rect, so we can just use the existing LINE/RECT test.
  (let* ((rect-origin (orect:origin rect))
         (half-extents (orect:half-extents rect))
         (vector (v2:- (line2d:start line) rect-origin))
         (rotation (m2:rotation-from-angle (- (orect:angle rect))))
         (local-line (line2d:line)))
    (declare (dynamic-extent vector rotation local-line))
    (m2:*v2! vector rotation vector)
    (v2:+! (line2d:start local-line) vector half-extents)
    (v2:-! vector (line2d:end line) rect-origin)
    (m2:*v2! vector rotation vector)
    (v2:+! (line2d:end local-line) vector half-extents)
    (line2d/rect local-line (rect:rect :size (v2:scale (orect:half-extents rect) 2f0)))))

(u:fn-> line2d/oriented-rect (line2d:line orect:rect) boolean)
(defun line2d/oriented-rect (line rect)
  "Test if a 2D line intersects an oriented rect."
  (declare (optimize speed))
  (%line2d/oriented-rect line rect))

(u:fn-> oriented-rect/line2d (orect:rect line2d:line) boolean)
(defun oriented-rect/line2d (rect line)
  "Test if an oriented rect intersects a 2D line."
  (declare (optimize speed))
  (%line2d/oriented-rect line rect))

(u:fn-> circle/circle (circle:circle circle:circle) boolean)
(defun circle/circle (circle1 circle2)
  "Test if two circles intersect."
  (declare (optimize speed))
  ;; First, construct a line between the two circle centers. Then, if the length of the line is less
  ;; than the sum of the two circle radii, we have an intersection. To avoid the square root, we get
  ;; the squared length of the line and compare it to the squared sum of the radii.
  (<= (point2d:distance-squared (circle:origin circle1) (circle:origin circle2))
      (expt (+ (circle:radius circle1) (circle:radius circle2)) 2)))

(u:fn-> %circle/rect (circle:circle rect:rect) boolean)
(declaim (inline %circle/rect))
(defun %circle/rect (circle rect)
  "Helper function that does the work for CIRCLE/RECT and RECT/CIRCLE."
  (declare (optimize speed))
  ;; First, we clamp the circle's center point to the rect's minimum and maximum bounds, which
  ;; places the closest point to the circle on the surface of the rect. Then, we draw a line from
  ;; the closest point to the center of the circle, and if it's less than the squared radius of the
  ;; circle, they intersect.
  (let ((circle-origin (circle:origin circle)))
    (<= (line2d:length-squared
         (line2d:line :start circle-origin
                      :end (v2:clamp circle-origin (rect:min rect) (rect:max rect))))
        (expt (circle:radius circle) 2))))

(u:fn-> circle/rect (circle:circle rect:rect) boolean)
(defun circle/rect (circle rect)
  "Test if a circle intersects a rect."
  (declare (optimize speed))
  (%circle/rect circle rect))

(u:fn-> rect/circle (rect:rect circle:circle) boolean)
(defun rect/circle (rect circle)
  "Test if a rect intersects a circle."
  (declare (optimize speed))
  (%circle/rect circle rect))

(u:fn-> %circle/oriented-rect (circle:circle orect:rect) boolean)
(declaim (inline %circle/oriented-rect))
(defun %circle/oriented-rect (circle rect)
  "Helper function that does the work for CIRCLE/ORIENTED-RECT and ORIENTED-RECT/CIRCLE."
  (declare (optimize speed))
  ;; First, we move the circle into the local space of the oriented rect by translating the center
  ;; of the circle relative to the center of the rect. Then, we rotate the translated point in the
  ;; negative orientation of the rect. We can then perform a check with the existing CIRCLE/RECT
  ;; test to see if they intersect.
  (let ((half-extents (orect:half-extents rect))
        (vector (v2:- (circle:origin circle) (orect:origin rect)))
        (rotation (m2:rotation-from-angle (- (orect:angle rect)))))
    (declare (dynamic-extent vector rotation))
    (m2:*v2! vector rotation vector)
    (v2:+! vector vector half-extents)
    (circle/rect (circle:circle :origin vector :radius (circle:radius circle))
                 (rect:rect :size (v2:scale half-extents 2f0)))))

(u:fn-> circle/oriented-rect (circle:circle orect:rect) boolean)
(defun circle/oriented-rect (circle rect)
  "Test if a circle intersects an oriented rect."
  (declare (optimize speed))
  (%circle/oriented-rect circle rect))

(u:fn-> oriented-rect/circle (orect:rect circle:circle) boolean)
(defun oriented-rect/circle (rect circle)
  "Test if an oriented rect intersects a circle."
  (declare (optimize speed))
  (%circle/oriented-rect circle rect))

(u:fn-> rect/rect (rect:rect rect:rect) boolean)
(defun rect/rect (rect1 rect2)
  "Test if two rects intersect."
  (declare (optimize speed))
  (and (v2:<= (rect:min rect1) (rect:max rect2))
       (v2:<= (rect:min rect2) (rect:max rect1))))

(u:fn-> %rect/oriented-rect (rect:rect orect:rect) boolean)
(declaim (inline %rect/oriented-rect))
(defun %rect/oriented-rect (rect1 rect2)
  "Helper function that does the work for RECT/ORIENTED-RECT and ORIENTED-RECT/RECT."
  (declare (optimize speed))
  (v2:with-components ((h (orect:half-extents rect2)))
    (let ((axes (vector v2:+right+ v2:+up+ (v2:zero) (v2:zero)))
          (rotation (m2:rotation-from-angle (orect:angle rect2)))
          (axis3 (v2:normalize (v2:vec hx 0f0)))
          (axis4 (v2:normalize (v2:vec 0f0 hy))))
      (declare (dynamic-extent axes rotation axis3 axis4))
      (m2:*v2! (aref axes 2) rotation axis3)
      (m2:*v2! (aref axes 3) rotation axis4)
      (map nil
           (lambda (x)
             (v2:with-components ((i1 (rect::interval rect1 x))
                                  (i2 (orect::interval rect2 x)))
               (unless (and (<= i2x i1y) (<= i1x i2y))
                 (return-from %rect/oriented-rect nil))))
           axes)
      t)))

(u:fn-> rect/oriented-rect (rect:rect orect:rect) boolean)
(defun rect/oriented-rect (rect1 rect2)
  "Test if a rect and an oriented rect intersect."
  (declare (optimize speed))
  (%rect/oriented-rect rect1 rect2))

(u:fn-> oriented-rect/rect (orect:rect rect:rect) boolean)
(defun oriented-rect/rect (rect1 rect2)
  "Test if an oriented rect and a rect intersect."
  (declare (optimize speed))
  (%rect/oriented-rect rect2 rect1))

(u:fn-> oriented-rect/oriented-rect (orect:rect orect:rect) boolean)
(defun oriented-rect/oriented-rect (rect1 rect2)
  "Test if two oriented rects intersect."
  (declare (optimize speed))
  (let* ((angle1 (orect:angle rect1))
         (half-extents1 (orect:half-extents rect1))
         (origin2 (v2:copy (orect:origin rect2)))
         (local1 (rect:rect :size (v2:scale half-extents1 2f0)))
         (local2 (orect:rect :origin origin2
                             :half-extents (orect:half-extents rect2)
                             :angle (- (orect:angle rect2) angle1)))
         (vector (v2:- origin2 (orect:origin rect1)))
         (rotation (m2:rotation-from-angle (- angle1))))
    (declare (dynamic-extent origin2 local1 local2 vector rotation))
    (m2:*v2! vector rotation vector)
    (v2:+! (orect:origin local2) vector half-extents1)
    (rect/oriented-rect local1 local2)))

(u:fn-> %shape-set-2d/line2d (shape-set-2d:shape-set line2d:line) boolean)
(declaim (inline %shape-set-2d/line-2d))
(defun %shape-set-2d/line2d (shape-set line)
  "Helper function that does the work for SHAPE-SET-2D/LINE2D and LINE2D/SHAPE-SET-2D."
  (declare (optimize speed))
  (let ((circles (shape-set-2d:circles shape-set)))
    (dotimes (i (length circles))
      (when (line2d/circle line (svref circles i))
        (return-from %shape-set-2d/line2d t))))
  (let ((rects (shape-set-2d:rects shape-set)))
    (dotimes (i (length rects))
      (when (line2d/rect line (svref rects i))
        (return-from %shape-set-2d/line2d t))))
  (let ((oriented-rects (shape-set-2d:oriented-rects shape-set)))
    (dotimes (i (length oriented-rects))
      (when (line2d/oriented-rect line (svref oriented-rects i))
        (return-from %shape-set-2d/line2d t)))))

(u:fn-> shape-set-2d/line2d (shape-set-2d:shape-set line2d:line) boolean)
(defun shape-set-2d/line2d (shape-set line)
  "Test if any of a 2D shape set's shapes intersect a 2D line."
  (declare (optimize speed))
  (%shape-set-2d/line2d shape-set line))

(u:fn-> line2d/shape-set-2d (line2d:line shape-set-2d:shape-set) boolean)
(defun line2d/shape-set-2d (line shape-set)
  "Test if a 2D line intersects any of a 2D shape set's shapes."
  (declare (optimize speed))
  (%shape-set-2d/line2d shape-set line))

(u:fn-> %shape-set-2d/circle (shape-set-2d:shape-set circle:circle) boolean)
(declaim (inline %shape-set-2d/circle))
(defun %shape-set-2d/circle (shape-set circle)
  "Helper function that does the work for SHAPE-SET-2D/CIRCLE and CIRCLE/SHAPE-SET-2D."
  (declare (optimize speed))
  (let ((circles (shape-set-2d:circles shape-set)))
    (dotimes (i (length circles))
      (when (circle/circle circle (svref circles i))
        (return-from %shape-set-2d/circle t))))
  (let ((rects (shape-set-2d:rects shape-set)))
    (dotimes (i (length rects))
      (when (circle/rect circle (svref rects i))
        (return-from %shape-set-2d/circle t))))
  (let ((oriented-rects (shape-set-2d:oriented-rects shape-set)))
    (dotimes (i (length oriented-rects))
      (when (circle/oriented-rect circle (svref oriented-rects i))
        (return-from %shape-set-2d/circle t)))))

(u:fn-> shape-set-2d/circle (shape-set-2d:shape-set circle:circle) boolean)
(defun shape-set-2d/circle (shape-set circle)
  "Test if any of a 2D shape set's shapes intersects a circle."
  (declare (optimize speed))
  (%shape-set-2d/circle shape-set circle))

(u:fn-> circle/shape-set-2d (circle:circle shape-set-2d:shape-set) boolean)
(defun circle/shape-set-2d (circle shape-set)
  "Test if a circle intersects any of a 2D shape set's shapes."
  (declare (optimize speed))
  (%shape-set-2d/circle shape-set circle))

(u:fn-> %shape-set-2d/rect (shape-set-2d:shape-set rect:rect) boolean)
(declaim (inline %shape-set-2d/rect))
(defun %shape-set-2d/rect (shape-set rect)
  "Helper function that does the work for SHAPE-SET-2D/RECT and RECT/SHAPE-SET-2D."
  (declare (optimize speed))
  (let ((circles (shape-set-2d:circles shape-set)))
    (dotimes (i (length circles))
      (when (rect/circle rect (svref circles i))
        (return-from %shape-set-2d/rect t))))
  (let ((rects (shape-set-2d:rects shape-set)))
    (dotimes (i (length rects))
      (when (rect/rect rect (svref rects i))
        (return-from %shape-set-2d/rect t))))
  (let ((oriented-rects (shape-set-2d:oriented-rects shape-set)))
    (dotimes (i (length oriented-rects))
      (when (rect/oriented-rect rect (svref oriented-rects i))
        (return-from %shape-set-2d/rect t)))))

(u:fn-> shape-set-2d/rect (shape-set-2d:shape-set rect:rect) boolean)
(defun shape-set-2d/rect (shape-set rect)
  "Test if any of a 2D shape set's shapes intersects a rect."
  (declare (optimize speed))
  (%shape-set-2d/rect shape-set rect))

(u:fn-> rect/shape-set-2d (rect:rect shape-set-2d:shape-set) boolean)
(defun rect/shape-set-2d (rect shape-set)
  "Test if a rect intersects any of a 2D shape set's shapes."
  (declare (optimize speed))
  (%shape-set-2d/rect shape-set rect))

(u:fn-> %shape-set-2d/oriented-rect (shape-set-2d:shape-set orect:rect) boolean)
(declaim (inline %shape-set-2d/oriented-rect))
(defun %shape-set-2d/oriented-rect (shape-set rect)
  "Helper function that does the work for SHAPE-SET-2D/ORIENTED-RECT and
ORIENTED-RECT/SHAPE-SET-2D."
  (declare (optimize speed))
  (let ((circles (shape-set-2d:circles shape-set)))
    (dotimes (i (length circles))
      (when (oriented-rect/circle rect (svref circles i))
        (return-from %shape-set-2d/oriented-rect t))))
  (let ((rects (shape-set-2d:rects shape-set)))
    (dotimes (i (length rects))
      (when (oriented-rect/rect rect (svref rects i))
        (return-from %shape-set-2d/oriented-rect t))))
  (let ((oriented-rects (shape-set-2d:oriented-rects shape-set)))
    (dotimes (i (length oriented-rects))
      (when (oriented-rect/oriented-rect rect (svref oriented-rects i))
        (return-from %shape-set-2d/oriented-rect t)))))

(u:fn-> shape-set-2d/oriented-rect (shape-set-2d:shape-set orect:rect) boolean)
(defun shape-set-2d/oriented-rect (shape-set rect)
  "Test if any of a 2D shape set's shapes intersects an oriented rect."
  (declare (optimize speed))
  (%shape-set-2d/oriented-rect shape-set rect))

(u:fn-> oriented-rect/shape-set-2d (orect:rect shape-set-2d:shape-set) boolean)
(defun oriented-rect/shape-set-2d (rect shape-set)
  "Test if an oriented rect intersects any of a 2D shape set's shapes."
  (declare (optimize speed))
  (%shape-set-2d/oriented-rect shape-set rect))

(u:fn-> shape-set-2d/shape-set-2d (shape-set-2d:shape-set shape-set-2d:shape-set) boolean)
(defun shape-set-2d/shape-set-2d (shape-set1 shape-set2)
  "Test if any two shapes of two 2D shape sets intersect. NOTE: This is O(n^2) in time, but usually
of a very small N."
  (declare (optimize speed))
  (let ((circles1 (shape-set-2d:circles shape-set1))
        (circles2 (shape-set-2d:circles shape-set2)))
    (dotimes (i (length circles1))
      (dotimes (j (length circles2))
        (when (circle/circle (svref circles1 i) (svref circles2 j))
          (return-from shape-set-2d/shape-set-2d t)))))
  (let ((rects1 (shape-set-2d:rects shape-set1))
        (rects2 (shape-set-2d:rects shape-set2)))
    (dotimes (i (length rects1))
      (dotimes (j (length rects2))
        (when (rect/rect (svref rects1 i) (svref rects2 j))
          (return-from shape-set-2d/shape-set-2d t)))))
  (let ((oriented-rects1 (shape-set-2d:oriented-rects shape-set1))
        (oriented-rects2 (shape-set-2d:oriented-rects shape-set2)))
    (dotimes (i (length oriented-rects1))
      (dotimes (j (length oriented-rects2))
        (when (oriented-rect/oriented-rect (svref oriented-rects1 i) (svref oriented-rects2 j))
          (return-from shape-set-2d/shape-set-2d t))))))

(u:fn-> point3d/point3d (point3d:point point3d:point) boolean)
(declaim (inline point3d/point3d))
(defun point3d/point3d (point1 point2)
  "Test if two 3D points intersect."
  (declare (optimize speed))
  (v3:= point1 point2))

(u:fn-> %point3d/sphere (point3d:point sphere:sphere) boolean)
(declaim (inline %point3d/sphere))
(defun %point3d/sphere (point sphere)
  "Helper function that does the work for POINT3D/SPHERE and SPHERE/POINT3D."
  (declare (optimize speed))
  (let ((line (line3d:line :start point :end (sphere:origin sphere))))
    (< (line3d:length-squared line) (expt (sphere:radius sphere) 2))))

(u:fn-> point3d/sphere (point3d:point sphere:sphere) boolean)
(defun point3d/sphere (point sphere)
  "Test if a 3D point is contained within a sphere."
  (declare (optimize speed))
  (%point3d/sphere point sphere))

(u:fn-> sphere/point3d (sphere:sphere point3d:point) boolean)
(defun sphere/point3d (sphere point)
  "Test if a sphere contains a 3D point."
  (declare (optimize speed))
  (%point3d/sphere point sphere))

(u:fn-> %point3d/aabb (point3d:point aabb:aabb) boolean)
(declaim (inline %point3d/aabb))
(defun %point3d/aabb (point aabb)
  "Helper function that does the work for POINT3D/AABB and AABB/POINT3D."
  (declare (optimize speed))
  (and (v3:<= (aabb:min aabb) point)
       (v3:<= point (aabb:max aabb))))

(u:fn-> point3d/aabb (point3d:point aabb:aabb) boolean)
(defun point3d/aabb (point aabb)
  "Test if a 3D point is contained within an AABB."
  (declare (optimize speed))
  (%point3d/aabb point aabb))

(u:fn-> aabb/point3d (aabb:aabb point3d:point) boolean)
(defun aabb/point3d (aabb point)
  "Test if an AABB contains a 3D point."
  (declare (optimize speed))
  (%point3d/aabb point aabb))

(u:fn-> %point3d/obb (point3d:point obb:obb) boolean)
(declaim (inline %point3d/obb))
(defun %point3d/obb (point obb)
  "Helper function that does the work for POINT3D/OBB and OBB/POINT3D."
  (declare (optimize speed))
  (let* ((direction (v3:- point (obb:origin obb)))
         (size (obb:size obb))
         (-size (v3:negate size))
         (rotation (obb:rotation obb))
         (distances (v3:vec (v3:dot direction (m3:get-column rotation 0))
                            (v3:dot direction (m3:get-column rotation 1))
                            (v3:dot direction (m3:get-column rotation 2)))))
    (declare (dynamic-extent direction -size distances))
    (and (v3:<= distances size)
         (v3:>= distances -size))))

(u:fn-> point3d/obb (point3d:point obb:obb) boolean)
(defun point3d/obb (point obb)
  "Test if a 3D point is contained within an OBB."
  (declare (optimize speed))
  (%point3d/obb point obb))

(u:fn-> obb/point3d (obb:obb point3d:point) boolean)
(defun obb/point3d (obb point)
  "Test if an OBB contains a 3D point."
  (declare (optimize speed))
  (%point3d/obb point obb))

(u:fn-> %point3d/plane (point3d:point plane:plane) boolean)
(declaim (inline %point3d/plane))
(defun %point3d/plane (point plane)
  "Helper function that does the work for POINT3D/PLANE and PLANE/POINT3D."
  (declare (optimize speed))
  (let ((dot (v3:dot point (plane:normal plane))))
    (com:= (- dot (plane:distance plane)) 0f0)))

(u:fn-> point3d/plane (point3d:point plane:plane) boolean)
(defun point3d/plane (point plane)
  "Test if a 3D point is on a plane."
  (declare (optimize speed))
  (%point3d/plane point plane))

(u:fn-> plane/point3d (plane:plane point3d:point) boolean)
(defun plane/point3d (plane point)
  "Test is a plane contains a 3D point."
  (declare (optimize speed))
  (%point3d/plane point plane))

(u:fn-> %point3d/line3d (point3d:point line3d:line) boolean)
(declaim (inline %point3d/line3d))
(defun %point3d/line3d (point line)
  "Helper function that does the work for POINT3D/LINE3D and LINE3D/POINT3D."
  (declare (optimize speed))
  (let* ((closest-point (closest-point-line3d line point))
         (vector (v3:- closest-point point)))
    (declare (dynamic-extent vector))
    (com:= (v3:length-squared vector) 0f0)))

(u:fn-> point3d/line3d (point3d:point line3d:line) boolean)
(defun point3d/line3d (point line)
  "Test if a 3D point is on a 3D line."
  (declare (optimize speed))
  (%point3d/line3d point line))

(u:fn-> line3d/point3d (line3d:line point3d:point) boolean)
(defun line3d/point3d (line point)
  "Test if a 3D line contains a 3D point."
  (declare (optimize speed))
  (%point3d/line3d point line))

(u:fn-> %point3d/ray (point3d:point ray:ray) boolean)
(declaim (inline %point3d/ray))
(defun %point3d/ray (point ray)
  "Helper function that does the work for POINT3D/RAY and RAY/POINT3D."
  (declare (optimize speed))
  (let ((origin (ray:origin ray)))
    (when (v3:= point origin)
      (return-from %point3d/ray t))
    (let ((normal (v3:- point origin)))
      (declare (dynamic-extent normal))
      (v3:normalize! normal normal)
      (com:= (v3:dot normal (ray:direction ray)) 1f0))))

(u:fn-> point3d/ray (point3d:point ray:ray) boolean)
(defun point3d/ray (point ray)
  "Test if a 3D point is on a ray."
  (declare (optimize speed))
  (%point3d/ray point ray))

(u:fn-> ray/point3d (ray:ray point3d:point) boolean)
(defun ray/point3d (ray point)
  "Test if a ray contains a 3D point."
  (declare (optimize speed))
  (%point3d/ray point ray))

(u:fn-> sphere/sphere (sphere:sphere sphere:sphere) boolean)
(defun sphere/sphere (sphere1 sphere2)
  "Test if two spheres intersect."
  (declare (optimize speed))
  (<= (point3d:distance-squared (sphere:origin sphere1) (sphere:origin sphere2))
      (expt (+ (sphere:radius sphere1) (sphere:radius sphere2)) 2)))

(u:fn-> %sphere/aabb (sphere:sphere aabb:aabb) boolean)
(declaim (inline %sphere/aabb))
(defun %sphere/aabb (sphere aabb)
  "Helper function that does the work for SPHERE/AABB and AABB/SPHERE."
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (closest-point-aabb aabb sphere-origin)))
    (declare (dynamic-extent closest-point))
    (< (point3d:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/aabb (sphere:sphere aabb:aabb) boolean)
(defun sphere/aabb (sphere aabb)
  "Test if a sphere intersects an AABB."
  (declare (optimize speed))
  (%sphere/aabb sphere aabb))

(u:fn-> aabb/sphere (aabb:aabb sphere:sphere) boolean)
(defun aabb/sphere (aabb sphere)
  "Test if an AABB intersects a sphere."
  (declare (optimize speed))
  (%sphere/aabb sphere aabb))

(u:fn-> %sphere/obb (sphere:sphere obb:obb) boolean)
(declaim (inline %sphere/obb))
(defun %sphere/obb (sphere obb)
  "Helper function that does the work for SPHERE/OBB and OBB/SPHERE."
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (closest-point-obb obb sphere-origin)))
    (< (point3d:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/obb (sphere:sphere obb:obb) boolean)
(defun sphere/obb (sphere obb)
  "Test if a sphere intersects an OBB."
  (declare (optimize speed))
  (%sphere/obb sphere obb))

(u:fn-> obb/sphere (obb:obb sphere:sphere) boolean)
(defun obb/sphere (obb sphere)
  "Test if an OBB intersects a sphere."
  (declare (optimize speed))
  (%sphere/obb sphere obb))

(u:fn-> %sphere/plane (sphere:sphere plane:plane) boolean)
(declaim (inline %sphere/plane))
(defun %sphere/plane (sphere plane)
  "Helper function that does the work for SPHERE/PLANE and PLANE/SPHERE."
  (declare (optimize speed))
  (let* ((sphere-origin (sphere:origin sphere))
         (closest-point (closest-point-plane plane sphere-origin)))
    (< (point3d:distance-squared closest-point sphere-origin)
       (expt (sphere:radius sphere) 2))))

(u:fn-> sphere/plane (sphere:sphere plane:plane) boolean)
(defun sphere/plane (sphere plane)
  "Test if a sphere intersects a plane."
  (declare (optimize speed))
  (%sphere/plane sphere plane))

(u:fn-> plane/sphere (plane:plane sphere:sphere) boolean)
(defun plane/sphere (plane sphere)
  "Test if a plane intersects a sphere."
  (declare (optimize speed))
  (%sphere/plane sphere plane))

(u:fn-> aabb/aabb (aabb:aabb aabb:aabb) boolean)
(defun aabb/aabb (aabb1 aabb2)
  "Test if two AABBs intersect."
  (declare (optimize speed))
  (and (v3:<= (aabb:min aabb1) (aabb:max aabb2))
       (v3:<= (aabb:min aabb2) (aabb:max aabb1))))

(u:fn-> %aabb/obb (aabb:aabb obb:obb) boolean)
(declaim (inline %aabb/obb))
(defun %aabb/obb (aabb obb)
  "Helper function that does the work for AABB/OBB and OBB/AABB."
  (declare (optimize speed))
  (let* ((rotation (obb:rotation obb))
         (obb-x (m3:get-column rotation 0))
         (obb-y (m3:get-column rotation 1))
         (obb-z (m3:get-column rotation 2))
         (axes (vector v3:+right+
                       v3:+up+
                       v3:+forward+
                       obb-x
                       obb-y
                       obb-z
                       (v3:cross obb-x v3:+right+)
                       (v3:cross obb-x v3:+up+)
                       (v3:cross obb-x v3:+forward+)
                       (v3:cross obb-y v3:+right+)
                       (v3:cross obb-y v3:+up+)
                       (v3:cross obb-y v3:+forward+)
                       (v3:cross obb-z v3:+right+)
                       (v3:cross obb-z v3:+up+)
                       (v3:cross obb-z v3:+forward+))))
    (declare (dynamic-extent obb-x obb-y obb-z axes))
    (dotimes (i 15)
      (let ((x (aref axes i)))
        (v2:with-components ((i1 (aabb::interval aabb x))
                             (i2 (obb::interval obb x)))
          (unless (and (<= i2x i1y) (<= i1x i2y))
            (return-from %aabb/obb nil)))))
    t))

(u:fn-> aabb/obb (aabb:aabb obb:obb) boolean)
(defun aabb/obb (aabb obb)
  "Test if an AABB intersects an OBB."
  (declare (optimize speed))
  (%aabb/obb aabb obb))

(u:fn-> obb/aabb (obb:obb aabb:aabb) boolean)
(defun obb/aabb (obb aabb)
  "Test if an OBB intersects an AABB."
  (declare (optimize speed))
  (%aabb/obb aabb obb))

(u:fn-> %aabb/plane (aabb:aabb plane:plane) boolean)
(declaim (inline %aabb/plane))
(defun %aabb/plane (aabb plane)
  "Helper function that does the work for AABB/PLANE and PLANE/AABB."
  (declare (optimize speed))
  (let ((normal (plane:normal plane)))
    (<= (abs (- (v3:dot normal (aabb:origin aabb))
                (plane:distance plane)))
        (v3:dot (aabb:size aabb) (v3:abs normal)))))

(u:fn-> aabb/plane (aabb:aabb plane:plane) boolean)
(defun aabb/plane (aabb plane)
  "Test if an AABB intersects a plane."
  (declare (optimize speed))
  (%aabb/plane aabb plane))

(u:fn-> plane/aabb (plane:plane aabb:aabb) boolean)
(defun plane/aabb (plane aabb)
  "Test if a plane intersects an AABB."
  (declare (optimize speed))
  (%aabb/plane aabb plane))

(u:fn-> obb/obb (obb:obb obb:obb) boolean)
(defun obb/obb (obb1 obb2)
  "Test if two OBB's intersect."
  (declare (optimize speed))
  (let* ((rotation1 (obb:rotation obb1))
         (rotation2 (obb:rotation obb2))
         (obb1-x (m3:get-column rotation1 0))
         (obb1-y (m3:get-column rotation1 1))
         (obb1-z (m3:get-column rotation1 2))
         (obb2-x (m3:get-column rotation2 0))
         (obb2-y (m3:get-column rotation2 1))
         (obb2-z (m3:get-column rotation2 2))
         (axes (vector obb1-x
                       obb1-y
                       obb1-z
                       obb2-x
                       obb2-y
                       obb2-z
                       (v3:cross obb2-x obb1-x)
                       (v3:cross obb2-x obb1-y)
                       (v3:cross obb2-x obb1-z)
                       (v3:cross obb2-y obb1-x)
                       (v3:cross obb2-y obb1-y)
                       (v3:cross obb2-y obb1-z)
                       (v3:cross obb2-z obb1-x)
                       (v3:cross obb2-z obb1-y)
                       (v3:cross obb2-z obb1-z))))
    (declare (dynamic-extent obb1-x obb1-y obb1-z obb2-x obb2-y obb2-z axes))
    (dotimes (i 15)
      (let ((x (aref axes i)))
        (v2:with-components ((i1 (obb::interval obb1 x))
                             (i2 (obb::interval obb2 x)))
          (unless (and (<= i2x i1y) (<= i1x i2y))
            (return-from obb/obb nil)))))
    t))

(u:fn-> %obb/plane (obb:obb plane:plane) boolean)
(declaim (inline %obb/plane))
(defun %obb/plane (obb plane)
  "Helper function that does the work for OBB/PLANE and PLANE/OBB."
  (declare (optimize speed))
  (let* ((normal (plane:normal plane))
         (rotation (obb:rotation obb))
         (x (m3:get-column rotation 0))
         (y (m3:get-column rotation 1))
         (z (m3:get-column rotation 2))
         (vector (v3:abs (v3:vec (v3:dot normal x) (v3:dot normal y) (v3:dot normal z)))))
    (declare (dynamic-extent x y z vector))
    (<= (abs (- (v3:dot normal (obb:origin obb))
                (plane:distance plane)))
        (v3:dot (obb:size obb) vector))))

(u:fn-> obb/plane (obb:obb plane:plane) boolean)
(defun obb/plane (obb plane)
  "Test if an OBB intersects a plane."
  (declare (optimize speed))
  (%obb/plane obb plane))

(u:fn-> plane/obb (plane:plane obb:obb) boolean)
(defun plane/obb (plane obb)
  "Test if a plane intersects an OBB."
  (declare (optimize speed))
  (%obb/plane obb plane))

(u:fn-> plane/plane (plane:plane plane:plane) boolean)
(defun plane/plane (plane1 plane2)
  "Test if two planes intersect."
  (declare (optimize speed))
  (let ((direction (v3:cross (plane:normal plane1) (plane:normal plane2))))
    (declare (dynamic-extent direction))
    (not (com:= (v3:length-squared direction) 0f0))))
