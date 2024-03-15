(in-package #:virality)

;;;; Implementation of the various GRAPH structures

(defun make-analyzed-graph (&rest init-args)
  (apply #'make-instance 'analyzed-graph init-args))


(defun make-graphdef (name &rest init-args)
  (apply #'make-instance 'graphdef :name name init-args))


(defun make-graphdef-depends-on (name &rest init-args)
  (apply #'make-instance 'graphdef-depends-on :name name init-args))


(defun make-subform (name &rest args)
  (apply #'make-instance 'subform :name name args))


(defun make-depform (&rest args)
  (apply #'make-instance 'depform args))

;;; annotation classes / protocol for different graph categories

(defgeneric generate-graph-annotation (category graph)
  (:documentation "Generate any annotations this analyzed-graph may need.")
  (:method (category graph)))

(defgeneric make-graph-annotation (category &rest args)
  (:documentation "Make an instance of an appropriate graph annotation ~
                   depending on cateogry.")
  (:method (category &rest args)
    (declare (ignore args))))

;; For category COMPONENT-DEPENDENCY

(defmethod make-graph-annotation ((category (eql 'component-dependency))
                                  &rest init-args)
  (apply #'make-instance 'graph-annotation/component-dependency init-args))

;; For category COMPONENT-PACKAGE-ORDER

(defmethod make-graph-annotation ((category (eql 'component-package-order))
                                  &rest init-args)
  (apply #'make-instance 'graph-annotation/component-package-order
         init-args))

(defun graph-roots (graph)
  "Find all vertex roots (vertexes with no parents) in the directed graph GRAPH
and return them as a list in no particular order."
  (let (results)
    (cl-graph:iterate-vertexes
     graph
     (lambda (v)
       (unless (cl-graph:parent-vertexes v)
         (push v results))))
    results))

(defun graph-leaves (graph)
  "Find all vertex leaves (vertexes with no children) in the graph GRAPH and
return them as a list."
  (let (results)
    (cl-graph:iterate-vertexes
     graph
     (lambda (v)
       (unless (cl-graph:child-vertexes v)
         (push v results))))
    results))

;; A crappy kind of pattern matching.
(defun is-syntax-form-p (syntax-symbol form)
  (cond
    ((and (consp syntax-symbol)
          (consp form))
     (u:symbol-name= (first syntax-symbol) (first form)))
    ((and (symbolp syntax-symbol)
          (symbolp form))
     (u:symbol-name= syntax-symbol form))
    ;; maybe other cases needed?
    (t nil)))

(defgeneric canonicalize-dependency-form (category dependency-form))

(defmethod canonicalize-dependency-form ((category (eql 'component-dependency))
                                         dependency-form)
  (loop :for element :in dependency-form
        :collect
        (cond
          ((consp element)
           element)
          ((is-syntax-form-p '-> element)
           element)
          (t
           `(component-type ,element)))))

(defmethod canonicalize-dependency-form ((category
                                          (eql 'component-package-order))
                                         dependency-form)
  (loop :for element :in dependency-form
        :collect
        (cond
          ((consp element)
           element)
          ((is-syntax-form-p '-> element)
           element)
          (t
           `(potential-package ,element)))))

(defun segment-dependency-form (category form)
  "Lift splices and then segment the dependency FORM into hyperedges. If the
form is null, return values: NIL, :empty If the form is not null, but contains
no hyper edges, return values: canonical-form and :vertex If the form is not
null, and contains hyper edges, return values: list of hyper-edge pairs,
:hyperedges"
  (let* ((canonical-form (canonicalize-dependency-form category form))
         (x (split-sequence:split-sequence
             '-> canonical-form :test #'u:symbol-name=))
         ;; cut into groups of two with rolling window
         (connections
           (loop :for (k j . nil) :in (maplist #'identity x)
                 :when j
                   :collect `(,k ,j))))
    (cond
      ((null canonical-form)
       (values canonical-form :empty))
      ((= (length x) 1)
       ;; no hyperedges
       (values canonical-form :vertex))
      (t ;; hyperedges found
       (values connections :hyperedges)))))

;; Then the code to perform the parsing.
(defun parse-subform (category form)
  (assert (or (is-syntax-form-p '(subdag) form)
              (is-syntax-form-p '(subgraph) form)))
  (destructuring-bind (kind name . dependency-forms) form
    (make-subform
     name
     :kind kind
     :depforms
     (loop :for dep :in dependency-forms
           :collect
           (multiple-value-bind (lifted-dependency-form kind)
               (segment-dependency-form category dep)
             (make-depform :original-form dep
                           :canonical-form lifted-dependency-form
                           :kind kind))))))

(defun parse-graph-definition (name category depends-on roots subforms)
  (let ((subform-db (u:dict #'eq)))
    (make-graphdef name
                   :category category
                   :depends-on depends-on
                   :roots roots
                   :subforms
                   (loop :for i :in subforms
                         :for subform = (parse-subform category i)
                         :do (setf (u:href subform-db (name subform)) subform)
                         :finally (return subform-db)))))

(defun add-cross-product-edges (clg source-list target-list)
  (u:map-product
   (lambda (v1 v2)
     (cl-graph:add-edge-between-vertexes clg v1 v2 :edge-type :directed))
   source-list
   target-list))

(defun annotate-splice (v &rest args)
  (if (is-syntax-form-p '(splice) v)
      `(,v ,@(copy-seq args))
      v))

(defun annotate-splices (val-list &rest args)
  (mapcar
   (lambda (x)
     (apply #'annotate-splice x args))
   val-list))

(defun absorb-depforms (clg gdef depforms)
  "Traverse the depforms and add them into the clg as edges while keeping
reference to the initial gdef associated with the depforms. Return three values:
the cl-graph, the roots as elements, the leaves as elements."
  (let (roots leaves)
    (loop :for depform :in depforms
          :for kind = (kind depform)
          :for canonical-form = (canonical-form depform)
          ;; check this when condition for validity.
          :when canonical-form
            :do (ecase kind
                  (:empty nil)
                  (:hyperedges
                   (pushnew (car (first canonical-form)) roots :test #'equalp)
                   (pushnew (cadar (last canonical-form)) leaves :test #'equalp)
                   (loop :for (from to) :in canonical-form
                         :do (add-cross-product-edges
                              clg
                              (annotate-splices from gdef)
                              (annotate-splices to gdef))))
                  (:vertex
                   ;; the :vertex for is not only the roots, but also the
                   ;; leaves.
                   (pushnew canonical-form roots :test #'equalp)
                   (pushnew canonical-form leaves :test #'equalp)
                   (dolist (vert canonical-form)
                     (cl-graph:add-vertex clg (annotate-splice vert gdef))))))
    (values clg
            (remove-duplicates (mapcan #'identity roots) :test #'equalp)
            (remove-duplicates (mapcan #'identity leaves) :test #'equalp))))

(defun analyze-graphdef-depends-on (graph)
  "Transmute the :depends-on form in each graphdef object in the GRAPH into real
graphdef references holding real references to the named subforms."
  (loop :for gdef :in (u:hash-values (graphdefs graph))
        :for whole-depends-on-form = (depends-on gdef)
        :do (setf (depends-on gdef) (u:dict #'eq))
            (loop :for (gdef-name subform-names) :in whole-depends-on-form
                  :for gdef-reference = (u:href (graphdefs graph) gdef-name)
                  :for analyzed-depends-on = (make-graphdef-depends-on
                                              gdef-name
                                              :graphdef gdef-reference
                                              :original-form
                                              (list gdef-name subform-names))
                  :do (assert gdef-reference)
                      ;; Now set up the subforms entry in the
                      ;; analyzed-depends-on object.
                      (if (eq subform-names :all)
                          ;; get all subform names in gdef
                          (u:do-hash (k v (subforms gdef-reference))
                            (setf (u:href (subforms analyzed-depends-on) k) v))
                          ;; find the listed subform-names (which if it is NIL,
                          ;; do nothing) in the gdef and assign them.
                          (dolist (subform-name subform-names)
                            (setf (u:href (subforms analyzed-depends-on)
                                          subform-name)
                                  (u:href (subforms gdef-reference)
                                          subform-name))))
                      ;; store the semantic analysis of the depends-on form,
                      ;; transmuted into its new graphdef-depends-on form, back
                      ;; into the initiating gdef.
                      (setf (u:href (depends-on gdef)
                                    (name analyzed-depends-on))
                            analyzed-depends-on))))

(defun lookup-splice (splice-form gdef)
  "Find the subform describing SPLICE-FORM in GDEF, or in any available
depends-on in that GDEF."
  (let ((splice-name (second splice-form)))
    ;; Check if the splice is natively in the current gdef.
    (u:when-let ((splice (u:href (subforms gdef) splice-name)))
      (return-from lookup-splice (values splice gdef)))
    ;; If it isn't, then check which depends-on in the current gdef
    (dolist (dep-inst (u:hash-values (depends-on gdef)))
      (u:when-found (subform (u:href (subforms dep-inst) splice-name))
        (return-from lookup-splice (values subform (graphdef dep-inst)))))
    ;; Otheriwse, you're out of luck. Prolly should put an error here.
    nil))

(defun analyze-graph (graph)
  ;; TODO: Huge bug/missing feature. This only knows how to analyze/construct
  ;; directed graphs. Maybe that is actually ok and we don't need undirected
  ;; graphs....in which case, dump the subgraph form in the dsl. First, resolve
  ;; all depends-on lines into meaningful structures with real references and
  ;; such. This helps us look up splices.
  (analyze-graphdef-depends-on graph)
  (let ((clg (cl-graph:make-graph
              'cl-graph:graph-container
              ;; I store complex elements and edges.
              :vertex-test #'equalp
              :edge-test #'equalp
              :default-edge-type :directed)))
    ;; We do an iterative algorithm where we continuously refine the graph we're
    ;; making by expanding slices in additional passes until there are no
    ;; splices left. First, add the initial depforms from the roots.
    (loop :for gdef :in (u:hash-values (graphdefs graph))
          :when (roots gdef)
            :do (dolist (root (roots gdef))
                  ;; For the initial seeding, we don't care about the
                  ;; roots/leaves of the initial root subforms.
                  (absorb-depforms
                   clg
                   ;; This is the gdef that these depforms came from...
                   gdef
                   ;; ...in which all splices must be looked up in, either
                   ;; directly or through a graphdef-depends-on object.
                   (depforms (u:href (subforms gdef) root)))))
    ;; Then, iterate the graph. Each iteration will substitute the current
    ;; splice forms for the actual graphs indicated by those splice names. This
    ;; may introduce more splices the next iteration will get. Stop when there
    ;; are no more splices to substitute. Don't convert to dolist, I need to
    ;; recompute the find-vertexes-if in each iteration.
    (loop :for splices = (cl-graph:find-vertexes-if
                          clg
                          (lambda (v)
                            (is-syntax-form-p
                             ;; TODO: This FIRST here implies a structure that
                             ;; not all vertexes may actually have. It forces
                             ;; canonicalize-dependency-form to always make the
                             ;; element a list, like (component-type foo) or
                             ;; (potential-package :bar)
                             '(splice)
                             (first (cl-graph:element v)))))
          :unless splices
            :return nil
          :do (dolist (splice splices)
                (let ((parents (cl-graph:parent-vertexes splice))
                      (children (cl-graph:child-vertexes splice)))
                  (destructuring-bind (splice-form gdef)
                      (cl-graph:element splice)
                    (multiple-value-bind (lookedup-splice lookedup-gdef)
                        (lookup-splice splice-form gdef)
                      ;; Now, absorb the splice into clg, get the roots and
                      ;; leaves, then fixup the edges.
                      (multiple-value-bind (clg splice-roots splice-leaves)
                          (absorb-depforms
                           clg lookedup-gdef (depforms lookedup-splice))
                        ;; delete the original parent edges.
                        (dolist (parent parents)
                          (cl-graph:delete-edge-between-vertexes
                           clg parent splice))
                        ;; add the new edges from the parents to the new-roots.
                        (add-cross-product-edges
                         clg
                         parents
                         (annotate-splices splice-roots lookedup-gdef))
                        ;; delete the original child edges.
                        (dolist (child children)
                          (cl-graph:delete-edge-between-vertexes
                           clg splice child))
                        ;; add the new edges from the new-leaves to the
                        ;; children.
                        (add-cross-product-edges
                         clg
                         (annotate-splices splice-leaves lookedup-gdef)
                         children)
                        ;; Then finally, delete the expanding splice vertex
                        (cl-graph:delete-vertex clg splice)))))))
    ;; finally store it in the analyhzed-graph.
    (setf (graph graph) clg)
    ;; and then generate any annotations we might need.
    (generate-graph-annotation (category graph) graph)))

(defmethod generate-graph-annotation ((category (eql 'component-dependency))
                                      graph)
  (let* ((clg (graph graph))
         (contains-cycles-p
           (cl-graph:find-vertex-if
            clg
            (lambda (vert) (cl-graph:in-cycle-p clg vert))))
         (annotation (make-graph-annotation
                      (category graph)
                      :unknown-type-id (u:make-gensym "UNKNOWN-TYPE-ID-"))))
    ;; compute/store toposort, can only do if no cycles.
    (unless contains-cycles-p
      (let ((tsort (mapcar #'cl-graph:element (cl-graph:topological-sort clg))))
        (setf (toposort graph) tsort)))
    ;; collect all referenced component-types in the graph.
    (cl-graph:iterate-vertexes
     clg
     (lambda (v)
       (let ((elem-v (cl-graph:element v)))
         (when (is-syntax-form-p '(component-type) elem-v)
           (setf (u:href (referenced-types annotation) (second elem-v)) t)))))
    (setf (annotation graph) annotation)))

(defmethod generate-graph-annotation ((category (eql 'component-package-order))
                                      graph)
  (let* ((clg (graph graph))
         (contains-cycles-p
           (cl-graph:find-vertex-if
            clg
            (lambda (vert) (cl-graph:in-cycle-p clg vert))))
         (annotation (make-graph-annotation (category graph)))
         (all-packages (sort (mapcar #'package-name (list-all-packages))
                             #'string<)))
    ;; compute/store toposort, can only do if no cycles.
    (unless contains-cycles-p
      (let ((tsort (mapcar #'cl-graph:element (cl-graph:topological-sort clg))))
        (setf (toposort graph) tsort)))
    ;; Then, for each vertex in the clg, annotate the actual packages that match
    ;; to it.
    (cl-graph:iterate-vertexes
     clg
     (lambda (v)
       (let* ((elem-v (cl-graph:element v))
              (second (second elem-v))
              (putative-package-name (symbol-name second))
              ;; This regex is mildly wrong and will match a extraneous stuff
              ;; because it isn't escaped properly, stuff like . + ? | whatever
              ;; in the package name will mess things up.
              (putative-package-name-regex
                (format nil "^~a$" putative-package-name)))
         ;; Kind of a terrible Big-O...
         (dolist (pkg-name all-packages)
           (u:when-let* ((matched-pkg-name (ppcre:scan-to-strings
                                            putative-package-name-regex
                                            pkg-name))
                         (found-pkg (find-package matched-pkg-name)))
             (pushnew found-pkg (u:href (pattern-matched-packages annotation)
                                        second)))))))
    (setf (annotation graph) annotation)))

(defun canonicalize-component-type (component-type core)
  "If the COMPONENT-TYPE is reference in the component-dependency graph, then
return it, otherwise return the unknown-type-id symbol."
  (let* ((putative-component-type component-type)
         (component-dependency-graph (u:href (analyzed-graphs core)
                                             'component-dependency))
         (annotation (annotation component-dependency-graph)))
    (assert annotation)
    (unless (u:href (referenced-types annotation) putative-component-type)
      ;; Nope, so we use the unknown type
      (setf putative-component-type (unknown-type-id annotation)))
    ;; The canonicalized component-type
    putative-component-type))

(defmacro define-graph (&whole form name (&key category depends-on roots)
                        &body body)
  (declare (ignore category depends-on roots body))
  `(setf (u:href =meta/graphs= ',name) ',(cdr form)))

(defun load-graphs (core)
  (with-slots (%analyzed-graphs) core
    (setf %analyzed-graphs (u:dict #'eq))
    (u:do-hash-values (graph-code =meta/graphs=)
      (destructuring-bind (name (&key category depends-on roots) . body)
          graph-code
        (symbol-macrolet ((graph (u:href %analyzed-graphs category)))
          (unless graph
            (setf graph (make-analyzed-graph :category category)))
          (setf (u:href (graphdefs graph) name)
                (parse-graph-definition name category depends-on roots body)))))
    (u:do-hash-values (graph %analyzed-graphs)
      (analyze-graph graph))))
