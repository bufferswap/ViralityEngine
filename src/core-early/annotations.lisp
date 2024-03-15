(in-package #:virality)

(defmacro define-annotation (name &key
                                    (getter
                                     '(lambda (value component)
                                       (declare (ignore component)
                                        value)))
                                    (setter
                                     '(lambda (value component)
                                       (declare (ignore component)
                                        value))))
  `(register-annotation 'component ',name :initialized
                        :getter (function ,getter)
                        :setter (function ,setter)))
