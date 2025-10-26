;;;; io.lisp ---- File input and output

(in-package :mlx-cl)


;;; utils

(defparameter *mlx-supported-extensions*
  (make-hash-table :test 'cl:equal)
  "Hashtable whose key is extension string (upcase).
The value is format keyword. ")

(defmacro defmlx-extension ((format &rest extensions)
                            &rest options)
  "Define the implementation of mlx FORMAT.
Add EXTENSIONS to `*mlx-supported-extensions*' as FORMAT.

Syntax:

    (defmlx-extension (format . extensions)
      ((:load-path path :lambda-list ... :element-type ...)
        load-doc
        load-code)
      ((:load stream :lambda-list ... :element-type ...)
        load-doc
        load-code)
      ((:save-path arr path :lambda-list ... :element-type ...)
        save-doc
        save-code)
      ((:save arr stream :lambda-list ... :element-type ...)
        save-doc
        save-code))

"
  (assert (keywordp format))
  `(progn
     (loop :for ext :in ',extensions
           :do (assert (stringp ext))
           :do (setf (gethash (string-upcase ext) *mlx-supported-extensions*) ,format))
     ,@(when (assoc :load-path options :key #'car)
         (destructuring-bind ((load path
                               &key
                                 (lambda-list  '(&key)))
                              &body body)
             (assoc :load-path options :key #'car)
           (declare (ignore load))
           (multiple-value-bind (doc plist body)
               (split-doc-body body)
             `((defmethod load-from ((,path pathname) (format (eql ,format))
                                     ,@lambda-list)
                 ,(when doc
                    (apply #'gen-doc doc plist))
                 ,@body)))))
     ,@(when (assoc :load options :key #'car)
         (destructuring-bind ((load stream
                               &key
                                 (element-type '(unsigned-byte 8))
                                 (lambda-list  '(&key)))
                              &body body)
             (assoc :load options :key #'car)
           (declare (ignore load))
           (multiple-value-bind (doc plist body)
               (split-doc-body body)
             `((defmethod load-from ((,stream stream) (format (eql ,format))
                                     ,@lambda-list)
                 ,(when doc
                    (apply #'gen-doc doc plist))
                 ,@body)
               (defmethod load-from ((path pathname) (format (eql ,format))
                                     &rest keys)
                 (with-open-file (stream path :direction :input
                                              :element-type ',element-type)
                   (apply #'load-from stream format keys)))))))
     ,@(when (assoc :save-path options :key #'car)
         (destructuring-bind ((save arr path
                               &key
                                 (lambda-list  '(&key)))
                              &body body)
             (assoc :save-path options :key #'car)
           (declare (ignore save))
           (multiple-value-bind (doc plist body)
               (split-doc-body body)
             `((defmethod save-to ((,arr mlx-array)
                                   (,path pathname)
                                   (format (eql ,format))
                                   ,@lambda-list)
                 ,(when doc (apply #'gen-doc doc plist))
                 ,@body)))))
     ,@(when (assoc :save options :key #'car)
         (destructuring-bind ((save arr stream
                               &key
                                 (element-type '(unsigned-byte 8))
                                 (lambda-list  '(&key)))
                              &body body)
             (assoc :save options :key #'car)
           (declare (ignore save))
           (multiple-value-bind (doc plist body)
               (split-doc-body body)
             `((defmethod save-to ((,arr mlx-array)
                                   (,stream stream)
                                   (format (eql ,format))
                                   ,@lambda-list)
                 ,(when doc (apply #'gen-doc doc plist))
                 ,@body)
               (defmethod save-to ((arr mlx-array)
                                   (path pathname)
                                   (format (eql ,format))
                                   &rest keys
                                   &key
                                     (if-exists :supersede)
                                     (if-does-not-exists :create))
                 (with-open-file (stream path
                                         :element-type      ',element-type
                                         :direction         :output
                                         :if-exists         if-exists
                                         :if-does-not-exist if-does-not-exists)
                   (apply #'save-to arr stream format keys)))))))
     ,format))

(defun pathname-format (pathname)
  "Return format keyword or raise error for unknown format. "
  (declare (type (cl:or string pathname) pathname))
  (cl:or (gethash (string-upcase (pathname-type pathname))
                  *mlx-supported-extensions*)
         (error "Unknown extensino format ~A. " pathname)))


;;; High-level
(defgeneric load-from (source format &key &allow-other-keys)
  (:documentation
   "Load SOURCE of FORMAT. ")
  (:method ((source string) format &rest args)
    (apply #'load-from (pathname source) format args)))

(defgeneric save-to (object output format
                     &key if-exists if-does-not-exists
                     &allow-other-keys)
  (:documentation
   "Save OBJECT to OUTPUT of FORMAT. ")
  (:method (object (output string) format &rest args)
    (apply #'save-to object (pathname output) format args)))

(defgeneric save (object
                  &key output format if-exists if-does-not-exists
                  &allow-other-keys)
  (:documentation
   "Save OBJECT to OUTPUT of FORMAT. ")
  (:method ((arr mlx-array)
            &rest args
            &key output format
            &aux
              (out (if output (pathname output)
                       (error "Missing `:output'. ")))
              (fmt (cl:or format (pathname-format out))))
    (apply #'save-to arr out fmt args)))

(defmethod mlx-array ((filespec pathname) &key)
  "Load `mlx-array' from FILESPEC. "
  (load-from filespec (pathname-format filespec)))


;;; Low-level bindings

;; TODO: #mlx-cl #optimization
;; make `load-from' and `save-to' faster using MLX CFFI bindings
;;
(defmlx-extension (:npy "NPY")
  ((:load-path path)
   "Load from NPY file. "
   :parameters ((stream "NPY stream to load from"))
   (mlx-array (numpy-file-format:load-array path)))
  ((:save-path arr path)
   "Write as NPY file. "
   :parameters ((arr    "`mlx-array' to write")
                (stream "NPY stream to save to"))
   (numpy-file-format:store-array (lisp<- arr) path)))

;;;; io.lisp ends here
