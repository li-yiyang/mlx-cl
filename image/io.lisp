;;;; io.lisp ---- Implementation of image reading and writing

(in-package :mlx-cl.image)


;;; high-level

(defmethod image ((filespec pathname)
                  &key (format (mlx::pathname-format filespec)))
  (change-class (load-from filespec format) 'image))

(defmethod image ((filespec string) &rest keys &key)
  (apply #'image (pathname filespec) keys))

(defmethod save ((img image) &rest args &key output format)
  (unless output
    (error "Missing :output"))
  (apply #'save-to
         img
         (pathname output)
         (cl:or format (mlx::pathname-format output))
         args)
  output)

;;;; io.lisp ends here
