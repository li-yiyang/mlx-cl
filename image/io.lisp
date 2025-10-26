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
         args))


;;; low-level

(mlx::defmlx-extension (:tiff "TIF" "TIFF")
  ((:load stream)
   (let* ((img   (tiff:read-tiff-stream stream))
          (w     (tiff:tiff-image-width img))
          (h     (tiff:tiff-image-length img))
          (d     (tiff:tiff-image-bits-per-sample img))
          (c     (tiff:tiff-image-samples-per-pixel img))
          (dtype (ecase d
                   ;; TODO: #mlx-cl.image #optimization
                   ;; bits should use?
                   (1  :uint8)
                   (4  :uint8)
                   (8  :uint8)
                   (16 :uint16)))
          (dat   (mlx-array (tiff:tiff-image-data img) :dtype dtype)))
     (reshape dat (list h w c))))
  ((:save img stream)
   (let ((img (make-instance 'tiff:tiff-image
                             :width  (w img)
                             :length (h img)
                             :bits-per-sample (ecase (dtype img)
                                                (:uint8  8)
                                                (:uint16 16)
                                                (:uint32 32))
                             :samples-per-pixel (shape img :axis 2)
                             :data (lisp<- (reshape img '(-1))))))
     (tiff:write-tiff-stream stream img))))

;;;; io.lisp ends here
