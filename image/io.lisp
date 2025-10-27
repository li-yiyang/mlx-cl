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


;;; low-level

(defun mlx-dtype<-bit-depth (d)
  (ecase d
    ;; TODO: #mlx-cl.image #optimization
    ;; bits should use?
    (1  :uint8)
    (4  :uint8)
    (8  :uint8)
    (16 :uint16)))

(mlx::defmlx-extension (:tiff "TIF" "TIFF")
  ((:load stream)
   "Load TIFF image. "
   (let* ((img   (tiff:read-tiff-stream stream))
          (w     (tiff:tiff-image-width img))
          (h     (tiff:tiff-image-length img))
          (d     (tiff:tiff-image-bits-per-sample img))
          (c     (tiff:tiff-image-samples-per-pixel img))
          (dtype (mlx-dtype<-bit-depth (if (arrayp d) (aref d 0) d)))
          (dat   (mlx-array (tiff:tiff-image-data img) :dtype dtype)))
     (reshape dat (list h w c))))
  ((:save img stream)
   "Write TIFF image. "
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

(mlx::defmlx-extension (:png "PNG")
  ((:load stream)
   "Load PNG image. "
   (let* ((img   (pngload:load-stream stream))
          (w     (pngload:width  img))
          (h     (pngload:height img))
          (c     (ecase (pngload:color-type img)
                   (:greyscale        1)
                   (:greyscale-alpha  2)
                   (:truecolour       3)
                   (:truecolour-alpha 4)))
          (dtype (mlx-dtype<-bit-depth (pngload:bit-depth img)))
          (dat   (mlx-array (pngload:data img) :dtype dtype)))
     (reshape dat (list h w c))))
  ((:save img stream)
   "Write PNG image. "
   (let ((img (make-instance 'zpng:png
                             :image-data (lisp<- (flatten img))
                             :bpp (d img)
                             :color-type (ecase (colorspace img)
                                           (:grayscale       :grayscale)
                                           (:grayscale-alpha :grayscale-alpha)
                                           (:rgb             :truecolor)
                                           (:rgba            :truecolor-alpha))
                             :height (h img)
                             :width  (w img))))
     (zpng:write-png-stream img stream))))


;;;; io.lisp ends here
