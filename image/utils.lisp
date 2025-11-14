;;;; utils.lisp --- utils functions for mlx-cl.image

(in-package :mlx-cl.image)

(defun scale<-dtype (dtype)
  "Convert DTYPE as scaling factor.
Return a float as scaling factor. "
  (declare (type mlx-dtype dtype))
  (the float
    (ecase dtype
      (:uint8   255.0f0)
      (:uint16  65535.0f0)
      (:float32 1.0))))

(defun d<-dtype (dtype)
  "Convert DTYPE as bit depth of IMAGE.
Return an integer as bit depth. "
  (declare (type mlx-dtype dtype))
  (the integer
    (cl:* 8 (mlx::mlx-dtype-size dtype))))

(defun guess-colorspace (channels)
  "Guess colorspace from CHANNELS. "
  (declare (type (integer 1) channels))
  (ecase channels
    (1 :grayscale)
    (2 :grayscale-alpha)
    (3 :rgb)
    (4 :rgba)))

;;;; utils.lisp ends here
