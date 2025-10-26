;;;; image.lisp --- Implementation of image

(in-package :mlx-cl.image)

(defclass image (mlx-array)
  ((colorspace :initarg :colorspace
               :reader  colorspace))
  (:documentation
   "The `mlx-array' of image data.

The `image' is a `mlx-array' of shape (H W CHANNELS).

Use:
+ `h': (h image) to get height of `image'
+ `w': (w image) to get width of `image'
+ `colorspace': (colorspace image) to get colorspace of `image'

Use `image' to create images. "))

(defgeneric image (object &key w h colorspace depth &allow-other-keys)
  (:documentation
   "Convert OBJECT into `image' object. ")
  (:method ((arr mlx-array) &key)
    (assert (cl:<= 2 (dim arr) 3))
    (let* ((w (shape arr :axis 1))
           (h (shape arr :axis 0))
           (d (dim arr))
           (c (if (cl:= 2 d) 1 (shape arr :axis 2))))
      (change-class (reshape arr (list h w c)) 'image))))

(defgeneric w (image)
  (:documentation "Return width of IMAGE. ")
  (:method ((image image))
    (shape image :axis 1)))

(defgeneric h (image)
  (:documentation "Return height of IMAGE. ")
  (:method ((image image))
    (shape image :axis 0)))

(defgeneric colorspace (image)
  (:documentation "Return colorspace of IMAGE. ")
  (:method ((image image))
    (if (slot-boundp image 'colorspace)
        (slot-value image 'colorspace)
        (setf (slot-value image 'colorspace)
              (ecase (shape image :axis 2)
                (1 :grayscale)
                (3 :rgb)
                (4 :rgba))))))

;;;; image.lisp ends here
