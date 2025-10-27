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

(defmethod reinitialize-instance :after
    ((img image) &key colorspace
     &aux (change-colorspace
           (cl:and colorspace
                   (cl:not (eql (colorspace img) colorspace)))))
  (when change-colorspace
    (let ((dup (copy img)))
      (setf (colorspace dup) colorspace)
      (mlx::%steal-mlx-array-pointer dup img))))

(defgeneric image (object &key w h colorspace &allow-other-keys)
  (:documentation
   "Convert OBJECT into `image' object. ")
  (:method :around (arr &key (colorspace nil colorspace?))
    (let ((img (call-next-method)))
      (when colorspace?
        (setf (colorspace img) colorspace))
      img))
  (:method ((arr mlx-array) &rest args &key)
    (assert (cl:<= 2 (dim arr) 3))
    (let* ((w (shape arr :axis 1))
           (h (shape arr :axis 0))
           (d (dim arr))
           (c (if (cl:= 2 d) 1 (shape arr :axis 2))))
      (apply #'change-class (reshape arr (list h w c)) 'image args))))

(defgeneric w (image)
  (:documentation "Return width of IMAGE. ")
  (:method ((image image))
    (shape image :axis 1)))

(defgeneric h (image)
  (:documentation "Return height of IMAGE. ")
  (:method ((image image))
    (shape image :axis 0)))

(defgeneric d (image)
  (:documentation "Return bit depth of IMAGE. ")
  (:method ((image image))
    ;; TODO: #mlx-cl.image #bug?
    (ecase (dtype image)
      (:uint8  8)
      (:uint16 16))))

(defgeneric colorspace (image)
  (:documentation "Return colorspace of IMAGE. ")
  (:method ((image image))
    (if (slot-boundp image 'colorspace)
        (slot-value image 'colorspace)
        (setf (slot-value image 'colorspace)
              ;; guess the image
              (ecase (shape image :axis 2)
                (1 :grayscale)
                (2 :grayscale-alpha)
                (3 :rgb)
                (4 :rgba))))))

(defgeneric (setf colorspace) (colorspace image &key &allow-other-keys)
  (:documentation
   "Change `colorspace' of IMAGE to COLORSPACE.

Dev Note:
Behind the scene, it's more like creating a new `image' and steal
it's `mlx::mlx-object-pointer' as IMAGE's own data. So the
implementation of colorspace conversion functions should alloc
new `image' and not let it goes outside the function.

See `mlx::%steal-mlx-array-pointer' for details. "))

;;;; image.lisp ends here
