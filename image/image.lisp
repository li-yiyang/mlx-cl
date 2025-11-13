;;;; image.lisp --- Implementation of image

(in-package :mlx-cl.image)

(defclass image (mlx-array)
  (colorspace)
  (:documentation
   "The `mlx-array' of image data.

The `image' is a `mlx-array' of shape (H W CHANNELS).

Use:
+ `h': (h image) to get height of `image'
+ `w': (w image) to get width of `image'
+ `colorspace': (colorspace image) to get colorspace of `image'

Use `image' to create images. "))

(declaim (inline assert-mlx-array-is-image))
(defun assert-mlx-array-is-image (mlx-array)
  (let ((dim (dim mlx-array)))
    (unless (or (cl:= dim 3) (cl:= dim 2))
      (error
       "Image(dim=~D) should be mlx-array with shape (H W) or (H W C), but got:~%~A"
       dim mlx-array))))

(defgeneric image (object &key w h colorspace dtype &allow-other-keys)
  (:documentation
   "Convert OBJECT into `image' object. ")
  (:method (object &rest keys &key)
    (image (apply #'mlx-array object keys)))
  (:method ((arr mlx-array) &key (dtype :uint8 dtype?))
    (assert-mlx-array-is-image arr)
    (let* ((w (shape arr :axis 1))
           (h (shape arr :axis 0))
           (d (dim arr))
           (c (if (cl:= 2 d) 1 (shape arr :axis 2))))
      (->* (reshape arr (list h w c))
        (if dtype? (as-type * dtype) *)
        (change-class 'image))))
  (:method :around ((arr mlx-array) &rest keys &key (colorspace nil colorspace?))
    (let ((img (call-next-method)))
      (when colorspace?
        (apply #'(setf colorspace) colorspace img keys))
      img))
  (:method ((img image) &key)
    img))

(defmethod mlx-array ((image image) &key)
  "Convert `image' as `mlx-array'. "
  (copy image))

(defgeneric w (image)
  (:documentation "Return width of IMAGE. ")
  (:method ((array mlx-array))
    (assert-mlx-array-is-image array)
    (shape array :axis 1))
  (:method ((image image))
    (shape image :axis 1)))

(defgeneric h (image)
  (:documentation "Return height of IMAGE. ")
  (:method ((array mlx-array))
    (assert-mlx-array-is-image array)
    (shape array :axis 0))
  (:method ((image image))
    (shape image :axis 0)))

(defgeneric d (image)
  (:documentation "Return bit depth of IMAGE. ")
  (:method ((array mlx-array))
    (assert-mlx-array-is-image array)
    (d<-dtype (dtype array)))
  (:method ((image image))
    (d<-dtype (dtype image))))

(defgeneric channels (image)
  (:documentation "Return numbers of channels of IMAGE. ")
  (:method ((array mlx-array))
    (assert-mlx-array-is-image array)
    (ecase (dim array)
      (2 1)
      (3 (shape array :axis 2))))
  (:method ((image image))
    (shape image :axis 2)))

(defgeneric colorspace (image)
  (:documentation "Return colorspace of IMAGE.

Dev Note:
if colorspace is not binded with IMAGE, it would be guessed by
the image channels:
+ 1: `:grayscale'
+ 2: `:grayscale-alpha'
+ 3: `:rgb'
+ 4: `:rgba'")
  (:method ((array mlx-array))
    "Guess colorspace from `mlx-array'. "
    (assert-mlx-array-is-image array)
    (guess-colorspace (channels array)))
  (:method :around ((image image))
    (if (slot-boundp image 'colorspace)
        (slot-value image 'colorspace)
        (setf (slot-value image 'colorspace) (call-next-method)))))

(defgeneric (setf colorspace) (colorspace image &key &allow-other-keys)
  (:documentation
   "Change `colorspace' of IMAGE to COLORSPACE.

Dev Note:
Behind the scene, it's more like creating a new `image' and steal
it's `mlx::mlx-object-pointer' as IMAGE's own data. So the
implementation of colorspace conversion functions should alloc
new `image' and not let it goes outside the function.

See `mlx::%steal-mlx-array-pointer' for details. "))

(defmethod print-object ((image image) stream)
  "#<image(WxH COLORSPACE)>"
  (print-unreadable-object (image stream)
    (format stream "~S(~Dx~D ~S)"
            (class-name (class-of image))
            (w image) (h image)
            (colorspace image))))

;;;; image.lisp ends here
