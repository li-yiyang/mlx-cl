;;;; color-internal.lisp --- Internal rule interpreter of color

(in-package :mlx-cl.image)

(declaim (type (satisfies colorspacep) *colorspace*))
(defparameter *colorspace* :rgb
  "Internal colorspace used in `mlx-cl.image'. ")

(defparameter *colors* (make-hash-table :test 'eql)
  "Tables of colors.
Key: name of color
Val: `color'. ")

(defclass color (image) ()
  (:documentation
   "The `color' is a (1 1 C) `image' of colorspace `*colorspace*'.
The dtype of `color' `mlx-array' should be `:float32' [0, 1].

To create a `color' object from `mlx-array', use `color'. "))

(defun scale<-d (image)
  "Return IMAGE depth scale factor. "
  (ecase (dtype image)
    (:uint8   255.0)
    (:uint16  65535.0)
    (:float32 1.0)))

(defun color-value (color &optional (colorspace (colorspace color) colorspace?))
  "Convert COLOR as IMAGE compatiable mlx-array for value. "
  (declare (type (or keyword color) color)
           (type keyword colorspace))
  (let ((color (etypecase color
                 (keyword (or (gethash color *colors*)
                              (error "Unknown color `~S'. " color)))
                 (color   color))))
    (the mlx-array
      (if colorspace?
          (as-colorspace color colorspace)
          color))))

(defun assert-image-is-color (image)
  "Test if IMAGE is `color' like shape (1 1 C). "
  (unless (and (typep image 'image)
               (equal (shape image :axis 0) 1)
               (equal (shape image :axis 1) 1))
    (error "Expecting a `~S' of shape (1 1 C), but got: ~A. "
           (class-name (class-of image))
           (shape image))))

;; TODO: #mlx.img #cleanup
;; the code is hard to read
(defgeneric color (object &key colorspace &allow-other-keys)
  (:documentation
   "Convert OBJECT as `color'.
Return `color'.

Parameters:
+ OBJECT: should be able to convert into shape (1 1 C)
+ COLORSPACE: if given, will try to change OBJECT into COLORSPACE

See class `color' documentation for details.
")
  (:method (object &rest keys &key)
    (apply #'color (apply #'mlx-array object keys) keys))
  (:method ((color mlx-array) &rest keys &key (colorspace *colorspace*))
    (let ((color color))
      ;; check mlx-array is color
      (case (dim color)
        (0
         (unless (equal 1 (colorspace-channels colorspace))
           (error "Expecting colorspace having 1 channels, but got: ~D"
                  (colorspace-channels colorspace)))
         (setf color (reshape color (list 1 1 1))))
        (1
         (unless (equal (colorspace-channels colorspace)
                        (shape color :axis 0))
           (error "Expecting a `~S' of shape (~D), but got: ~A. "
                  (class-name (class-of color))
                  (colorspace-channels colorspace)
                  (shape color)))
         (setf color (reshape color (list 1 1 (shape color :axis 0)))))
        (3
         (unless (equal (list 1 1 (colorspace-channels colorspace))
                        (shape color))
           (error "Expecting a `~S' of shape (1 1 ~D), but got: ~A. "
                  (class-name (class-of color))
                  (colorspace-channels colorspace)
                  (shape color))))
        (otherwise
         (error "Expecting a `~S' of dim 1 or 3, but got: ~A. "
                (class-name (class-of color))
                (dim color))))
      (change-class (apply #'as-colorspace (/ color (scale<-d color)) colorspace keys) 'color)))
  (:method ((image image) &rest keys &key (colorspace (colorspace image)))
    (assert-image-is-color image)
    (change-class (if (eql (colorspace      image)
                           (colorspace-name colorspace))
                      image
                      (apply #'as-colorspace image colorspace keys))
                  'color)))

(defmethod mlx-array ((color color) &key)
  (copy color))

(defmethod print-object ((color color) stream)
  (if *print-readably*
      (format stream "#.(color ~A :dtype :float32 :colorspace ~S)"
              (lisp<-     (squeeze color))
              (colorspace color))
      (print-unreadable-object (color stream)
        (format stream "~S(~S ~A)"
                (class-name (class-of color))
                (colorspace color)
                (lisp<-     (squeeze color))))))


;;; Wrapper

(defun %define-color (name color)
  "See `define-color'. "
  (declare (type keyword name)
           (type (or keyword color) color))
  (let ((color (etypecase color
                 (keyword (or (gethash name *colors*)
                              (error "Unknown color `~S'. " name)))
                 (color   color))))
    (setf (gethash name *colors*) color)
    name))

(defmacro define-color (name color)
  "Define a COLOR with NAME.
Return NAME.

Parameters:
+ NAME: a keyword as color name
+ COLOR: should be a `color' object or a keyword of colorname"
  `(%define-color ,name ,color))

;;;; color-internal.lisp ends here
