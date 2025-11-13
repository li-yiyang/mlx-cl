;;;; color-internal.lisp --- Internal rule interpreter of color

(in-package :mlx-cl.image)

(declaim (type (satisfies colorspacep) *colorspace*))
(defparameter *colorspace* :rgb
  "Internal colorspace used in `mlx-cl.image'. ")

(defparameter *colors* (make-hash-table :test 'eql)
  "Tables of colors.
Key: name of color
Val: `color'. ")

(defmethod colorspace ((color symbol))
  "Return colorspace of COLOR. "
  (declare (type keyword color))
  (colorspace (the color (or (gethash color *colors*)
                             (error "Unknown color `~S'. " color)))))

(defclass color (image) ()
  (:documentation
   "The `color' is a (1 1 C) `image' of colorspace `*colorspace*'.
The dtype of `color' `mlx-array' should be `:float32' [0, 1].

To create a `color' object from `mlx-array', use `color'. "))

(defun get-color (color)
  "Get `color' by name COLOR.
Return the `color' object or raise error if no color is not found. "
  (declare (type keyword color))
  (the color
    (or (gethash color *colors*)
        (error "Unknown color `~S'. " color))))

(defun color-value (color &optional (colorspace (colorspace color) colorspace?))
  "Convert COLOR as IMAGE compatiable mlx-array for value.
Return a `mlx-array' of COLOR under COLORSPACE. "
  (declare (type (or keyword color mlx-array) color)
           (type keyword colorspace))
  (let ((color (etypecase color
                 (keyword   (get-color color))
                 (color     color)
                 (mlx-array (color color :colorspace colorspace)))))
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

(defun ensure-mlx-array-is-color (mlx-array channels)
  "Ensure IMAGE is `color' like shape (1 1 C).
Return the `mlx-array' of shape (1 1 C) or raise error. "
  (declare (type mlx-array mlx-array))
  (case (dim mlx-array)
    (0
     (unless (equal 1 channels)
       (error "Expecting colorspace having 1 channels, but got: ~D" channels))
     (reshape mlx-array (list 1 1 1)))
    (1
     (unless (equal channels (shape mlx-array :axis 0))
       (error "Expecting a `~S' of shape (~D), but got: ~A. "
              (class-name (class-of mlx-array))
              channels
              (shape mlx-array)))
     (reshape mlx-array (list 1 1 (shape mlx-array :axis 0))))
    (3
     (unless (equal (list 1 1 channels) (shape mlx-array))
       (error "Expecting a `~S' of shape (1 1 ~D), but got: ~A. "
              (class-name (class-of mlx-array))
              channels
              (shape mlx-array)))
     mlx-array)
    (otherwise
     (error "Expecting a `~S' of dim 1 or 3, but got: ~A. "
            (class-name (class-of mlx-array))
            (dim mlx-array)))))

;; TODO: #mlx.img #cleanup
;; the code is hard to read
(defgeneric color (object &key colorspace &allow-other-keys)
  (:documentation
   "Convert OBJECT as `color'.
Return `color'.

Parameters:
+ OBJECT: should be able to convert into shape (1 1 C)
+ COLORSPACE: if given, will try to change OBJECT into COLORSPACE

  see `as-colorspace' parameters and (documentation COLORSPACE 'colorspace)
  documentations for other parameters if specifing the COLORSPACE
  parameters for converting the color colorspaces

Example:

    + (color :white :colorspace

See class `color' documentation for details.
")
  (:method (object &rest keys &key)
    "By default, the OBJECT is convert to `mlx-array' and then convert to `color'. "
    (apply #'color (apply #'mlx-array object keys) keys))
  (:method ((color symbol) &rest keys &key (colorspace *colorspace*))
    "To create a `color' from color name COLOR. "
    (declare (type keyword color))
    (apply #'as-colorspace
           (or (gethash color *colors*)
               (error "Unknown color `~S'. " color))
           colorspace
           keys))
  (:method ((color mlx-array) &rest keys &key (colorspace *colorspace*))
    "To convert from `mlx-array' to `color'.

Dev Note:
see `mlx.img::ensure-mlx-array-is-color'. "
    (let ((color (ensure-mlx-array-is-color color (colorspace-channels colorspace))))
      ;; check mlx-array is color
      (change-class
       (/ (apply #'as-colorspace color colorspace keys)
          (scale<-dtype (dtype color)))
       'color)))
  (:method ((image image) &rest keys &key (colorspace (colorspace image)))
    (assert-image-is-color image)
    (change-class
     (if (eql (colorspace      image)
              (colorspace-name colorspace))
         (/ image (scale<-d (dtype image)))
         (/ (apply #'as-colorspace image colorspace keys)
            (scale<-dtype (dtype image))))
     'color)))

(defmethod mlx-array ((color color) &key)
  "Convert COLOR back as `mlx-array'. "
  (copy color))

(defmethod print-object ((color color) stream)
  "Print #<COLOR(COLORSPACE VALUES)> or
#.(color VALUES :dtype :float32 :colorspace COLORSPACE)
if `*print-readably*'. "
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

;; Dev Note:
;; the `def*' should be defined as a macro, just to make it look nice...
(defmacro define-color (name color)
  "Define a COLOR with NAME.
Return NAME.

Parameters:
+ NAME: a keyword as color name
+ COLOR: should be a `color' object or a keyword of colorname"
  `(%define-color ,name ,color))

;;;; color-internal.lisp ends here
