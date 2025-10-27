;;;; colorspace.lisp --- Implementation of colorspaces transformation

(in-package :mlx-cl.image)

(deftype colorspace ()
  "Currently supported image colorspace. "
  `(member :grayscale :grayscale-alpha :rgb :rgba))

(defun colorspace-channels (colorspace)
  "Return channels of COLORSPACE. "
  (declare (type colorspace colorspace))
  (the integer
    (ecase colorspace
      (:grayscale       1)
      (:grayscale-alpha 2)
      (:rgb             3)
      (:rgba            4))))

;; alloc const weight at compile time to accelerate CFFI copying
(let ((+ntsc+ (mlx-array #(0.2989 0.5870 0.1140) :dtype :float32))
      (+mean+ (mlx-array #(1.0    1.0    1.0)    :dtype :float32)))
  (defun grayscale<-rgb (image
                         &key (method :ntsc) (weight +ntsc+ weight?)
                         &allow-other-keys)
    (declare (type image image))
    (assert (eq (colorspace image) :rgb))
    (if weight?
        (image (as-type (/ (inner weight image)
                           (sum weight))
                        (dtype image))
               :colorspace :grayscale)
        (ecase method
          (:ntsc                 (grayscale<-rgb image :weight +ntsc+))
          ((:average :mean :avg) (grayscale<-rgb image :weight +mean+))))))

(defmethod (setf colorspace) :around (colorspace (image image) &rest keys)
  "If IMAGE is not assigned with COLORSPACE infomation,
just accept the COLORSPACE with simple tests. "
  (when (cl:or (slot-boundp image 'colorspace)
               (cl:/= (colorspace-channels colorspace)
                      (shape image :axis 2)))
    (unless (eql (colorspace image) colorspace)
      (let ((new (the image (apply #'call-next-method colorspace image keys))))
        (mlx::%steal-mlx-array-pointer new image))))
  (setf (slot-value image 'colorspace) colorspace))

(defmethod (setf colorspace) ((colorspace (eql :grayscale)) (image image) &rest keys)
  (ecase (colorspace image)
    (:rgb (apply #'grayscale<-rgb image keys))))

;;;; colorspace.lisp ends here
