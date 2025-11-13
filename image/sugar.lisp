;;;; sugar.lisp --- Syntax sugar for mlx-cl.image

(in-package :mlx-cl.image)


;;;; Creating a new image

(defmethod image ((color symbol)
                  &rest keys
                  &key
                    (w nil w?)
                    (h nil h?)
                    (colorspace *colorspace*)
                    (dtype      :uint8)
                  &aux
                  (w! (cond (w? (truncate (mlx::num<- w)))
                            (h? (truncate (mlx::num<- h)))
                            (t  (error "Missing `image' size, set `:w' or `:h'. "))))
                  (h! (cond (h? (truncate (mlx::num<- h)))
                            (w? (truncate (mlx::num<- w)))
                            (t  (error "Missing `image' size, set `:h' or `:w'. ")))))
  "Create an `image' of COLOR. "
  (declare (type (integer 1) w! h!)
           (type keyword color))
  (let* ((*colorspace* colorspace)
         (color        (apply #'as-colorspace
                              (get-color color)
                              colorspace
                              keys))
         (image        (full (list w! h! (channels color))
                             (* color (scale<-dtype dtype))
                             :dtype dtype)))
    (change-class image 'image)
    (setf (slot-value image 'colorspace) colorspace)
    image))

(defmethod (setf at) :around ((color symbol) (image image) &rest slices)
  (declare (type keyword color))
  (apply #'call-next-method
         (as-type (* (color-value color (colorspace image))
                     (scale<-dtype (dtype image)))
                  (dtype image))
         image
         slices))

(defun colorspace= (image colorspace &rest more-colorspaces)
  "Test if IMAGE is of colorspace. "
  (member (colorspace image)
          (apply #'colorspaces (cons colorspace more-colorspaces))))

;;;; sugar.lisp ends here
