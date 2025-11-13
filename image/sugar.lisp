;;;; sugar.lisp --- Syntax sugar for mlx-cl.image

(in-package :mlx-cl.image)


;;;; Creating a new image

(defmethod image ((color symbol)
                  &rest keys
                  &key
                    (w nil w?)
                    (h nil h?)
                    (colorspace *colorspace*)
                    (dtype :uint8)
                  &aux
                  (w! (cond (w? (truncate (mlx::num<- w)))
                            (h? (truncate (mlx::num<- h)))
                            (t  (error "Missing `image' size, set `:w' or `:h'. "))))
                  (h! (cond (h? (truncate (mlx::num<- h)))
                            (w? (truncate (mlx::num<- w)))
                            (t  (error "Missing `image' size, set `:h' or `:w'. "))))
                  (c! (color-value color colorspace)))
  "Create an `image' of COLOR. "
  (declare (type (integer 1) w! h!))
  (image
   (full (list w! h! (channels c!))
         (* (apply #'as-colorspace c! colorspace keys)
            (scale<-dtype dtype))
         :dtype dtype)
   :colorspace colorspace))

;;;; sugar.lisp ends here
