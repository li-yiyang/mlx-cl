;;;; io.lisp --- PNG I/O for mlx-array

(in-package :mlx-cl.io.png)

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
          (dtype (mlx::mlx-dtype<-bits (pngload:bit-depth img)))
          (dat   (mlx:mlx-array (pngload:data img) :dtype dtype)))
     (mlx:reshape dat (list h w c))))
  ((:save img stream)
   "Write PNG image. "
   (assert (or (mlx:dim= img 2)
               (mlx:dim= img 3)))
   (let ((img (make-instance 'zpng:png
                             :image-data (mlx:lisp<- (mlx:flatten img))
                             :bpp (ecase (mlx:dtype img)
                                    (:uint8 8))
                             :color-type (ecase (if (mlx:dim= img 2) 1
                                                    (mlx:shape img :axis 2))
                                           (1       :grayscale)
                                           (2       :grayscale-alpha)
                                           (3       :truecolor)
                                           (4       :truecolor-alpha))
                             :height (mlx:shape img :axis 0)
                             :width  (mlx:shape img :axis 1))))
     (zpng:write-png-stream img stream))))

;;;; io.lisp ends here
