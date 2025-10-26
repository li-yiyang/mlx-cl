;;;; colorspace.lisp --- Implementation of colorspaces transformation

(in-package :mlx-cl.image)

(deftype colorspace ()
  "Currently supported image colorspace. "
  `(member :grayscale :rgb :rgba))

(deftype color-depth ()
  "Common color bit depth of images. "
  `(member 1 8))

;;;; colorspace.lisp ends here
