;;;; mlx-cl.asd --- CL bindings for MLX

;;; MLX-CL

(defsystem #:mlx-cl
  :author ("凉凉")
  :license "GPL"
  :version "0"
  :description "CL bindings for MLX"
  :depends-on (:mlx-cl/lib :cffi :cffi-libffi :trivial-garbage)
  :pathname "lisp"
  :components
  ((:file "package")
   (:file "utils"   :depends-on ("package"))
   (:file "string"  :depends-on ("utils"))
   (:file "device"  :depends-on ("string"))
   (:file "stream"  :depends-on ("device"))
   (:file "array"   :depends-on ("utils"))
   (:file "vector"  :depends-on ("array"))
   (:file "ops"     :depends-on ("array" "vector"))
   (:file "version" :depends-on ("string"))
   (:file "metal"   :depends-on ("utils"))
   ))

;;; MLX-CL/Lib

(defsystem #:mlx-cl/lib
  :author ("凉凉")
  :license "MIT"
  :version "0"
  :description "Compiling libmlxc.dylib. "
  :serial t
  :depends-on (:cffi)
  :components
  ((:module :mlx-c
    :pathname "mlx-c")
   (:module :lib
    :components ((:file "package")
                 (:file "build")))))

(defsystem #:mlx-cl/dev
  :author ("凉凉")
  :license "GPL"
  :version "0"
  :description "For developping `mlx-cl', here's some dev tools. "
  :depends-on (:trivial-indent :mlx-cl)
  :pathname "dev"
  :components
  ((:file "package")
   (:file "indent")))


;; TODO: tests mlx-cl/test

;;;; mlx-cl.asd ends here
