;;;; mlx-cl.asd --- CL bindings for MLX

;;; MLX-CL

(defsystem #:mlx-cl
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"                      ; NEXT: 0.0.1 finish API test
  :description "CL bindings for MLX"
  :depends-on (:mlx-cl/lib :cffi :cffi-libffi :trivial-garbage
               :str :numpy-file-format)
  :pathname "lisp"
  :components
  ((:file "package")
   (:file "utils"   :depends-on ("package"))
   (:file "string"  :depends-on ("utils"))
   (:file "device"  :depends-on ("string"))
   (:file "stream"  :depends-on ("device"))
   (:file "array"   :depends-on ("utils"))
   (:file "vector"  :depends-on ("array"))
   (:file "wrap"    :depends-on ("utils" "array"))
   (:file "ops"     :depends-on ("wrap" "vector"))
   (:file "slice"   :depends-on ("wrap"))
   (:file "axis"    :depends-on ("wrap"))
   (:file "version" :depends-on ("string"))
   (:file "metal"   :depends-on ("utils")) ; NEXT: metal trace for debug
   (:file "io"      :depends-on ("array"))
   (:file "sugar"   :depends-on ("array")) ; NEXT: math input #$ x * 2 => (* x 2)

   ;; (:file "fft"     :depends-on ("array"))
   (:file "random"  :depends-on ("array"))
   ;; (:file "linalg"  :depends-on ("array"))
   )
  :in-order-to ((test-op (test-op :mlx-cl/test))))

(defsystem #:mlx-cl/test
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"                      ; NEXT: 0.0.1 finish API test
  :description "Test for MLX-CL"
  :depends-on (:mlx-cl :fiveam)
  :pathname "test"
  :components
  ((:file "package")
   (:file "device" :depends-on ("package"))
   (:file "api"    :depends-on ("package")))
  :perform (test-op (op c)
             (symbol-call :mlx-cl.test :run-tests)))

;;; MLX-CL/Lib

(defsystem #:mlx-cl/lib
  :author ("凉凉")
  :license "MIT"
  :version "0.0.0"
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
  :version "0.0.0"
  :description "For developping `mlx-cl', here's some dev tools. "
  :depends-on (:trivial-indent
               :mlx-cl
               :mlx-cl/test)
  :pathname "dev"
  :components
  ((:file "package")
   (:file "indent")))


;;;; Submodules

(defsystem #:mlx-cl/image
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"                      ; NEXT: 0.0.1 colorspace, io
  :description "Use mlx-cl for image processing. "
  :depends-on (:mlx-cl
               ;; io
               :retrospectiff
               :pngload
               :zpng)
  :pathname "image"
  :components
  ((:file "package")
   (:file "image"      :depends-on ("package"))
   (:file "colorspace" :depends-on ("image"))
   (:file "io"         :depends-on ("image"))))


;;;; mlx-cl.asd ends here
