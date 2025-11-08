;;;; mlx-cl.asd --- CL bindings for MLX

;;; MLX-CL

(defsystem #:mlx-cl
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"                      ; NEXT: 0.0.1 finish API test
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
   (:file "wrap"    :depends-on ("utils" "array"))
   (:file "ops"     :depends-on ("wrap" "vector"))
   (:file "slice"   :depends-on ("wrap"))
   (:file "axis"    :depends-on ("wrap"))
   (:file "version" :depends-on ("string"))
   (:file "metal"   :depends-on ("utils")) ; NEXT: metal trace for debug
   (:file "io"      :depends-on ("array"))
   (:file "sugar"   :depends-on ("array")) ; NEXT: math input #$ x * 2 => (* x 2)

   (:file "fft"     :depends-on ("array"))
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
  :version "0.0.0"
  :description "Use mlx-cl for image processing. "
  :depends-on (:mlx-cl
               :mlx-cl/image/minimal
               :mlx-cl/io/tiff
               :mlx-cl/io/png
               :mlx-cl/io/jpeg))

(defsystem #:mlx-cl/image/minimal
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"                      ; NEXT: 0.0.1 colorspace, io
  :description "Minimal package for `mlx-cl.image'. "
  :long-description
  "This is the core functional part of mlx-cl/image.
The main image processing algorithms should be implemented here. "
  :depends-on (:mlx-cl)
  :pathname "image"
  :components
  ((:file "package")
   (:file "image"      :depends-on ("package"))
   (:file "colorspace" :depends-on ("image"))
   (:file "io"         :depends-on ("image"))))


;;;; IO Submodules

;; TODO: #future
;; use libTIFF, libPNG, libJPEG bindings to replace existing
;; CL library (pass decoded pointer directly maybe reduce data
;; copying between FFI and CL and CL back to FFI).
;; the design is like this:
;;
;; 1. MMAP support as base
;;
;; (defsystem #:mlx-cl/io/mmap
;;   :author ("凉凉")
;;   :license "GPL"
;;   :version "0.0.0"
;;   :description "Support mmap buffer <-> mlx-array. "
;;   :pathname "io/mmap"
;;   :depends-on (:mlx-cl :cffi)
;;   :serial t
;;   :components
;;   ((:file "package")
;;    (:file "io")))
;;
;; 2. wrap libXXX with a C function for dumping data into
;; mmap buffer, return pointer to mmap region, then calling
;; to mmap->mlx-array would make things done easily.
;;
;; 3. another way would be mannually generate the C bindings
;; for C libraries. maybe not easy.
;;

(defsystem #:mlx-cl/io/npy
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"
  :description "Support TIFF format IO for mlx-array numpy-file-format. "
  :pathname "io/npy"
  :depends-on (:mlx-cl :numpy-file-format)
  :serial t
  :components
  ((:file "package")
   (:file "io")))

(defsystem #:mlx-cl/io/tiff
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"
  :description "Support TIFF format IO for mlx-cl/image via libTIFF. "
  :pathname "io/tiff"
  :depends-on (:mlx-cl :retrospectiff)
  :serial t
  :components
  ((:file "package")
   (:file "io")))

(defsystem #:mlx-cl/io/png
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"
  :description "Support TIFF format IO for mlx-cl/image. "
  :pathname "io/png"
  :depends-on (:mlx-cl :pngload :zpng)
  :serial t
  :components
  ((:file "package")
   (:file "io")))

(defsystem #:mlx-cl/io/jpeg
  :author ("凉凉")
  :license "GPL"
  :version "0.0.0"
  :description "Support JPEG format IO for mlx-cl/image. "
  :pathname "io/jpeg"
  :depends-on (:mlx-cl :cl-jpeg)
  :serial t
  :components
  ((:file "package")
   (:file "io")))

;;;; mlx-cl.asd ends here
