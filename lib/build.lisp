;;;; build.lisp --- Build libmlxc.dylib

(in-package :mlx-cl.lib)

;;;; DEV Note:
;; Sorry, i don't know how to pack the libmlx.dylib, libmlxc.dylib.
;; If only these two libraries are copied from mlx-c/build, would
;; raise error for not loading metalib.
;;
;; not sure how to fix them, so I choose to just keep the mlx-c
;; source as it be, and load the libraries under the mlx-c/build.
;;
;; this is not so neat, but it works.
;;
;; i will consider fixing it after i finished rest of the `mlx-cl'.
;;

(defparameter *debug-output* nil)

(defparameter *libmlxc-src-dir*
  (asdf:component-pathname (asdf:find-component :mlx-cl/lib :mlx-c))
  "Directory of ml-explore/mlx-c source dir.

Change it to a cloned local repo if you'd like.
REPO: https://github.com/ml-explore/mlx-c.git")

(defparameter *libmlxc-build-dir*
  "build"
  "Directory to build libmlxc.dylib. ")

(defparameter *libmlxc-flags*
  '("-DMLX_BUILD_TESTS=OFF"
    "-DMLX_BUILD_EXAMPLES=OFF"
    "-DMLX_BUILD_BENCHMARKS=OFF"
    "-DMLX_BUILD_METAL=ON"
    "-DMLX_BUILD_CPU=ON"
    "-DMLX_BUILD_PYTHON_BINDINGS=OFF"
    "-DMLX_METAL_DEBUG=ON"
    "-DMLX_BUILD_SAFETENSORS=OFF"
    "-DMLX_BUILD_GGUF=OFF"
    "-DMLX_METAL_JIT=ON"
    "-DCMAKE_BUILD_TYPE=MinSizeRel"
    "-DBUILD_SHARED_LIBS=ON")
  "CMake Flags used to build libmlxc.dylib.
This should be a list of -D...=... flags toggling options ON/OFF. ")

(defun libmlxc-builded-path ()
  "Return path to builded libmlxc.dylib. "
  (merge-pathnames (format nil "~A/libmlxc.dylib" *libmlxc-build-dir*)
                   *libmlxc-src-dir*))

(defun libmlxc-builded-p ()
  "Test if libmlxc.dylib is built before. "
  (uiop:file-exists-p (libmlxc-builded-path)))

(defun exec (command &key (output *debug-output*))
  (when output (format output "exec >> ~{~A~^ ~}~%" command))
  (uiop:run-program command :output output))

(defun build-libmlxc ()
  "Build libmlxc.dylib in mlx-c source dir under `build' directory. "
  (uiop:with-current-directory (*libmlxc-src-dir*)
    (warn "Building libmlxc.dylib and this might take few minutes... ")

    ;; reconfigure libmlxc
    (exec `("cmake" "-B" ,*libmlxc-build-dir* ,@*libmlxc-flags*))

    ;; try to build
    (exec `("cmake" "--build" ,*libmlxc-build-dir*))

    (exec '("cmake" "--clean"))

    (unless (libmlxc-builded-p)
      (error "Cannot find libmlxc.dylib at ~A, is building sucessful? "
             (libmlxc-builded-path)))

    t))

(defun install-libmlxc ()
  (unless (libmlxc-builded-p) (build-libmlxc)))

(defun libmlxc-dir ()
  "Get/Set directory of libmlxc.dylib. "
  (merge-pathnames "build/" (asdf:component-pathname
                             (asdf:find-component :mlx-cl/lib :mlx-c))))

(pushnew '(libmlxc-dir) cffi:*foreign-library-directories*)

(cffi:define-foreign-library libmlxc
  (:darwin "libmlxc.dylib")
  (t       "libmlxc"))

(defun load-libmlxc ()
  "Load libmlxc.dylib into Lisp environment. "
  (restart-case (cffi:load-foreign-library 'libmlxc)
    (install-libmlxc ()
      (install-libmlxc)
      (load-libmlxc))))

;;;; build.lisp ends here
