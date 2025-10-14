;;;; string.lisp --- MLX-C string

(in-package :mlx-cl)


;;;; Lowlevel

;; struct mlx_string { void* ctx; };
(defctype mlx-string :pointer)

;; mlx_string mlx_string_new();
(defcfun (mlx_string_new "mlx_string_new") mlx-string)

;; char * mlx_string_data(mlx_string mlx_string);
(defcfun (mlx_string_data "mlx_string_data") :string
  (mlx-string mlx-string))

;; mlx_string mlx_string_new_data(char * string);
(defcfun (mlx_string_new_data "mlx_string_new_data") mlx-string
  (string :string))

;; int mlx_string_free(mlx_string mlx_string);
(defcfun (mlx_string_free "mlx_string_free") :int
  (mlx-string mlx-string))

(defalias mlx-string<-string mlx_string_new_data)
(defalias string<-mlx-string mlx_string_data)

(defmacro with-mlx-string& ((str str& &optional initial-contents) &body body)
  "Allocate a new mlx-string as STR, a pointer STR& to the mlx-string.

Parameters:
+ STR   a variable to the allocated mlx-string
+ STR&  a variable as the pointer to mlx-string
+ INITIAL-CONTENTS if given, allocate mlx-string with initial string

Dev Notes:
This is equal to calling:

  mlx_string str = mlx_string_new();
             // or mlx_string_new_data(initial_contents);
  mlx_string* str& = &str;

  // body...

  mlx_string_free(str);
"
  `(with-elem& (,str ,str&
                :type  :pointer
                :free  (mlx_string_free ,str)
                :alloc ,(if initial-contents
                            `(mlx_string_new_data ,initial-contents)
                            '(mlx_string_new)))
     ,@body))

;;;; string.lisp
