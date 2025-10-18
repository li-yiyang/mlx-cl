;;;; vector.lisp --- Vector for MLX-C

(in-package :mlx-cl)


;;;; Lowlevel

(macrolet ((mlx-vector-* (&rest types)
             `(progn
                ,@(loop :for (type mlx-name dtype alloc) :in types
                        :for ctype := (intern* 'mlx-vector- type)
                        :for valloc := (intern* 'mlx_vector_ mlx-name '_new)
                        :collect `(defctype ,ctype :pointer)
                        :collect `(defcfun (,valloc
                                            ,(format nil "mlx_vector_~(~A~)_new" mlx-name))
                                      ,ctype)
                        :collect `(defcsetfun ,(format nil "mlx_vector_~(~A~)_set" mlx-name)
                                      (,ctype :alloc (,valloc))
                                    ,ctype ,ctype)
                        :collect `(defun ,(intern* 'mlx_vector_ type '_free) (,ctype)
                                    (ensure-success
                                        ,(format nil "mlx_vector_~(~A~)_free" mlx-name)
                                      ,ctype ,ctype))
                        :collect `(defcfun (,(intern* 'mlx_vector_ type '_size)
                                            ,(format nil "mlx_vector_~(~A~)_size" mlx-name))
                                      :size
                                    (,ctype ,ctype))
                        :collect `(defcsetfun ,(format nil "mlx_vector_~(~A~)_get" mlx-name)
                                      (,dtype ,@(when alloc `(:alloc (,alloc))))
                                    ,ctype ,ctype
                                    :size  idx)
                        :collect `(defcfun (,(intern* 'mlx_vector_ type '_new_data)
                                            ,(format nil "mlx_vector_~(~A~)_new_data" mlx-name))
                                      ,ctype
                                    (,ctype ,ctype)
                                    (size :size))))))
  (mlx-vector-*
   (array        array         mlx-array        mlx_array_new)
   (vector-array vector_array  mlx-vector-array mlx_vector_array_new)
   (int          int           :int             nil)
   (string       string        mlx-string       mlx_string_new)))


;;;; Highlevel

(defun wrap-as-mlx-array-list (mlx-vector-array)
  "Turn `mlx-vector-array' into a list of `mlx-array'. "
  (loop :for idx :below (mlx_vector_array_size mlx-vector-array)
        :collect (wrap-as-mlx-array
                  (mlx_vector_array_get mlx-vector-array idx))))

(defmacro with-array-vector<-sequence
    ((vector sequence &optional (len (gensym "LEN"))) &body body)
  "Turn SEQUENCE of `mlx-array' into VECTOR. "
  (let ((data (gensym "DATA")))
    `(with-foreign<-sequence (,data (map 'list #'mlx-object-pointer ,sequence) :pointer ,len)
       (let ((,vector (mlx_vector_array_new_data ,data ,len)))
         (unwind-protect (progn ,@body)
           (mlx_vector_array_free ,vector))))))

;;;; vector.lisp ends here
