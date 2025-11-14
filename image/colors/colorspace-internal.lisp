;;;; colorspace-internal.lisp --- Internal rule interpreter of colorspace

(in-package :mlx-cl.image)

;; Dev Note:
;; the %colorspace
(defstruct %colorspace
  (name        :colorspace                  :type keyword)
  (channels    1                            :type (integer 1))
  (alias       ()                           :type list)
  (convert     (make-hash-table :test 'eql) :type hash-table)
  (fallback    nil                          :type (or null keyword))
  (description ""                           :type string))


;;; *colorspaces* as rule table

(defparameter *colorspaces*
  (make-hash-table :test 'eql)
  "Hash table of MLX-CL.IMAGE colorspace.
Key: color space name.
Val: a `%colorspace' structure to hold the infomation of colorspace.

Dev Note: all the `mlx-cl.image' known `colorspace' should be
registered here. The infomation and transform rules of colorspaces
should be defined in `mlx-cl.image::%colorspace' structure. ")

(defun get-%colorspace (colorspace)
  "Get `%colorspace' of name COLORSPACE.
Return `%colorspace'.

Dev Note:
this is a wrapper of `gethash' for `mlx-cl.image::*colorspaces*'. "
  (declare (type keyword colorspace))
  (or (gethash colorspace *colorspaces*)
      (error "Unknown colorspace `~S'" colorspace)))

(defun set-%colorspace (name channels alias fallback description)
  "Create and set a new `%colorspace'.
See `define-colorspace'. "
  (declare (type keyword name)
           (type (integer 1) channels))
  (assert (every #'keywordp alias))
  (let ((colorspace (make-%colorspace
                      :name        name
                      :channels    channels
                      :alias       (union (list name) alias)
                      :convert     (make-hash-table :test 'eql)
                      :fallback    (when fallback
                                     (colorspace-name fallback))
                      :description (or description ""))))
    (dolist (name (cons name alias))
      (setf (gethash name *colorspaces*) colorspace))))

(defun del-%colorspace (colorspace &optional all)
  "Remove COLORSPACE.

Parameters:
+ COLORSPACE: if COLORSPACE is the base name of the colorspace,
  all the alias of COLORSPACE would be removed
+ ALL: if ALL, remove regardless whether the COLORSPACE is or not
  the base name of the colorspace, remove all the aliases together
"
  (declare (type keyword colorspace))
  (let ((space (gethash colorspace *colorspaces*)))
    (when space
      (cond ((or all                    ; remove all
                 (eql (%colorspace-name space) colorspace))
             (dolist (colorspace (%colorspace-alias space))
               (setf (gethash colorspace *colorspaces*) nil)))
            (t
             (setf (%colorspace-alias space)
                   (remove colorspace (%colorspace-alias space)))
             (setf (gethash colorspace *colorspaces*) nil))))))

(defun colorspaces (&rest subsets)
  "Return a list of all colorspaces in `mlx-cl.image'.

Parameters:
+ SUBSETS: limitation of colorspace

Dev Note: this is like keys for hash-table
"
  (if (endp subsets)
      (let ((colorspaces ()))
        (maphash (lambda (k v) (declare (ignore v)) (push k colorspaces))
                 *colorspaces*)
        colorspaces)
      (reduce #'union
              (mapcar (lambda (c) (%colorspace-alias (get-%colorspace c)))
                      subsets))))

(defun colorspacep (colorspace &rest subsets)
  "Test if COLORSPACE is a valid colorspace.

Parameters:
+ COLORSPACE: a keyword of colorspace
+ SUBSETS: if given, reduce COLORSPACE in colorspace subsets"
  (and (keywordp colorspace)
       (if (endp subsets)
           (gethash colorspace *colorspaces*)
           ;; iter over `subsets' to check if they are all valid colorspace
           (loop :for subset :in subsets
                 :for space := (get-%colorspace subset)
                 :if (member colorspace (%colorspace-alias space))
                   :return t))
       t))

(deftype colorspace (&rest subsets)
  "Currently supported image colorspace.

Parameters:
+ SUBSETS: subset of the colorspace"
  `(and keyword (member ,@(apply #'colorspaces subsets))))


;;; Attributes

(defun add-colorspace-alias (colorspace alias)
  "Add ALIAS to COLORSPACE.

Parameters:
+ COLORSPACE: original colorspace
+ ALIAS: alias of colorspace"
  (declare (type keyword colorspace alias))
  (let ((colorspace (get-%colorspace colorspace)))
    (pushnew alias (%colorspace-alias colorspace))
    (setf (gethash alias *colorspaces*) colorspace)))

(defun colorspace-name (colorspace &optional (errorp t))
  "Get name of COLORSPACE, error if not found.
Return keyword of COLORSPACE.

Parameters:
+ COLORSPACE: keyword of colorspace
+ ERRORP: if nil, would ignore if not found, return COLORSPACE
"
  (declare (type keyword colorspace))
  (let ((space (gethash colorspace *colorspaces*)))
    (cond (space  (%colorspace-name space))
          (errorp (error "Unknown colorspace ~S. " colorspace))
          (t      colorspace))))

(defun add-colorspace-convert-rule (from to rule)
  "Set how to convert FROM colorspace to TO colorspace by RULE.

Parameters:
+ FROM, TO: colorspace name, if not created, would create a
  new `mlx-cl.image::%colorspace' in `mlx-cl.image::*colorspaces*'
+ RULE: symbol of function or function.

Dev Note: this would not check RULE lambda-list. "
  (declare (type keyword from to)
           (type (or symbol function) rule))
  (let* ((from       (colorspace-name from))
         (to         (colorspace-name to))
         (colorspace (get-%colorspace to)))
    (setf (gethash (colorspace-name from)
                   (%colorspace-convert colorspace))
          rule)))

(defun colorspace-channels (colorspace)
  "Return channels of COLORSPACE. "
  (the integer (%colorspace-channels (get-%colorspace colorspace))))


;;; Dev Interface Wrapper

(defun color-ch-val<- (value)
  "Convert VALUE as channel value (float [0-1]) for `color'.
Return a float [0, 1] for generating the color. "
  (the (float 0 1)
    (etypecase value
      (float     value)
      (integer   (cl:/ value 255.0))
      (mlx-array (assert (dim= value 0))
       (color-ch-val<- (lisp<- value))))))

(defmacro define-colorspace
    #+sbcl
    (name docstring
     &rest keys
     &key channels alias fallback
     &allow-other-keys)
  #+lispworks
  (name docstring &rest keys
        &aux
        (channels (getf keys :channels))
        (alias    (getf keys :alias))
        (fallback (getf keys :fallback)))
  "Define colorspace of NAME with OPTIONS.
Return NAME of colorspace.

Syntax:

    (define-colorspace NAME
      docstring
      :alias     aliases
      :fallback  fallback
      :channels  channels
      :rgb       function-to-convert-from-RGB
      (:<- :rgb) function-to-convert-from-RGB
      (:-> :rgb) function-to-convert-to-RGB
      . KEYS)

Parameters:
+ NAME:
+ DOCSTRING:
+ CHANNELS:
+ ALIAS:
+ FALLBACK:
+ ...

Note: this will also create a new function of NAME, which
would CHANNELS number arguments as input and return `color'.
"
  (declare (type keyword name)
           (type string  docstring)
           (type list    alias)
           (type (or null keyword) fallback)
           (type (or integer list) channels))
  (let* ((num-chn (if (integerp channels) channels (length channels)))
         (chn-var (if (listp channels)
                      (loop :for var :in channels
                            :collect (intern (format nil "~:@(~A~)" var)))
                      (loop :for i :from 1 :upto channels
                            :collect (intern (format nil "C~D" i))))))
    `(progn
       (set-%colorspace ,name ,num-chn ',alias ,fallback ,docstring)
       (defun ,(intern (symbol-name name)) ,chn-var
         ,(mlx::gen-doc
           (format nil "Create a `color' object of colorspace `~S'. " name)
           :parameters (loop :for var :in chn-var
                             :collect (list var (format nil "value for channel ~A" var)))
           :note (format nil
                         "See documentation of `~S' for colorspace: (documentation ~S 'colorspace). "
                         name name))
         (declare (type (or float integer mlx-array) ,@chn-var))
         (color (mlx-array (list ,@(loop :for var :in chn-var
                                         :collect `(color-ch-val<- ,var)))
                           :dtype :float32)
                :colorspace ,name))
     ,@(loop :with fallback? := nil
             :for (opt fn) :on keys :by #'cddr
             :for (dir space) := (if (keywordp opt)
                                     (list :<- opt)
                                     opt)
             :for from := (if (eql dir :<-) space name)
             :for to   := (if (eql dir :<-) name  space)
             :if (eql fallback from)
               :do (setf fallback? t)
             :if (not (member from '(:channels :alias :fallback)))
               :collect (etypecase fn
                          (symbol
                           `(add-colorspace-convert-rule ,from ,to ',fn))
                          (function
                           `(add-colorspace-convert-rule ,from ,to ,fn))
                          (list
                           (if (eql (first fn) 'lambda)
                               (destructuring-bind (lambda-list . body) (rest fn)
                                 `(define-colorspace-convert
                                      (,from :-> ,to) ,lambda-list
                                    ,@body))
                               `(add-colorspace-convert-rule ,from ,to ,fn))))
                 :into def-rules
             :finally (progn
                        (when (and fallback (not fallback?))
                          (warn "Colorspace ~S fallbacks to ~S, but not implemented how to convert from ~S. "
                                name fallback fallback))
                        (return def-rules)))
     ',name)))

(defmacro define-colorspace-convert
    ((colorspace1 dir colorspace2) lambda-list &body body)
  "Define a convert rule from colorspace FROM to colorspace TO.

Syntax:

    (define-colorspace-convert (:FROM :-> :TO) function)
    (define-colorspace-convert (:TO :<- :FROM) lambda-list &body body)
"
  (let (from to lambda docstring declares progn)
    ;; parse (colorspace1 dir colorspace2)
    (ecase dir
      (:-> (setf from colorspace1
                 to   colorspace2))
      (:<- (setf from colorspace2
                 to   colorspace1)))
    (etypecase lambda-list
      (symbol   `(add-colorspace-convert-rule ,from ,to ',lambda-list))
      (function `(add-colorspace-convert-rule ,from ,to  ,lambda-list))
      (list
       ;; parse lambda-list
       (multiple-value-bind (required optional rest keys others)
           (mlx::parse-lambda-list lambda-list)
         (unless (cl:= (length required) 1)
           (error
            "lambda-list should take only one required as image input, but got:~%~S"
            required))
         (unless (endp optional)
           (error
            "the lambda-list should take no optional parameters, but got:~%~S"
            optional))
         (setf lambda `(,@required
                        ,@(when rest `(&rest ,rest))
                        &key ,@keys
                        ,@(unless others '(&allow-other-keys)))))
       ;; parse body
       (multiple-value-bind (doc plist body)
           (mlx::split-doc-body body)
         (setf docstring
               (apply #'mlx::gen-doc
                      (or doc
                          (format nil
                                  "Convert colorspace from `~S' to `~S'. "
                                  from to))
                      plist))
         (loop :for (expr . rest) :on body
               :if (and (listp expr)
                        (eql (car expr) 'declare))
                 :do (push expr declares)
               :else
                 :do (return (setf progn (cons expr rest)))))
       `(add-colorspace-convert-rule
         ,from ,to
         (lambda ,lambda ,docstring ,@declares ,@progn))))))


;;; Interpreter

(defgeneric as-colorspace (image colorspace &key &allow-other-keys)
  (:documentation "Convert IMAGE to COLORSPACE.
Return an `image' object of COLORSPACE.

Parameters:
+ IMAGE: input image
+ COLORSPACE: target colorspace

Dev Note:
use (documentation (colorspace IMAGE) 'colorspace) to get documentation
of the colorspace of IMAGE.
")
  (:method ((arr mlx-array) (colorspace symbol) &rest keys &key)
    (declare (type keyword colorspace))
    (assert-mlx-array-is-image arr)
    (let ((space (get-%colorspace colorspace))
          (image (image arr)))
      (cond ((cl:= (channels arr)
                   (%colorspace-channels space))
             (setf (slot-value image 'colorspace)
                   (%colorspace-name space))
             image)
            (t
             (apply #'as-colorspace image colorspace keys)))))
  (:method ((image image) (colorspace symbol) &rest keys &key)
    (declare (type keyword colorspace))
    (let* ((source      (colorspace image))
           (space       (get-%colorspace  colorspace))
           (target      (%colorspace-name space))
           (convert     (gethash source (%colorspace-convert space)))
           (fallback    (%colorspace-fallback space))
           (image       (cond ((eql source target) image)
                              (convert             (apply convert image keys))
                              (fallback
                               (->* image
                                 ;; 1. convert to fallback colorspace
                                 (apply #'as-colorspace * fallback keys)
                                 ;; 2. convert to target colorspace
                                 (apply (gethash fallback (%colorspace-convert space))
                                        *
                                        keys)))
                              (t (error "Unknown how to convert from ~S to ~S. "
                                        source target))))
           (image       (typecase image
                          (image     image)
                          (mlx-array (image image))
                          (t
                           (error "Failed to convert colorspace into ~S.
Please check colorspace convert rule return value from ~S to ~S. "
                                  target source target)))))
      (setf (slot-value image 'colorspace) target)
      image)))

(defmethod (setf colorspace) (colorspace (image image) &rest keys)
  "If IMAGE is not assigned with COLORSPACE infomation,
just accept the COLORSPACE with simple tests. "
  (let* ((space    (or (gethash colorspace *colorspaces*)
                       (error "Unknown colorspace ~S. " colorspace)))
         (name     (%colorspace-name     space))
         (names    (%colorspace-alias    space))
         (channels (%colorspace-channels space)))
    (when (or (slot-boundp image 'colorspace)
              (cl:/= (channels image) channels)
              (not (member (colorspace image) names)))
      (let ((new (the image (apply #'as-colorspace image name keys))))
        (mlx::%steal-mlx-array-pointer new image)))
    (setf (slot-value image 'colorspace) name)))

;;;; colorspace-internal.lisp ends here
