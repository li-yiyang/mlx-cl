;;;; drawings.lisp --- Just draw something for fun

(uiop:define-package #:mlx-cl.tutorial.drawings
  (:use :mlx-cl)
  (:local-nicknames (:la :mlx-cl.linalg)))

(in-package :mlx-cl.tutorial.drawings)

;;; Utils
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun filter-plist (plist &rest keys)
    "Remove KEYS from PLIST.
  Return a new plist with KEYS removed. "
    (loop :for (key val) :on plist :by #'cddr
          :if (not (member key keys))
            :collect key :and :collect val))
  )

;;; Creating `mlx-array'
(defun fc (size &key (w size) (h size) (scale 1) (center nil)
                  (xmin (if center (/ w -2 scale) 0))
                  (xmax (if center (/ w  2 scale) scale))
                  (ymin (if center (/ h -2 scale) 0))
                  (ymax (if center (/ h  2 scale) scale)))
  "Return gl_FragCoord (H W (x y)=2). "
  (destructuring-bind (x y)
      (meshgrid (linspace xmin xmax w :dtype :float32)
                (linspace ymax ymin h :dtype :float32))
    (stack x y :axis 2)))
(defun rotate2d (theta)
  "Generate a 2-D vector rotation matrix. "
  (stack (stack (cos theta) (- (sin theta)))
         (stack (sin theta) (cos theta))))

;;; Normalize `mlx-array'
(defun ensure-mlx-array-as-rgb-img (arr)
  "Ensure input ARR is (H W 3) `mlx-array' of dtype `:uint8'.
Return the formatted `mlx-array'.

Rule:
1. ARR :uint8   -> ARR
   ARR :float32 -> ARR*255
2. ARR (H W)    -> (H W 3) ; repeat 3
   ARR (H W 3)  -> ARR
   ARR (H W 4)  -> (H W 3) ; RGB*A + RGB*(1-A)
"
  (declare (type mlx-array arr))
  (->* (ecase (dtype arr)
         (:uint8   (/ arr 255.0))
         (:float32 arr))
    (ecase (dim *)
      (2 (reshape (repeat * 3) `(,@(shape *) 3)))
      (3 (ecase (len * 2)
           (1 (reshape (repeat * 3) `(,@(shape * :axes #(0 1)) 3)))
           (3 arr)
           (4 (destructuring-bind (r g b a) (split * 4 :axis 2)
                (->* (clip a :max 1 :min 0)
                  (stack (* r *)
                         (* g *)
                         (* b *)
                         :axis 2)
                  (squeeze *)))))))
    (as-dtype (* * 255) :uint8)))
(defmacro x (fc)
  "Get X part of FC (gl_FragCoord.x). "
  `(at ,fc :* :* 0))

(defmacro y (fc)
  "Get Y part of FC (gl_FragCoord.y). "
  `(at ,fc :* :* 1))

;;; Arrows
(defmacro ->o (expr &body bodys-output)
  "Enhanced `->*' arrow to write image to OUTPUT.
Return OUTPUT file path.

Syntax:

    (->o EXPRS...
      OUTPUT)

Parameters:
+ EXPRS: chained expressions, see `->*'
+ OUTPUT: image output path"
  `(->* ,expr
     ,@(butlast bodys-output)
     (ensure-mlx-array-as-rgb-img *)
     (save * :output ,@(last bodys-output))))
(defmacro ->frame ((i frames output &rest keys
                    &key (tmp "./tmp/animated%d.ppm")
                    &allow-other-keys)
                   &body body)
  "Enhanced `->*' arrow to write image of frames to OUTPUT as animation.
Return OUTPUT file path.

Syntax:

    (->frame (I FRAMES OUTPUT &key) &body)

Parameters:
+ I: variable bind of current frame id (starting from 0)
+ FRAMES: number of total frames
+ OUTPUT: path of output
+ TMP: pattern of temporary frame output (default \"./tmp/animated%2d.ppm\")
  the format rules should follow FFMPEG file input formatting convention
+ DEBUG: see `ffmpeg'
"
  (declare (type symbol i))
  (let ((res (gensym "RES"))
        (pat (gensym "PAT"))
        (out (gensym "OUT")))
    `(let ((,res ())
           (,pat (uiop:native-namestring ,tmp))
           (,out (uiop:native-namestring ,output)))
       (unwind-protect
            (dotimes (,i ,frames (ffmpeg ,pat ,out ,@(filter-plist keys :tmp)))
              (->o ,@body
                (let ((tmp (ffmpeg-fmt ,pat ,i)))
                  (push tmp ,res)
                  tmp))
              ;; this is used to avoid cache expanding too fast
              (gc-all))
         (dolist (tmp ,res) (uiop:delete-file-if-exists tmp))))))

;;;; drawings.lisp ends here
