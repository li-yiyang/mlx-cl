;;;; io.lisp --- Read and Write PPM file for MLX

(in-package :mlx-cl.io.ppm)

;;; Dev Note:
;; https://netpbm.sourceforge.net/doc/ppm.html
;;
;; The PPM format is a lowest common denominator color image file format.
;; Each PPM image consists of the following:
;; 1. A "magic number" for identifying the file type.
;;    A ppm image's magic number is the two characters "P6".
;; 2. Whitespace (blanks, TABs, CRs, LFs).
;; 3. A width, formatted as ASCII characters in decimal.
;; 4. Whitespace.
;; 5. A height, again in ASCII decimal.
;; 6. Whitespace.
;; 7. The maximum color value (Maxval), again in ASCII decimal.
;;    Must be less than 65536 and more than zero.
;; 8. A single whitespace character (usually a newline).
;; 9. A raster of Height rows, in order from top to bottom.
;;    Each row consists of Width pixels, in order from left to right.
;;   Each pixel is a triplet of red, green, and blue samples, in that order.
;;    Each sample is represented in pure binary by either 1 or 2 bytes.
;;    If the Maxval is less than 256, it is 1 byte.
;;    Otherwise, it is 2 bytes. The most significant byte is first.
;;
;;    A row of an image is horizontal.
;;    A column is vertical.
;;    The pixels in the image are square and contiguous.

(mlx:defmlx-extension (:ppm "PPM")
  ((:load-path path)
   (flet ((whitespacep (code) (or (cl:= code 10)    ; #\Newline
                                  (cl:= code 32)    ; #\Space
                                  (cl:= code 9)     ; #\Tab
                                  (cl:= code 13)))) ; #\Return
     (mmap:with-mmap (addr fd size (uiop:native-namestring path))
       (assert (string= (foreign-string-to-lisp addr :count 2) "P6"))
       (assert (whitespacep (mem-aref addr :char 2)))
       (let ((idx 2))
         (flet ((read-int ()
                  (loop :with int := 0
                        :for chr := (mem-aref addr :char (incf idx))
                        :if (whitespacep chr)
                          :return int
                        :do (setf int (+ (* int 10) (- chr 48))))))
           (let* ((w     (read-int))
                  (h     (read-int))
                  (m     (read-int))
                  (shape (list h w 3)))
             (assert (= m 255))
             (mlx::wrap-as-mlx-array
              (mlx::with-foreign<-sequence (shape* shape :int)
                (mlx::mlx_array_new_data
                 (inc-pointer addr (1+ idx)) shape* 3 :uint8)))))))))
  ((:save-path arr path)
   (assert (mlx:dim= arr 3))
   (assert (mlx:len= arr 3 2))
   (assert (eql (mlx:dtype arr) :uint8))
   (with-fopen-file (fd path)
     (let* ((h    (mlx:shape arr :axis 0))
            (w    (mlx:shape arr :axis 1))
            (head (format nil "P6~%~D ~D~%255~%" w h)))
       (fwrite-string fd head)
       (fwrite-buffer fd (mlx::foreign-pointer<-mlx-array arr) :uint8 (* w h 3))))))

;;;; io.lisp ends here
