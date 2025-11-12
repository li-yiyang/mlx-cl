;;;; colorspace.lisp --- Implementation of colorspaces transformation

(in-package :mlx-cl.image)

;;; Commentary:
;; see `colorspace-internal.lisp' for the colorspace

;;; Utils

(defun scale<-d (image)
  "Return IMAGE depth scale factor. "
  (ecase (dtype image)
    (:uint8   255)
    (:uint16  65535)
    (:float32 1.0)))

(defun alpha<- (image alpha)
  "Rescale ALPHA"
  (etypecase alpha
    (integer alpha)
    (float
     (assert (cl:<= 0 alpha 1) ()
             "Expect alpha scale to be [0, 1], but got: ~A"
             alpha)
     (cl:* alpha (scale<-d image)))
    (mlx-array
     (assert (dim= alpha 0))
     (alpha<- image (lisp<- alpha)))))

(defun add-alpha-channel (image &key (alpha 1.0) &allow-other-keys)
  "Add ALPHA channel to IMAGE. "
  (concat image
          (full (list (h image) (w image) 1)
                (alpha<- image alpha)
                :dtype (dtype image))
          :axis 2))

;;; Colorspaces

(define-colorspace :rgb
  "Red Green Blue. "
  :channels 3
  :alias    (:red-green-blue))

(define-colorspace :rgba
  "Red Green Blue Alpha. "
  :channels  4
  :alias     (:red-green-blue :rgb-alpha)
  :fallback  :rgb

  ;; Convert from:
  :rgb add-alpha-channel)

  ;; Convert to:
  ;; (:-> :rgb)
  ;; (lambda (image &key (background-color :white) &allow-other-keys)
  ;;   (let* ((*keep-dim-p* t)
  ;;          (rgb          (at image :* :* :butlast))
  ;;          (alpha        (at image :* :* :last))
  ;;          (bgcolor      (full (shape rgb)
  ;;                              (color<- background-color :rgb (scale<-d image)))))
  ;;     (bgcolor


(define-colorspace :grayscale
  "Grayscale image. "
  :channels 1
  :alias    (:gray)
  :fallback :rgb

  ;; Convert from:
  :rgb
  (lambda (image &key (method :default) (weight nil weight?))
    "Grayscale from RGB IMAGE. "
    :parameters ((method "how to convert the RGB image:
 + :ntsc, :rec601:
 + :atsc, :hdtv, :itu-r-bt.709:
 + :hdr, :itu-r-bt.2100:
 + :avg :average:
 + :red:   split red   channel of IMG
 + :green: split green channel of IMG
 + :blue:  split blue  channel of IMG")
                 (weight "if given, would use weight to inner dot on RGB"))
    (flet ((clsdot (weight)
             (->* (inner weight (as-type image :float32))
               (/ (sum weight))
               (as-type (dtype image))
               (image :colorspace :grayscale)))
           (nthchn (n)
             (let ((*keep-dim-p* t))
               (image (at image :* :* n) :colorspace :grayscale))))
      (if weight? (clsdot weight)
          (ecase method
            ((:default :ntsc :rec601)     (clsdot #(0.2989 0.5870 0.1140)))
            ((:atsc :hdtv :itu-r-bt.709)  (clsdot #(0.2126 0.7152 0.0722)))
            ((:hdr :itu-r-bt.2100)        (clsdot #(0.2627 0.6780 0.0593)))
            ((:avg :average)              (clsdot #(1.0    1.0    1.0)))
            (:red                         (nthchn 0))
            (:green                       (nthchn 1))
            (:blue                        (nthchn 2)))))))

(define-colorspace :grayscale-alpha
  "Grayscale with ALPHA channel. "
  :channels 2
  :alias    (:gray-alpha)
  :fallback :grayscale

  ;;; Convert from:
  :grayscale add-alpha-channel)

;;;; colorspace.lisp ends here
