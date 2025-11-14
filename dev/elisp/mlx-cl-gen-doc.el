;;;; mlx-cl-gen-doc.el ---- Generate docs from org-mode.  -*- lexical-binding: t; -*-

(require 'mlx-cl)
(require 'cl-lib)

(defvar mlx-cl-gen-doc-exclude
  '("mlx-c" ".git" "." "..")
  "Exclude directory name. ")

(defvar mlx-cl-before-gen-doc-hook ()
  "Hooks to run before gen-doc. ")

(defvar mlx-cl-after-gen-doc-hook ()
  "Hooks to run after gen-doc. ")

;; Dev Note:
;; I use the following code in `mlx-cl-before-gen-doc-hook'
;; and `mlx-cl-after-gen-doc-hook':
;;
;; (add-hook 'mlx-cl-before-gen-doc-hook (lambda () (ryo.ui:mac-apply-theme 'dark)))
;; (add-hook 'mlx-cl-after-gen-doc-hook  (lambda () (ryo.ui:mac-apply-theme ns-system-appearance)))
;;
;; you may modify it to load `tao-yin' theme before generating the doc.
;;

(defun mlx-cl--gen-doc (dir)
  "Generate all the documentation of mlx-cl under DIR. "
  (dolist (name (directory-files dir))
    (let ((full (expand-file-name name dir)))
      (cond ((f-directory-p full)
             (unless (cl-member name mlx-cl-gen-doc-exclude :test 'string=)
               (mlx-cl--gen-doc full)))
            ((string= (file-name-extension name) "org")
             (with-current-buffer (find-file-noselect full)
               (save-buffer)
               (let* ((org-export-with-broken-links t)
                      (html (org-html-export-to-html)))
                 (rename-file (expand-file-name html dir)
                              (expand-file-name "index.html" dir)
                              t))))))))

(cl-defun mlx-cl-gen-doc (&optional (dir mlx-cl-root-dir))
  "Generate all the documentation of mlx-cl. "
  (interactive)
  (run-hooks 'mlx-cl-before-gen-doc-hook)
  (mlx-cl--gen-doc dir)
  (run-hooks 'mlx-cl-after-gen-doc-hook))

(provide 'mlx-cl-gen-doc)

;;;; mlx-cl-gen-doc.el
