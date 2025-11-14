;;;; mlx-cl-tags.el ---- Emacs support for tags in mlx-cl codes.  -*- lexical-binding: t; -*-

;;;; Commentary:
;; This is some Emacs scripts to add some support for MLX-CL DEV:
;; + `mlx-cl-highlight-test-tags':
;;   refresh buffer's `mlx-cl-highlight' overlay,
;;   add highlights and jumps to test cases

(require 'mlx-cl)

(defface mlx-cl-test-tag-face
  '((t :background "#ffd278"
       :foreground "#ff6a0d"))
  "Highlight test tags for MLX-CL. "
  :group 'mlx-cl)

(defvar mlx-cl-test-file)

(defun mlx-cl-goto-test (tag)
  "Goto test. "
  (find-file-other-window
   (expand-file-name mlx-cl-test-file mlx-cl-test-dir))
  (goto-char (point-min))
  (if (re-search-forward (format "^(test %s$" (regexp-quote tag)) nil t)
      (progn
        (beginning-of-line)
        (message "Moved to MLX-CL test #%s in %s. " tag (buffer-name)))
    (progn
      (end-of-buffer)
      (open-line 1)
      (insert (format "(test %s\n" tag))
      (save-excursion
        (insert "\n)")))))

(defun mlx-cl-highlight-test-tags ()
  "Highlight all `#tags' on lines starting with `;; TEST:'."
  (interactive)
  (save-excursion
    ;; Remove old overlays
    (remove-overlays (point-min) (point-max) 'mlx-cl-highlight t)
    (goto-char (point-min))
    ;; Step 1: find lines starting with ';; TEST:'
    (while (re-search-forward ";; TEST: " nil t)
      (let ((line-end   (line-end-position))
            (line-start (match-beginning 0)))
        ;; Step 2: within that line, find all #tags
        (save-excursion
          (goto-char line-start)
          (while (re-search-forward "#\\([0-9A-Za-z-/<>~=]+\\)" line-end t)
            (let* ((beg (match-beginning 0))
                   (end (match-end       0))
                   (ov  (make-overlay beg end))
                   (tag (buffer-substring-no-properties (1+ beg) end)))
              (overlay-put ov 'face              'mlx-cl-test-tag-face)
              (overlay-put ov 'mlx-cl-highlight  t)
              (overlay-put ov 'mouse-face        'highlight)
              (overlay-put ov 'help-echo         (format "Go to test %s. " tag))
              (overlay-put
               ov 'keymap
               (let ((map   (make-sparse-keymap))
                     (goto `(lambda () (interactive) (mlx-cl-goto-test ,tag))))
                 (define-key map (kbd "<mouse-1>")      goto)
                 (define-key map (kbd "C-c C-o")        goto)
                 (define-key map (kbd "Enter")          goto)
                 map)))))))))

(define-minor-mode mlx-cl-tag-mode
  "Enable tag jump in code. "
  :init-value nil
  :group      mlx-cl
  (cond (mlx-cl-tag-mode
         (make-local-variable 'mlx-cl-test-file)
         (setq-local mlx-cl-test-file
                     (file-relative-name (buffer-file-name)
                                         mlx-cl-root-dir))
         (mlx-cl-highlight-test-tags)
         (add-hook 'after-save-hook 'mlx-cl-highlight-test-tags nil t))
        (t
         (kill-local-variable 'mlx-cl-test-file)
         (remove-overlays (point-min) (point-max) 'mlx-cl-highlight t)
         (remove-hook 'after-save-hook 'mlx-cl-highlight-test-tags t))))

(provide 'mlx-cl-tags)

;;;; mlx-cl-tags.el ends here
