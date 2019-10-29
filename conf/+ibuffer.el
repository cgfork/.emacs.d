;;; package --- summary
;;; Commentary:
;;; Code:

(use-package ibuffer
  :ensure nil
  :commands (ibuffer-find-file
	     ibuffer-current-buffer)
  :bind (("C-x C-b" . ibuffer))
  :init
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
				 (if (buffer-live-p buf)
				     (with-current-buffer buf default-directory)
				   default-directory))))
	(counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))
  (use-package ibuffer-projectile
    :ensure t))

(provide '+ibuffer)
;;; +ibuffer.el ends here
