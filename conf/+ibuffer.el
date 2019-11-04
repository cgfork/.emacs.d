;;; package --- Initialize ibuffer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
(with-eval-after-load 'ibuffer
  (define-key global-map (kbd "C-x C-b") 'ibuffer)
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file ()
      (interactive)
      (let ((default-directory (let ((buf (ibuffer-current-buffer)))
				 (if (buffer-live-p buf)
				     (with-current-buffer buf default-directory)
				   default-directory))))
	(counsel-find-file default-directory)))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))
  (cgfork/try-install 'ibuffer-projectile))

(provide '+ibuffer)
;;; +ibuffer.el ends here
