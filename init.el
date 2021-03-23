;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "25.3")
  (error "This requires Emacs 25.3 or above!"))

;; Load `custom-file'.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; If it doesn't exist, copy from the template, then load it.
(let ((custom-template-file (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
	   (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))
(if (file-exists-p custom-file)
    (load custom-file)
  (message "custom file is not exist!"))

;; Load `custom-post.el'.
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((file (expand-file-name "custom-post.el" user-emacs-directory)))
	      (when (file-exists-p file)
		(load file)))))

;; To avoid 'Warning (package): Unnecessary call to ‘package-initialize’ in init file [2 times]'.
(setq warning-suppress-log-types '((package reinitialization)))
;; Load `cask.el'.
(require 'cask (expand-file-name ".cask/cask.el" user-emacs-directory))
(cask-initialize)

(require 'evil)
(evil-mode t)

(require 'which-key)
(which-key-mode t)

(provide 'init)
;;; Init.el ends here
