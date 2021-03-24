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
(require 'cask (expand-file-name "cask.d/cask.el" user-emacs-directory))
(cask-initialize)

(require 'evil)
(evil-mode t)

(require 'which-key)
(which-key-mode t)

(require 'general)
(general-evil-setup)
(general-create-definer yw-space-key-define
  :states '(normal visual motion evilified)
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "M-SPC")
(general-create-definer yw-comma-key-define
  :states '(normal visual motion evilified)
  :keymaps 'override
  :prefix ",")

;; Basic Key Bindings
(yw-space-key-define
  "f" '(nil :wk "file")
  "f f" 'find-file
  "f r" 'recentf)

(yw-comma-key-define
 "f" '(nil :wk "find")
 "f d" 'xref-find-definitions
 "f f" 'find-file-at-point
 "f r" 'xref-find-references
 "f s" 'xref-find-apropos)

(require 'simple)
(add-hook 'after-init-hook 'size-indication-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require '+shell)
(require '+buffer)
(require '+window)
(require '+projectile)
(require '+editor)

(provide 'init)
;;; Init.el ends here
