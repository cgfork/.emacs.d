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
 "f s" 'xref-find-apropos
 "f b" 'xref-pop-marker-stack)

(general-nmap xref--xref-buffer-mode-map
  "RET" 'xref-goto-xref
  "TAB" 'xref-quit-and-goto-xref
  "p" 'xref-prev-line
  "n" 'xref-next-line
  "q" 'quit-window)

(require 'simple)
(add-hook 'after-init-hook 'size-indication-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'after-init-hook 'global-hl-line-mode)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require '+shell)
(require '+buffer)
(require '+window)
(require '+projectile)
(require '+sidebar)
(require '+editor)
(require '+company)
(require '+flycheck)
(require '+lsp)
(require '+go)
(require '+rust)
(require '+markdown)
(require '+org)
(require '+plantuml)

(defun yw-apply-themes ()
  "Forcibly load the themes listed in the `custome-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))))

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory)) 
(setq custom-enabled-themes '(vscode-dark-plus))
(yw-apply-themes)

;; (set-frame-font "-*-Ubuntu Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default (selected-frame) "-*-Ubuntu Mono-normal-normal-normal-*-14-*-*-*-m-0-fontset-auto9")
;; (set-face-attribute 'default (selected-frame) :font "-*-Ubuntu Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(cond
 ((member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-14"))
 ((member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-12"))) 
(provide 'init)
;;; Init.el ends here
