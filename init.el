;; package --- summary
;;; Commentary:
;;; Code:

(when (version< emacs-version "28.0")
  (error "This requires Emacs 28.0 or above!"))

;; Set Key Modifiers.
(with-no-warnings
  (cond
   (sys/win32p
    (setq w32-lwindow-modifier 'super ; Left windows key
	  w32-apps-modifier 'hyper) ; Menu key
    (w32-register-hot-key [s-t]))
   (sys/macp
    (setq mac-option-modifier 'meta ; option
	  mac-command-modifier 'super ; command
	  mac-control-modifier 'control ; control
	  ns-function-modifier 'hyper)))) ; fn

(setq confirm-kill-emacs 'y-or-n-p)

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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(when (display-graphic-p)
  (when (or sys/macp sys/linuxp)
    (require 'ask-shell-environments)
    (ask-copy-shell-variables ask-shell-executable "PATH")))

(defvar ask-start-emacs-without-packages nil)
(defun check-custom-command-arg (arg)
  (let ((found (member arg command-line-args)))
    (setq command-line-args (delete arg command-line-args))
    found))

(when (check-custom-command-arg "-mini")
  (setq ask-start-emacs-without-packages t))

(unless ask-start-emacs-without-packages
  (require 'ask-straight)
  ;; https://github.com/dholm/benchmark-init-el/issues/15
  (cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
    (use-package benchmark-init
      :config
      (require 'benchmark-init-modes)                                     ; explicitly required
      (add-hook 'after-init-hook #'benchmark-init/deactivate)))
  (use-package diminish)
  (use-package which-key
    :init
    (which-key-mode t)))

(require 'ask-emacs)
(require 'ask-font)
(require 'ask-windows)
(require 'ask-project)
(require 'ask-minibuffer)
(require 'ask-edit)
(require 'ask-dired)
;; (require 'ask-tab-bar)
(require 'ask-headerline)
;; completion 需要在 ask-minibuffer 之后加载，不然 completion-style 会被 ask-minibuffer 的配置覆盖
(require 'ask-completion)
(require 'ask-git)
(require 'ask-org)
(require 'ask-markdown)
(require 'ask-plantuml)
(require 'ask-tempo)
(require 'ask-lsp)
(require 'ask-go)
(require 'ask-rust)
(require 'ask-dark-theme)
(require 'ask-acme-theme) ;; light theme
(require 'ask-theme-switch)


;; Enable dark theme if terminal
;; (unless (display-graphic-p)
;;   (setq ask-theme-style 'dark))
;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions (lambda (frame)
;; 					    (ask-theme-load)))
;;   (ask-theme-load))


(provide 'init)
;;; Init.el ends here
(put 'scroll-left 'disabled nil)
