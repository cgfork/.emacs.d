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

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:1235")
     ("https" . "127.0.0.1:1235")))

(setq straight-base-dir user-emacs-directory
      straight-cache-autoloads t
      straight-repository-branch "develop"
      straight-check-for-modifications '(check-on-save find-when-checking)
      straight-enable-package-integration nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      use-package-always-ensure nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

(use-package which-key
  :config
  (which-key-mode t))

(require 'simple)
(add-hook 'after-init-hook #'size-indication-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'after-init-hook #'global-hl-line-mode)

(use-package diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require '+shell)
(require '+buffer)
(require '+window)
(require '+projectile)
(require '+sidebar)
(require '+themes)
(require '+editor)
(require '+company)
(require '+flycheck)
(require '+lsp)
(require '+go)
(require '+rust)
(require '+markdown)
(require '+org)
(require '+plantuml)
(require '+git)
;;
;; (set-frame-font "-*-Ubuntu Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default (selected-frame) "-*-Ubuntu Mono-normal-normal-normal-*-14-*-*-*-m-0-fontset-auto9")
;; (set-face-attribute 'default (selected-frame) :font "-*-Ubuntu Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(cond
 ((member "Ubuntu Mono" (font-family-list))
  (set-face-attribute 'default nil :font "Ubuntu Mono-12"))
 ((member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-12")))

(when (member "HYKaiTiJ" (font-family-list))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-unknown-HYKaiTiJ-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
		:weight 'normal
		:slant 'normal
		:size 10))))
(provide 'init)
;;; Init.el ends here
