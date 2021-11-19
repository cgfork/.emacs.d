;;; +buffer.el --- buffer setup. -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: buffer
;; Package-requires: ((emacs "25.3") (ivy) (swiper) (counsel) (wgrep) (rg) (deadgrep))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'server)
(with-eval-after-load 'server
  (unless (server-running-p)
    (add-hook 'after-init-hook #'server-mode)))

(add-hook 'after-init-hook #'winner-mode)

(add-hook 'after-init-hook #'recentf-mode)
(with-eval-after-load 'recentf
  (setq recentf-max-saved-items 200
	recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$" "^/tmp/" "^/ssh:"
          "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  (push (expand-file-name recentf-save-file) recentf-exclude))

(add-hook 'after-init-hook #'savehist-mode)
(with-eval-after-load 'savehist
   (setq enable-recursive-minibuffers t 
         history-length 1000
         savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
         savehist-autosave-interval 300))

;; Completion engine
(use-package minibuffer
  :straight (:type built-in)
  :bind (:map minibuffer-local-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-ns-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-completion-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-must-match-map
         ([escape] . abort-recursive-edit)
         :map minibuffer-local-isearch-map
         ([escape] . abort-recursive-edit))
  :custom
  (completion-auto-help t)
  (completion-show-help nil)
  ;; Cycle completions regardless of the count
  (completion-cycle-threshold t)
  (enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
  (minibuffer-eldef-shorten-default t)
  (minibuffer-electric-default-mode t)
  ;; One frame one minibuffer.
  (minibuffer-follows-selected-frame nil)
  ;; Ignore cases when complete
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  ;; `selectrum', `vertico' and `icomplete' will honoring
  (completion-styles '(basic partial-completion substring flex))
  (completion-category-overrides '((buffer (styles . (flex)))))
  ;; vertical view
  (completions-format 'one-column)
  (completions-detailed t))

(use-package ivy
  :diminish
  :hook (after-init . ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer)
	 ("C-c C-r" . ivy-resume)
	 ("C-c C ." . ivy-switch-view))
  :config
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
	ivy-count-format "%d/%d"
        ivy-display-style 'fancy
        ivy-format-function 'ivy-format-function-line
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) 
				(t . ivy--regex-plus))
        ivy-initial-inputs-alist nil))

(use-package swiper
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backwark)))

;; on MacOS, ls doesn't support the --dired option while on Linux is supported.
(when sys/macp
  (setq dired-use-ls-dired nil))

(use-package counsel
  :diminish counsel-mode
  :hook (after-init . counsel-mode)
  :bind (:map counsel-mode-map
	      ([remap find-file] . counsel-find-file)
	      ([remap amx] . counsel-M-x)
	      ([remap swiper] . counsel-grep-or-swiper)
	      ([remap swiper-backword] . counsel-grep-or-swiper-backword)
	      ([remap dired] . counsel-dired)
	      ([remap recentf] . counsel-recentf)))

(with-eval-after-load 'ibuffer
  (defun my-ibuffer-find-file ()
    (interactive)
    (let ((default-directory (let ((buf (ibuffer-current-buffer)))
			       (if (buffer-live-p buf)
				   (with-current-buffer buf default-directory)
				 default-directory))))
      (counsel-find-file default-directory)))
  (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))

(use-package wgrep
  :init
  (setq-default grep-highlight-matches t
		grep-scroll-output t)
  :config
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(use-package deadgrep)

(when (eq system-type 'darwin)
  (setq-default locate-command "mdfind"))

(use-package rg
  :config
  (when (executable-find "rg")
    (global-set-key (kbd "C-c C-p") 'rg-project)))

(provide '+buffer)
;;; +buffer.el ends here
