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
  :bind(("C-s" . swiper-isearch)
	("C-r" . swiper-isearch-backwark)))

;; (require 'swiper)
;; (define-key global-map (kbd "C-s") 'swiper-isearch)
;; (define-key global-map (kbd "C-r") 'swiper-isearch-backward)

(use-package counsel
  :diminish
  :hook (after-init . counsel-mode)
  :bind (:map counsel-mode-map
	      ([remap find-file] . counsel-find-file)
	      ([remap amx] . counsel-M-x)
	      ([remap swiper] . counsel-grep-or-swiper)
	      ([remap swiper-backword] . counsel-grep-or-swiper-backword)
	      ([remap dired] . counsel-dired)
	      ([remap recentf] . counsel-recentf)))

;; (require 'counsel)
;; (diminish 'counsel-mode)
;; (add-hook 'after-init-hook 'counsel-mode)
;; (with-eval-after-load 'counsel
;;   (define-key counsel-mode-map [remap find-file] 'counsel-find-file)
;;   (define-key counsel-mode-map [remap amx] 'counsel-M-x)
;;   (define-key counsel-mode-map [remap swiper] 'counsel-grep-or-swiper)
;;   (define-key counsel-mode-map [remap swiper-backword] 'counsel-grep-or-swiper-backword)
;;   (define-key counsel-mode-map [remap dired] 'counsel-dired)
;;   (define-key counsel-mode-map [remap recentf] 'counsel-recentf))

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
