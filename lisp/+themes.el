;;; +themes.el --- Load theme -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (doom-themes) (doom-modeline) (all-the-icons))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(use-package all-the-icons
  :if (display-graphic-p)
  :init
  (setq inhibit-compacting-font-caches t))

(defun ewx-apply-themes ()
  "Forcibly load the themes listed in the `custome-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))))

(defvar ewx-load-theme-hook nil
  "Hook run after the theme is loaded with `load-theme'.")

(defun ewx-run-load-theme-hooks (&rest _)
  "Run the hooks after `load-theme'."
  (run-hooks 'ewx-load-theme-hook))

(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  ;; all-the-icons must be installed!
  (add-hook 'ewx-load-theme-hook
	    (lambda ()
	      (progn
		 (with-eval-after-load 'neotree
		   (doom-themes-neotree-config)
		   (setq doom-themes-neotree-file-icons 't))
		 (with-eval-after-load 'org-mode
		   (doom-themes-org-config)))))
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions (lambda (frame) (load-theme 'doom-dark+ t)))
    (load-theme 'doom-dark+ t)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
   (when (display-graphic-p)
    (progn 
      (setq doom-modeline-height 15)
      (setq doom-modeline-bar-width 4)))
   (setq doom-modeline-icon (display-graphic-p))
   (setq doom-modeline-window-width-limit fill-column)
   (setq doom-modeline-project-detection 'auto)
   (setq doom-modeline-minor-modes nil)
   (setq doom-modeline-major-mode-icon t)
   (setq doom-modeline-major-mode-color-icon t)
   (setq doom-modeline-modal-icon nil)
   (setq doom-modeline-lsp t)
   (setq doom-modeline-workspace-name t)
   (setq doom-modeline-unicode-fallback nil))

(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(provide '+themes)
;;; +themes.el ends here
