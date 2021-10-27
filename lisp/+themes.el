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

;; (use-package doom-themes
;;   :init
;;   (setq doom-themes-enable-bold t
;; 	doom-themes-enable-italic t)
;;    ;; all-the-icons must be installed!
;;   (with-eval-after-load 'neotree
;;     (doom-themes-neotree-config)
;;     (setq doom-themes-neotree-file-icons 't))
;;   (with-eval-after-load 'org-mode
;;     (doom-themes-org-config))
;;   :config
;;   (setq custom-enabled-themes '(doom-xcode))
;;   (ewx-apply-themes))

(use-package modus-themes
  :init
  :config
  (modus-themes-load-themes)
  (modus-themes-load-operandi))

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
   (setq doom-modeline-lsp t)
   (setq doom-modeline-workspace-name t)
   (setq doom-modeline-unicode-fallback nil))

(provide '+themes)
;;; +themes.el ends here
