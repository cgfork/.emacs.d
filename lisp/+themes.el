;;; +themes.el --- Load theme -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (doom-themes) (doom-modeline) (all-the-icons))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(defun yw-apply-themes ()
  "Forcibly load the themes listed in the `custome-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))))

(setq inhibit-compacting-font-caches t)

(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)
(setq custom-enabled-themes '(doom-xcode))
(yw-apply-themes)
;; all-the-icons must be installed!
(doom-themes-neotree-config)
(doom-themes-org-config)

(add-hook 'after-init-hook 'doom-modeline-mode)
(with-eval-after-load 'doom-modeline
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
