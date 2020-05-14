;;; +themes.el --- Themes -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;
(power-emacs-install 'color-theme-sanityinc-solarized)
(power-emacs-install 'color-theme-sanityinc-tomorrow)

;; (setq custom-safe-themes t)
;; (setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (add-hook 'after-init-hook 'power-emacs-apply-themes)

(defun cgfork-light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (power-emacs-apply-themes))

(defun cgfork-dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (power-emacs-apply-themes))

(provide '+themes)
;;; +themes.el ends here
