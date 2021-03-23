;;; early-init.el ends here~;; early-init.el --- Configuration before loading packages -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

;; (setq package-enable-at-startup nil)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable scroll bar
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when (fboundp 'set-language-environment)
  (set-language-environment 'utf-8))

(when (fboundp 'display-time-mode)
  (display-time-mode t))

(when (fboundp 'save-place-mode)
  (save-place-mode t))

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t))

(setq-default
 column-number-mode t
 auto-save-default nil
 make-backup-files nil
 cursor-type 'bar
 ns-pop-up-frames nil
 tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; Set Key Modifiers.
(with-no-warnings
  (cond
   ((eq system-type 'windows-nt)
    (setq w32-lwindow-modifier 'super ; Left windows key
	  w32-apps-modifier 'hyper) ; Menu key
    (w32-register-hot-key [s-t]))
   ((eq system-type 'darwin)
    (setq mac-option-modifier 'super ; option
	  mac-command-modifier 'meta ; command
	  mac-control-modifier 'control ; control
	  ns-function-modifier 'hyper)))) ; fn

(provide 'early-init)
;;; early-init.el ends here
