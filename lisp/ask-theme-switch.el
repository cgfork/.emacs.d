;;; ask-theme-switch.el --- Switch theme light or dark -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(defgroup ask-theme nil
  "Ask theme group"
  :group 'convenience)

(defcustom ask-theme-style 'light
  "The theme style: 'light or 'dark"
  :type '(choice (const light)
		 (const dark))
  :group 'ask-theme)

(defcustom ask-theme-dark 'tango-dark
  "The default dark theme."
  :type 'symbol
  :group 'ask-theme)

(defcustom ask-theme-light 'tango
  "The default light theme."
  :type 'symbol
  :group 'ask-theme)

(defun ask-theme-load ()
  "Load the theme specified by `ask-theme-style'."
  (cl-case ask-theme-style
    ('light (load-theme ask-theme-light t))
    ('dark (load-theme ask-theme-dark t))
    (t (message "Unknown style: %s" (symbol-name ask-theme-style)))))

(defun ask-theme-style-load ()
  "Load the specified STYLE theme."
  (interactive)
  (let ((style (completing-read "Theme Style (light or dark): "
				'("light" "dark")
				nil
				t
				(symbol-name ask-theme-style))))
    (cl-case (intern style)
      ('light (setq ask-theme-style 'light))
      ('dark (setq ask-theme-style 'dark))
      (t (message "Unknown style: %s" style)))
    (ask-theme-load)))

(defun ask-theme-style-switch ()
  "Switch the light or dark theme."
  (interactive)
  (cl-case ask-theme-style
    ('light (setq ask-theme-style 'dark))
    ('dark (setq ask-theme-style 'light))
    (t (message "Unknown style: %s" (symbol-name ask-theme-style))))
  (ask-theme-load))

(provide 'ask-theme-switch)
;;; ask-theme-switch.el ends here
