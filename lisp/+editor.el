;;; +editor.el --- Configuration for editing -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:
(power-require
  (diminish paredit highlight-parentheses goto-line-preview dimmer)
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (@keys global-map
	 ((kbd "C-x C-b") . #'ibuffer)
	 ((kbd "C->") . #'mc/mark-next-like-this))
  )

(provide '+editor)
;;; +editor.el ends here
