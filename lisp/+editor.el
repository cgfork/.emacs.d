;;; +editor.el --- Configuration for editing -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:
(@requires
  (diminish paredit highlight-parentheses goto-line-preview dimmer)
  (@keys global-map
	 ((kbd "C->") . mc/mark-next-like-this)
	 ([remap goto-line] . goto-line-preview))
  (@hooks eval-expression-minibuffer-setup-hook enable-paredit-mode)
  (with-eval-after-load 'elisp-mode
    (@hooks emacs-lisp-mode-hook enable-paredit-mode))
  (defmacro cgfork-open-paredit (mode-hook)
    (declare (indent 1) (debug t))
    `(add-hook ,mode-hook #'enable-paredit-mode))
  (when (foundp )))

(provide '+editor)
;;; +editor.el ends here
