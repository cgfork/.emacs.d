;;; +editor.el --- Configuration for editing -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

;; Setup paredit for lisp programming.
;; If you want to open paredit mode, you should add the hook
;; to `enable-paredit-mode'.
(@->
 ('diminish 'paredit 'highlight-parentheses)
 (global-highlight-parentheses-mode 1)
 (defmacro cgfork-open-paredit (mode-hook)
   "Add the `enable-paredit-mode' to the specific MODE-HOOK."
   `(add-hook ,mode-hook #'enable-paredit-mode))
 (cgfork-open-paredit 'eval-expression-minibuffer-setup-hook)
 (with-eval-after-load 'elisp-mode
   (cgfork-open-paredit 'emacs-lisp-mode-hook))
 (add-hook 'after-init-hook #'show-paren-mode))

(@->
 ('goto-line-preview)
 (global-set-key [remap goto-line] 'goto-line-preview)
 (when (fboundp 'display-line-numbers-mode)
   (defun cgfork-with-display-line-numbers (f &rest args)
     (let ((display-line-numbers t))
       (apply f args)))
   (advice-add 'goto-line-preview :around #'cgfork-with-display-line-numbers)))


(provide '+editor)
;;; +editor.el ends here
