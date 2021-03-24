;;; +editor.el --- editor setup  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: goto yasnippet
;; Package-requires: ((emacs "25.3") (paredit) (highlight-parentheses) (goto-line-preview) (multiple-cursors) (avy) (yasnippet) (yasnippet-snippets))

;;; Commentary:

;;; Code:

(defmacro yw-open-paredit (mode-hook)
  "Add the `enable-paredit-mode' to the specific MODE-HOOK."
  `(add-hook ,mode-hook #'enable-paredit-mode))

(yw-open-paredit 'eval-expression-minibuffer-setup-hook)
(with-eval-after-load 'elisp-mode
  (yw-open-paredit 'emacs-lisp-mode-hook))
(add-hook 'after-init-hook #'show-paren-mode)

;; (require 'highlight-parentheses)
(global-highlight-parentheses-mode 1)
(global-set-key [remap goto-line] 'goto-line-preview)
(when (fboundp 'display-line-numbers-mode)
  (defun yw-with-display-line-numbers (f &rest args)
    (let ((display-line-numbers t))
      (apply f args)))
  (advice-add 'goto-line-preview :around #'yw-with-display-line-numbers))

(yw-space-key-define
  "m" '(nil :wk "mark")
  "m n" 'mc/mark-next-like-this
  "m p" 'mc/mark-previous-like-this
  "m a" 'mc/mark-all-like-this
  "m d" 'mc/mark-all-dwim)

(avy-setup-default)
(yw-space-key-define
  "j" '(nil :wk "jump")
  "j c" 'avy-goto-char-2
  "j f" 'beginning-of-defun
  "j l" 'avy-goto-line
  "j j" 'avy-resume)

(diminish 'yas-minor-mode)
(yas-global-mode 1)

(autoload 'yas-expand-snippet "yasnippet")
(defun yw-autoinsert-yas-expand (&rest _)
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(auto-insert-mode 1)
(setq auto-insert-query nil
      auto-insert-directory (locate-user-emacs-file "templates"))

(define-auto-insert "\\.org?$" [ "default-org.org" yw-autoinsert-yas-expand ])
(define-auto-insert "\\.el$" [ "default-el.el" yw-autoinsert-yas-expand ])
(define-auto-insert "\\.sh?$" "default-sh.sh")

(provide '+editor)
;;; +editor.el ends here 
