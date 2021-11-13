;;; +editor.el --- editor setup  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: goto yasnippet
;; Package-requires: ((emacs "25.3") (paredit) (highlight-parentheses) (goto-line-preview) (multiple-cursors) (avy) (yasnippet) (yasnippet-snippets))

;;; Commentary:

;;; Code:

(use-package goto-line-preview)
(use-package multiple-cursors)

(use-package paredit
  :config
  (defmacro ewx-open-paredit (mode-hook)
    "Add the `enable-paredit-mode' to the specific MODE-HOOK."
    `(add-hook ,mode-hook #'enable-paredit-mode))
  (ewx-open-paredit 'eval-expression-minibuffer-setup-hook))


(with-eval-after-load 'elisp-mode
  (ewx-open-paredit 'emacs-lisp-mode-hook))
(add-hook 'after-init-hook #'show-paren-mode)

(use-package highlight-parentheses
  :init
  (global-highlight-parentheses-mode 1)
  (global-set-key [remap goto-line] 'goto-line-preview)
  :config
  (when (fboundp 'display-line-numbers-mode)
    (defun ewx-with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
	(apply f args)))
    (advice-add 'goto-line-preview :around #'ewx-with-display-line-numbers)))

(when (fboundp 'display-fill-column-indicator)
  (require 'display-fill-column-indicator)
  (defun ewx-toggle-fill-column-indicator ()
    "Toggle displaying of fill column indicator."
    (interactive)
    (if display-fill-column-indicator
	(setq display-fill-column-indicator nil)
      (setq display-fill-column-indicator t)))

  (setq-default display-fill-column-indicator-column 120)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

(use-package avy
  :init
  (avy-setup-default))

(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :init
  (autoload 'yas-expand-snippet "yasnippet")
  :config
  (defun ewx-autoinsert-yas-expand (&rest _)
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (auto-insert-mode 1)
  (setq auto-insert-query nil
	auto-insert-directory (locate-user-emacs-file "templates"))

  (define-auto-insert "\\.org?$" [ "default-org.org" ewx-autoinsert-yas-expand ])
  (define-auto-insert "\\.el$" [ "default-el.el" ewx-autoinsert-yas-expand ])
  (define-auto-insert "\\.sh?$" "default-sh.sh"))

(provide '+editor)
;;; +editor.el ends here 
