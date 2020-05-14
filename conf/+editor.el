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

;; Diminish 
(power-emacs-install 'diminish)

;; Parentheses
(power-emacs-install 'paredit)
(defmacro cgfork-open-paredit (mode-hook)
  "Add the `enable-paredit-mode' to the specific MODE-HOOK."
  `(add-hook ,mode-hook #'enable-paredit-mode))
(cgfork-open-paredit 'eval-expression-minibuffer-setup-hook)
(with-eval-after-load 'elisp-mode
  (cgfork-open-paredit 'emacs-lisp-mode-hook))
(add-hook 'after-init-hook #'show-paren-mode)

;; Parentheses highlight
(power-emacs-install 'highlight-parentheses)
(global-highlight-parentheses-mode 1)

(power-emacs-install 'goto-line-preview)
(global-set-key [remap goto-line] 'goto-line-preview)
(when (fboundp 'display-line-numbers-mode)
   (defun cgfork-with-display-line-numbers (f &rest args)
     (let ((display-line-numbers t))
       (apply f args)))
   (advice-add 'goto-line-preview :around #'cgfork-with-display-line-numbers))

;; Multiple cursors editor
(when (power-emacs-try 'multiple-cursors)
   (define-key global-map (kbd "C->") 'mc/mark-next-like-this)
   (define-key global-map (kbd "C-<") 'mc/mark-previous-like-this)
   (define-key global-map (kbd "C-c C-<") 'mc/mark-all-like-this)
   (define-key global-map (kbd "C-c C-m") 'mc/mark-all-dwim))

;; avy
(power-emacs-try 'avy)
(avy-setup-default)
(define-key global-map (kbd "C-:") 'avy-goto-char-2)

;; Setup yasnippet
;; Or you can use `yas-minor-mode' on per-buffer basic
;; (yas-reload-all)
;;
;; (cgfork/after-load 'yasnippet
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))

;; Setup autoinsert.
(power-emacs-install 'yasnippet)
(power-emacs-install 'yasnippet-snippets)

(diminish 'yas-minor-mode)
(yas-global-mode 1)

(autoload 'yas-expand-snippet "yasnippet")
(defun autoinsert-yas-expand (&rest _)
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

(auto-insert-mode 1) ;; trigger to load the autoinsert package.
(with-eval-after-load 'autoinsert
  (setq auto-insert-query nil
	auto-insert-directory (locate-user-emacs-file "templates"))
  (define-auto-insert "\\.org?$" [ "default-org.org" autoinsert-yas-expand ])
  (define-auto-insert "\\.el?$" [ "default-el.el" autoinsert-yas-expand ])
  (define-auto-insert "\\.sh?$" "default-sh.sh"))

(provide '+editor)
;;; +editor.el ends here
