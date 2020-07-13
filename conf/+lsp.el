;;; +lsp.el --- LSP -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(power-emacs-install 'lsp-mode)
(diminish 'lsp-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'sh-mode 'plantuml-mode 'clojure-mode)
			      (lsp-deferred))))
(add-hook 'lsp-mode-hook (lambda ()
			   (lsp-enable-which-key-integration)
			   (add-hook 'before-save-hook #'lsp-format-buffer t t)
			   (add-hook 'before-save-hook #'lsp-organize-imports t t)))
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-rename)
  (define-key lsp-mode-map [remap xref-find-definitions] 'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] 'lsp-find-references)
  (setq lsp-prefer-flymake nil
	flymake-fringe-indicator-position 'right-fringe
        ;;prefer flycheck
        lsp-diagnostic-package :flycheck
        lsp-auto-guess-root t
        ;;disable file wathcer when large file
        lsp-enable-file-watchers nil
        ;; enable log only for debug
        lsp-log-io nil
        ;; completion
        lsp-prefer-capf  nil
        ;; turn off for better performance
        lsp-enable-symbol-highlighting nil
        ;; Disable eldoc displays in minibuffer
        lsp-eldoc-enable-hover nil
        ;; auto kill server
        lsp-keep-workspace-alive nil))

(power-emacs-install 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(with-eval-after-load 'lsp-ui-mode
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(power-emacs-install 'company-lsp)
(with-eval-after-load 'company-lsp
   (setq company-lsp-async t
	 company-lsp-enable-snippet t
	 company-lsp-cache-candidates 'auto
	 company-lsp-enable-recompletion t))

(provide '+lsp)
;;; +lsp.el ends here
