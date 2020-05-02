;;; +programmer.el --- Configuration for programmer -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1

;;; Commentary:
;;; Code:

(when (power-emacs-try 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)
  (setq-default projectile-mode-line-prefix " Proj")
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

(when (power-emacs-try 'company)
  (diminish 'company-mode)
  (add-hook 'after-init-hook 'global-company-mode)
  (power-emacs-install 'company-prescient)
  (company-prescient-mode 1)
  (with-eval-after-load 'company
    (define-key global-map (kbd "M-/") 'company-complete)
    (define-key global-map (kbd "<backtab>") 'company-yasnippet)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-search-map (kbd "C-p") 'company-select-previous)
    (define-key company-search-map (kbd "C-n") 'company-select-next)
    (setq company-idle-delay 0
	  company-minimum-prefix-length 3)))

(power-emacs-install 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit
	flycheck-emacs-lisp-check-declare t
        flycheck-display-errors-delay 0.25
        flycheck-indication-mode 'right-fringe
	flycheck-highlighting-mode 'symbols
        flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(if (display-graphic-p)
    (when (power-emacs-try 'flycheck-posframe :stable nil)
      (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
      (with-eval-after-load 'flycheck-posframe
	(setq flycheck-posframe-warning-prefix "⚠ "
              flycheck-posframe-info-prefix "··· "
              flycheck-posframe-error-prefix "✕ ")
        (with-eval-after-load 'company
          ;; Don't display popups if company is open
          (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))))
  (when (power-emacs-try 'flycheck-popup-tip)
    (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

(power-emacs-install 'lsp-mode)
(diminish 'lsp-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode)
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

(provide '+programmer)
;;; +programmer.el ends here
