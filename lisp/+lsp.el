;;; +lsp.el --- lsp -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (lsp-mode) (lsp-ui))

;; This file is not part of GNU Emacs.:

;;; Commentary:
;;; Code:

(use-package lsp-mode
  :diminish lsp-mode
  :commands lsp lsp-deferred
  :init
  (add-hook 'prog-mode-hook (lambda ()
			    (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'sh-mode 'plantuml-mode 'clojure-mode)
			      (lsp-deferred))))
  (add-hook 'lsp-mode-hook (lambda ()
			   (lsp-enable-which-key-integration)
			   (add-hook 'before-save-hook #'lsp-format-buffer t t)
			   (add-hook 'before-save-hook #'lsp-organize-imports t t)))
  :config
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-rename)
  (define-key lsp-mode-map [remap xref-find-definitions] 'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] 'lsp-find-references)
  (if (not (display-graphic-p))
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
            lsp-eldoc-enable-hover t
	    lsp-eldoc-render-all nil
            ;; auto kill server
            lsp-keep-workspace-alive nil)
    (setq lsp-eldoc-render-all nil
	  lsp-eldoc-enable-hover t
	  lsp-idle-delay 0.5
	  lsp-auto-guess-root t)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (if (not (display-graphic-p))
      (setq lsp-ui-doc-mode nil
	    lsp-ui-doc-enable nil
	    lsp-ui-doc-position 'at-point)
    (setq lsp-ui-peek-always-show t 
	  lsp-ui-sideline-show-hover nil
	  lsp-ui-sideline-ignore-duplicate t
	  lsp-ui-doc-position 'top
	  lsp-ui-doc-enable nil))
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(use-package dap-mode
  :config
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
				     :gdbpath "rust-gdb"
                                     :target nil
                                     :cwd nil)))

(provide '+lsp)
;;; +lsp.el ends here
