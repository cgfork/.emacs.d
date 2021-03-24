;;; +go.el --- go -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (go-mode))

;; This file is not part of GNU Emacs.:

;;; Commentary:
;;; Code:

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4
				  standard-indent 2
				  indent-tabs-mode nil)))
  (add-hook 'go-mode-hook #'lsp)
  ;; (add-hook 'go-mode-hook #'yas-minor-mode)
  (yw-copy-shell-variables "zsh" "GOPATH" "GO111MODULE" "GOPROXY")
  ;; (power-emacs-set-shell-variable "GO111MODULE" "on")
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (define-key go-mode-map (kbd "C-c i r") 'go-remove-unused-imports)
  (define-key go-mode-map (kbd "C-c i a") 'go-import-add)
  (define-key go-mode-map (kbd "<f1>") 'godoc-at-point)
  (define-key go-mode-map (kbd "C-c t t") 'go-test-current-test)
  (define-key go-mode-map (kbd "C-c t f") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t p") 'go-test-current-project)
  (with-eval-after-load 'gotest (setq go-test-verbose t)))

(provide '+go)

;;; +go.el ends here
