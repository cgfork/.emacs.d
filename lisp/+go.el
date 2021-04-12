;;; +go.el --- go -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (go-mode) (gotest))

;; This file is not part of GNU Emacs.

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
  (yw-comma-key-define
    "i" '(nil :wk "import")
    "i r" 'go-remove-unused-imports
    "i a" 'go-import-add
    "i g" 'go-goto-imports
    "t" '(nil :wk "test")
    "t b" 'go-test-current-benchmark
    "t t" 'go-test-current-test
    "t f" 'go-test-current-file
    "t p" 'go-test-current-project
    "t r" 'go-run)
  (with-eval-after-load 'gotest (setq go-test-verbose t)))

(provide '+go)

;;; +go.el ends here
