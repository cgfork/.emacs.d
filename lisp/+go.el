;;; +go.el --- go -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (go-mode) (gotest))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'go-mode-hook (lambda ()
			    (setq tab-width 4
				  standard-indent 2
				  indent-tabs-mode nil)))
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest
    :bind (:map go-mode-map
		("C-c C-t" . go-test-current-test)) 
    :config
    (setq go-test-verbose t)))

(provide '+go)

;;; +go.el ends here
