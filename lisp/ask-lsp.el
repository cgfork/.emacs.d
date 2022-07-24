;;; ask-lsp.el --- LSP -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Installation
(unless ask-start-emacs-without-packages
  (use-package eglot))

;; Configuration
(unless ask-start-emacs-without-packages
  (with-eval-after-load 'eglot
    (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c C-a") 'eglot-code-actions)))

(provide 'ask-lsp)
;;; ask-lsp.el ends here
