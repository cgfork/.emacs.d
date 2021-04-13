;;; +rust.el --- rust -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (rust-mode) (flycheck-rust))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(add-hook 'rust-mode-hook #'lsp-deferred)
(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook (lambda ()
			      (setq tab-width 4
				    standard-indent 2
				    indent-tabs-mode nil)))
  (setq rust-format-on-save t))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide '+rust)
;;; +rust.el ends here
