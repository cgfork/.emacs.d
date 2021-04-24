;;; +rust.el --- rust -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (rustic) (flycheck-rust) (cargo))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(add-hook 'rustic-mode-hook #'lsp-deferred)
(with-eval-after-load 'rustic
  (add-hook 'rustic-mode-hook (lambda ()
			      (setq tab-width 4
				    standard-indent 2
				    indent-tabs-mode nil)))
  (setq rustic-format-on-save t
	lsp-rust-analyzer-server-display-inlay-hints t))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide '+rust)
;;; +rust.el ends here
