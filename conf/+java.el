;;; +java.el --- Java LSP -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(power-emacs-install 'lsp-java)
(add-hook 'java-mode-hook 'lsp)

(provide '+java)
;;; +java.el ends here
