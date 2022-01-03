;;; +builtin.el --- setup builtin mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode 1))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(provide '+builtin)
;;; +builtin.el ends here
