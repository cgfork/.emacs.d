;;; +markdown.el --- markdown configuration -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (markdown-mode))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(with-eval-after-load 'markdown-mode
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\.markdown\\'" . markdown-mode)))

(provide '+markdown)
;;; +markdown.el ends here
