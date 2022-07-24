;;; ask-markdown.el --- Markdown -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Installation
(unless ask-start-emacs-without-packages
  (use-package markdown-mode
    :config
    (setq markdown-command "multimarkdown")
    (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))))

(provide 'ask-markdown)
;;; ask-markdown.el ends here
