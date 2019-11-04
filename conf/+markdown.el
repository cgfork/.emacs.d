;;; package --- Initialize Markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (cgfork/try-install 'markdown-mode)
  (setq markdown-command "multimarkdown")
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))
(provide '+markdown)
;;; +markdown.el ends here
