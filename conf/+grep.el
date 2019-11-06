;;; package --- Initialize Grep -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
	      grep-scroll-output t)

(when sys/macp
  (setq-default locate-command "mdfind"))

(cgfork/install 'wgrep)
(cgfork/after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (and (executable-find "ag")
	   (cgfork/try-install 'ag))
  (cgfork/install 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "C-c C-p") 'ag-project))

(when (and (executable-find "rg")
	   (cgfork/try-install 'rg))
  (cgfork/try-install 'deadgrep)
  (global-set-key (kbd "C-c C-p") 'rg-project))
(provide '+grep)
;;; +grep.el ends here
