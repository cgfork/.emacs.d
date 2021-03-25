;;; +sidebar.el --- filetree -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (neotree))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; Code:

(global-set-key (kbd "<f8>") 'neotree-toggle)
(with-eval-after-load 'neotree
  (setq neo-window-width 30
	neo-smart-open t
	neo-autorefresh t
	neo-window-fixed-size nil
	neo-hidden-regexp-list
	'(
	  "^\\.\\(DS_store\\|git\\|gitignore\\)$"
	  "^\\.\\(pyc\\|o\\|elc\\|lock\\|class\\)$")))
(provide '+sidebar)
;;; +sidebar.el ends here
