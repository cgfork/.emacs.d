;;; +editor.el --- Setup your editor -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

;;; Required Packages

(power-emacs-install 'diminish) ;; 隐藏 mode-line 中 minor mode
(power-emacs-install 'paredit)
(power-emacs-install 'highlight-parentheses)
(power-emacs-install 'goto-line-preview)

(require-packages
 :must '((diminish :stable t)
	 (paredit)
	 (highlight-parentheses)))

()
;;; +editor.el ends here
