;;; +company.el --- company -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3") (company) (company-prescient))

;; This file is not part of GNU Emacs.:

;;; Commentary:
;;; Code:

(diminish 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key global-map (kbd "M-/") 'company-complete)
  (define-key global-map (kbd "<backtab>") 'company-yasnippet)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (setq company-idle-delay 0
	company-minimum-prefix-length 3
	company-echo-delay 0
	company-show-numbers t
	company-tooltip-limit 10
	company-selection-wrap-around t
	company-dabbrev-ignore-case t
	company-dabbrev-downcase nil))

(company-prescient-mode 1)

(provide '+company)
;;; +company.el ends here
