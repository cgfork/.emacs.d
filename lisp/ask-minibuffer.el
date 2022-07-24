;;; ask-minibuffer.el --- Minibuffer -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:


(with-eval-after-load 'isearch
  (setq isearch-lazy-count t
	lazy-count-prefix-format "%s/%s "))

(with-eval-after-load 'grep
  (setq grep-highlight-matches t))

(with-eval-after-load 'minibuffer
  (define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)
  (define-key minibuffer-local-ns-map (kbd "<escape>") 'abort-recursive-edit)
  (define-key minibuffer-local-completion-map (kbd "<escape>") 'abort-recursive-edit)
  (define-key minibuffer-local-must-match-map (kbd "<escape>") 'abort-recursive-edit)
  (define-key minibuffer-local-isearch-map (kbd "<escape>") 'abort-recursive-edit)
  (setq enable-recursive-minibuffers t
	minibuffer-depth-indicate-mode t
	minibuffer-eldef-shorten-default t
	minibuffer-electric-default-mode t
	minibuffer-follows-selected-frame nil
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
	completion-auto-help t
	completion-show-help nil
	completion-cycle-threshold t
	completion-ignore-case t
	completion-styles '(basic substring partial-completion flex)
	completion-category-defaults nil
	completion-category-overrides '((buffer (styles . (flex)))
					(file (styles . (basic partial-completion))))
	completions-format 'one-column
	completions-detailed t))

(provide 'ask-minibuffer)
;;; ask-minibuffer.el ends here
