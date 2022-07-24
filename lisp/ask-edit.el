;;; ask-edit.el --- Edit -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Define

(defun ask-select-text-between (open close)
  "Select the text between the given OPEN and CLOSE regexp."
  (let ( marked )
    (re-search-backward open nil "move") ;; goto the point before open
    (skip-chars-forward open) ;; skip open
    (setq marked (point))
    (re-search-forward close nil "move") ;; goto the point after close
    (skip-chars-backward close) ;; skip close
    (set-mark marked)))

(defun ask-select-text-in-quote ()
  "Select the text between the nearest left and right \" ."
  (interactive)
  (ask-select-text-between "\"" "\""))

(defun ask-select-current-line ()
  "Select the current line."
  (interactive)
  (if (region-active-p)
      (progn
	(forward-line 1)
	(end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

;; Configuration

;; Show paren
(with-eval-after-load 'paren
  (setq show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t))

(when (fboundp 'display-fill-column-indicator)
  (require 'display-fill-column-indicator)
  (defun ask-toggle-fill-column-indicator ()
    "Toggle displaying of fill column indicator."
    (interactive)
    (if display-fill-column-indicator
	(setq display-fill-column-indicator nil)
      (setq display-fill-column-indicator t)))

  (setq-default display-fill-column-indicator-column 120)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

(provide 'ask-edit)
;;; ask-edit.el ends here
