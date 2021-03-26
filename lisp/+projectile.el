;;; +projectile.el --- projectile. -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: projectile 
;; Package-requires: ((emacs "25.3")  (ibuffer-projectile) (dimmer))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'projectile)
(add-hook 'after-init-hook 'projectile-mode)
(yw-space-key-define
  "p" '(nil :wk "projectile")
  "p !" 'projectile-run-shell-command-in-root
  "p &" 'projectile-run-async-shell-command-in-root
  "p %" 'projectile-replace-regexp
  "p /" 'projectile-ripgrep
  "p a" 'projectile-toggle-between-implementation-and-test
  "p b" 'projectile-switch-to-buffer
  "p c" 'projectile-compile-project
  "p d" 'projectile-find-dir
  "p D" 'projectile-dired
  "p f" 'projectile-find-file
  "p F" 'projectile-find-file-dwim
  "p g" 'projectile-find-tag
  "p G" 'projectile-regenerate-tags
  "p I" 'projectile-invalidate-cache
  "p k" 'projectile-kill-buffers
  "p p" 'projectile-switch-project
  "p r" 'projectile-recentf
  "p R" 'projectile-replace
  "p T" 'projectile-test-project
  "p v" 'projectile-vc)

(setq projectile-enable-caching t
      projectile-sort-order 'recentf
      projectile-compleetion-system 'ivy
      projectile-globally-ignore-file-suffixes
      '(".dir" ".cmake" ".make" ".o" ".elc" ".internal" ".DS_store"))
(add-to-list 'projectile-globally-ignored-directories "build")

(require 'ibuffer-projectile)

(provide '+projectile)
;;; +projectile.el ends here  
