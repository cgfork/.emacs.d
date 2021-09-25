;;; +projectile.el --- projectile. -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: projectile 
;; Package-requires: ((emacs "25.3")  (ibuffer-projectile) (dimmer) (ripgrep))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(use-package projectile
  :hook (after-init . projectile-mode)
  :config
  (setq projectile-enable-caching t
      projectile-sort-order 'recentf
      projectile-compleetion-system 'ivy
      projectile-globally-ignore-file-suffixes
      '(".dir" ".cmake" ".make" ".o" ".elc" ".internal" ".DS_store"))
  (add-to-list 'projectile-globally-ignored-directories "build"))

(use-package ibuffer-projectile)
(use-package dimmer)
(use-package ripgrep)

(provide '+projectile)
;;; +projectile.el ends here  
