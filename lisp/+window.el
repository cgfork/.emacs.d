;;; +window.el --- windows setup  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: window
;; Package-requires: ((emacs "25.3") (ace-window "0.10.0") (winum "2.2.0") (darkroom))

;;; Commentary:

;;; Code:

(use-package ace-window
  :config
  (global-set-key [remap other-window] #'ace-window))

(use-package darkroom
  :config
  (setq darkroom-margins 0.15
	darkroom-text-scale-increase 0
	darkroom-fringes-outside-margins nil))

(defun ewx-toggle-darkroom-mode ()
  (interactive)
  (darkroom-tentative-mode (if darkroom-tentative-mode 0 1))
  (if darkroom-tentative-mode
      (progn
	(add-hook 'window-configuration-change-hook 'darkroom-tentative-mode))
    (remove-hook 'window-configuration-change-hook 'darkroom-tentative-mode)))

(use-package winum
  :hook (after-init . winum-mode))

(provide '+window)
;;; +window.el ends here
