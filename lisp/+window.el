;;; +window.el --- windows setup  -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Maintainer: cgfork
;; Version: 1.0.0
;; Keywords: window
;; Package-requires: ((emacs "25.3") (ace-window "0.10.0") (winum "2.2.0") (darkroom))

;;; Commentary:

;;; Code:

(with-eval-after-load 'ace-window
  (global-set-key [remap other-window] #'ace-window))

(with-eval-after-load 'darkroom
  (setq darkroom-margins 0.15
	darkroom-text-scale-increase 0
	darkroom-fringes-outside-margins nil))

(defun yw-toggle-darkroom-mode ()
  (interactive)
  (darkroom-tentative-mode (if darkroom-tentative-mode 0 1))
  (if darkroom-tentative-mode
      (progn
	(add-hook 'window-configuration-change-hook 'darkroom-tentative-mode))
    (remove-hook 'window-configuration-change-hook 'darkroom-tentative-mode)))

(add-hook 'after-init-hook #'winum-mode)
(with-eval-after-load 'winum 
  (yw-space-key-define
    "w" '(nil :wk "window")
    "w 1" 'winum-select-window-1
    "w 2" 'winum-select-window-2
    "w 3" 'winum-select-window-3
    "w 4" 'winum-select-window-4
    "w 5" 'winum-select-window-5
    "w 6" 'winum-select-window-6
    "w 7" 'winum-select-window-7
    "w 8" 'winum-select-window-8
    "w 9" 'winum-select-window-9
    "w d" 'delete-window
    "w D" 'delete-other-window
    "w o" 'other-window
    "w w" 'yw-toggle-darkroom-mode))
(provide '+window)
;;; +window.el ends here
