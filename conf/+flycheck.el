;;; +flycheck.el --- flycheck -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(power-emacs-install 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (setq flycheck-emacs-lisp-load-path 'inherit
	flycheck-emacs-lisp-check-declare t
        flycheck-display-errors-delay 0.25
        flycheck-indication-mode 'right-fringe
	flycheck-highlighting-mode 'symbols
        flycheck-check-syntax-automatically '(save mode-enabled))
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

(if (display-graphic-p)
    (when (power-emacs-try 'flycheck-posframe :stable nil)
      (add-hook 'flycheck-mode-hook 'flycheck-posframe-mode)
      (with-eval-after-load 'flycheck-posframe
	(setq flycheck-posframe-warning-prefix "⚠ "
              flycheck-posframe-info-prefix "··· "
              flycheck-posframe-error-prefix "✕ ")
        (with-eval-after-load 'company
          ;; Don't display popups if company is open
          (add-hook 'flycheck-posframe-inhibit-functions #'company--active-p))))
  (when (power-emacs-try 'flycheck-popup-tip)
    (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)))

(provide '+flycheck)
;;; +flycheck.el ends here
