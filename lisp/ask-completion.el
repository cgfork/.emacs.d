;;; ask-completion.el --- Completion -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Installation
(unless ask-start-emacs-without-packages
  (use-package corfu)
  (unless (display-graphic-p)
    (use-package popon
      :straight (popon
	       :type git
	       :repo "https://codeberg.org/akib/emacs-popon.git"))
    (use-package corfu-popup
       :straight (corfu-popup
	       :type git
	       :repo "https://codeberg.org/akib/emacs-corfu-popup.git")))
  (use-package orderless))

;; Configuration
(add-hook 'after-init-hook #'icomplete-mode)
(add-hook 'icomplete-mode-hook #'icomplete-vertical-mode)
(with-eval-after-load 'icomplete
  (define-key icomplete-vertical-mode-minibuffer-map (kbd "<return>") 'icomplete-force-complete-and-exit)
  (define-key icomplete-vertical-mode-minibuffer-map (kbd "<C-return>") 'icomplete-ret)
  (setq icomplete-matches-format "[%s/%s] "))

(unless ask-start-emacs-without-packages
  (global-corfu-mode)
  (with-eval-after-load 'corfu
    (setq corfu-auto t
	  corfu-cycle t
	  corfu-separator ?\s
	  corfu-quit-no-match t
	  corfu-auto-prefix 3
	  corfu-on-exact-match nil
	  corfu-quit-at-boundary 'separator
	  corfu-echo-documentation nil)
    (unless (display-graphic-p)
      (corfu-popup-mode +1)))

  (defun flex-if-twiddle (pattern _index _total)
    (when (string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))

  (defun first-initialism (pattern index _total)
    (if (= index 0) 'orderless-initialism))

  (defun without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  (with-eval-after-load 'orderless
    (setq completion-styles '(orderless basic)
	  completion-category-defaults nil
	  completion-category-overrides '((buffer (styles . (flex partial-completion)))
					  (file (styles . (basic partial-completion))))
	  orderless-style-dispatchers '(first-initialism
					flex-if-twiddle
					without-if-bang))))

(provide 'ask-completion)
;;; ask-completion.el ends here
