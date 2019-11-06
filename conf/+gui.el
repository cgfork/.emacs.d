;;; package --- Initialize GUI -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(when (cgfork/try-install 'goto-line-preview)
  (global-set-key [remap goto-line] 'goto-line-preview)

  (when (fboundp 'display-line-numbers-mode)
    (defun cgfork/with-display-line-numbers (f &rest args)
      (let ((display-line-numbers t))
        (apply f args)))
    (advice-add 'goto-line-preview :around #'cgfork/with-display-line-numbers)))

(add-hook 'after-init-hook 'show-paren-mode)

(when (fboundp 'set-language-environment)
  (set-language-environment 'utf-8))

(when (fboundp 'display-time-mode)
  (display-time-mode t))

(when (fboundp 'save-place-mode)
  (save-place-mode t))

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t))

(setq-default
 column-number-mode t
 auto-save-default nil
 make-backup-files nil
 cursor-type 'bar
 ns-pop-up-frames nil
 tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(add-hook 'after-init-hook 'winner-mode)

;;;;;;;;;;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;
(cgfork/install 'color-theme-sanityinc-solarized)
(cgfork/install 'color-theme-sanityinc-tomorrow)
(setq custom-safe-themes t)

(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

(defun cgfork/re-apply-themes ()
  "Forcibly load the themes listed in the `custom-enabled-themes'"
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))))

(add-hook 'after-init-hook 'cgfork/re-apply-themes)

(defun cgfork/light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (cgfork/re-apply-themes))

(defun cgfork/dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (cgfork/re-apply-themes))


(when (cgfork/try-install 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode)
  ;; TODO: file upstream as a PR
  (cgfork/after-load 'dimmer
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))))

(provide '+gui)
;;; +gui.el ends here
