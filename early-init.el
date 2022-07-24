;;; early-init.el ends here~;; early-init.el --- Configuration before loading packages -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

;; (setq package-enable-at-startup nil)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable scroll bar
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(when (fboundp 'set-language-environment)
  (set-language-environment 'utf-8))

(when (fboundp 'display-time-mode)
  (display-time-mode t))

(when (fboundp 'save-place-mode)
  (save-place-mode t))

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode t))

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(when sys/macp
  (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin20/11.2.0"))

(setq native-comp-deferred-compilation t
      package-native-compile t
      comp-deferred-compilation-deny-list
      '("\\(?:[/\\\\]\\.dir-locals\\.el$\\)"
        ;; Don't native-compile *-authloads.el and *-pkg.el files as they
        ;; seem to produce errors during native-compile.
        "\\(?:[^z-a]*-autoloads\\.el$\\)"
        "\\(?:[^z-a]*-pkg\\.el$\\)"))

(setq-default
 cursor-type 'bar
 ns-pop-up-frames nil
 display-line-numbers t
 tab-stop-list
  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

;; Disable for preventing the package.el loading packages prior to init-file loading.
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Avoid rendering multiple icons simultaneously
(setq inhibit-compacting-font-caches t)

(let ((old gc-cons-threshold))  
  (setq gc-cons-threshold most-positive-fixnum)
  (setq garbage-collection-messages t)
  (add-hook 'after-init-hook (lambda ()
			       (setq gc-cons-threshold old)))
)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((start (current-time)))
     ,@body
     (float-time (time-since start))))

(defvar k-gc-timer
  (run-with-idle-timer 15 t
		       (lambda ()
			 (message "Garbage Collector has run for %.06fsec"
				  (k-time (garbage-collect))))))

(provide 'early-init)
;;; early-init.el ends here
