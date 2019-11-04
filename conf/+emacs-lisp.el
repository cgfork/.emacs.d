;;; package --- Initialize Emacs lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'elisp-mode
  (cgfork/open-paredit 'emacs-lisp-mode-hook))

(provide '+emacs-lisp)
;;; `(file-name-nondirectory (buffer-file-name))` ends here
