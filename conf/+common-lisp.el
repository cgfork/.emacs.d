;;; +common-lisp.el --- Initialize Common Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(power-emacs-install 'slime)
(setq inferior-lisp-program "sbcl")
(slime-setup)
(cgfork-open-paredit 'slime-mode-hook)

(provide '+common-lisp)
;;; +common-lisp.el ends here
