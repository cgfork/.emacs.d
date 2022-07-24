;;; ask-git.el --- GIt -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:
;; Installation
(unless ask-start-emacs-without-packages
  (use-package magit))

(provide 'ask-git)
;;; ask-git.el ends here
