;;; ask-tab-bar.el --- Tab bar Mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; Configuration
(global-set-key (kbd "<C-next>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "<C-prior>") 'tab-bar-switch-to-prev-tab)

(tab-bar-mode 1)

(provide 'ask-tab-bar)
;;; ask-tab-bar.el ends here
