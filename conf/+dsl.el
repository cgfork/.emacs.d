;;; +dsl.el --- Config the domain-specific languages -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

;; protobuf
(power-emacs-install 'protobuf-mode)
(add-to-list 'auto-mode-alist (cons "\\.proto\\'" 'protobuf-mode))
(with-eval-after-load 'protobuf-mode
  (add-hook 'protobuf-mode-hook
	    (lambda ()
	      (c-add-style "proto-style" '((c-basic-offset . 4) (indent-tabs-mode . nil)) t))))

(provide '+dsl)
;;; +dsl.el ends here
