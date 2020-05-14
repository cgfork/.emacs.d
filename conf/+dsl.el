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

(power-emacs-install 'plantuml-mode)
(setq plantuml-jar-path (executable-find "plantuml.jar"))
(with-eval-after-load 'plantuml-mode
  (plantuml-set-exec-mode "jar"))

(power-emacs-install 'yaml-mode)

(power-emacs-install 'markdown-mode)
(setq markdown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(provide '+dsl)
;;; +dsl.el ends here
