;;; +programmer.el --- Configuration for programmer -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1

;;; Commentary:
;;; Code:

(power-emacs-try 'company)
(diminish 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)
(power-emacs-install 'company-prescient)
(company-prescient-mode 1)
(with-eval-after-load 'company
  (define-key global-map (kbd "M-/") 'company-complete)
  (define-key global-map (kbd "<backtab>") 'company-yasnippet)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (setq company-idle-delay 0
	company-minimum-prefix-length 3
	company-echo-delay 0
	company-show-numbers t
	company-tooltip-limit 10
	company-selection-wrap-around t
	company-dabbrev-ignore-case t
	company-dabbrev-downcase nil))

(if (display-graphic-p)
    (when (and (power-emacs-try 'company-box :stable nil)
	       (power-emacs-try 'all-the-icons))
      (add-hook 'company-mode-hook #'company-box-mode)
      (with-eval-after-load 'company-box
	(setq company-box-backends-colors nil
	      company-box-show-single-candidate t
	      company-box-max-candidates 50
	      company-box-doc-delay 0.5)
	(require 'all-the-icons nil t)
	(declare-function all-the-icons-faicon 'all-the-icons)
	(declare-function all-the-icons-material 'all-the-icons)
	(declare-function all-the-icons-octicon 'all-the-icons)
	(setq company-box-icons-all-the-icons
	      `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
		(Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
		(Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
		(Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
		(Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face 'all-the-icons-purple))
		(Field . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
		(Variable . ,(all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face 'all-the-icons-lblue))
		(Class . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
		(Interface . ,(all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
		(Module . ,(all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
		(Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
		(Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
		(Value . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
		(Enum . ,(all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
		(Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
		(Snippet . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
		(Color . ,(all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
		(File . ,(all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
		(Reference . ,(all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
		(Folder . ,(all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
		(EnumMember . ,(all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-lblue))
		(Constant . ,(all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
		(Struct . ,(all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face 'all-the-icons-orange))
		(Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
		(Operator . ,(all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
		(TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
		(Template . ,(all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
	      company-box-icons-alist 'company-box-icons-all-the-icons))))

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

(power-emacs-install 'lsp-mode)
(diminish 'lsp-mode)
(add-hook 'prog-mode-hook (lambda ()
			    (unless (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'sh-mode)
			      (lsp-deferred))))
(add-hook 'lsp-mode-hook (lambda ()
			   (lsp-enable-which-key-integration)
			   (add-hook 'before-save-hook #'lsp-format-buffer t t)
			   (add-hook 'before-save-hook #'lsp-organize-imports t t)))
(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-rename)
  (define-key lsp-mode-map [remap xref-find-definitions] 'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] 'lsp-find-references)
  (setq lsp-prefer-flymake nil
	flymake-fringe-indicator-position 'right-fringe
        ;;prefer flycheck
        lsp-diagnostic-package :flycheck
        lsp-auto-guess-root t
        ;;disable file wathcer when large file
        lsp-enable-file-watchers nil
        ;; enable log only for debug
        lsp-log-io nil
        ;; completion
        lsp-prefer-capf  nil
        ;; turn off for better performance
        lsp-enable-symbol-highlighting nil
        ;; Disable eldoc displays in minibuffer
        lsp-eldoc-enable-hover nil
        ;; auto kill server
        lsp-keep-workspace-alive nil))

(power-emacs-install 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(with-eval-after-load 'lsp-ui-mode
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(power-emacs-install 'company-lsp)
(with-eval-after-load 'company-lsp
   (setq company-lsp-async t
	 company-lsp-enable-snippet t
	 company-lsp-cache-candidates 'auto
	 company-lsp-enable-recompletion t))

(provide '+programmer)
;;; +programmer.el ends here
