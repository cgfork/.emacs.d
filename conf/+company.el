;;; +company.el --- company -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(power-emacs-try 'company)
(diminish 'company-mode)
(add-hook 'after-init-hook 'global-company-mode)
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

(power-emacs-install 'company-prescient)
(company-prescient-mode 1)

(when (display-graphic-p)
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

(provide '+company)
;;; +company.el ends here
