;;; package --- summary
;;; Commentary:
;;; Code:

;; ivy - Completion
;; SEEALSO: https://www.reddit.com/r/emacs/comments/6xc0im/ivy_counsel_swiper_company_helm_smex_and_evil/
;; https://github.com/myrjola/diminish.el
(use-package ivy
  :ensure t
  ;:delight
  :diminish (ivy-mode) ;; hide the ivy-mode lighter from mode line
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
	ivy-count-format "%d/%d"
        ivy-display-style 'fancy
        ivy-format-function 'ivy-format-function-line
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) ; Only counsel-M-x use flx fuzzy search
				(t . ivy--regex-plus))
        ivy-initial-inputs-alist nil))

;; Add C-o quick menu in ivy selection
(use-package ivy-hydra)

;; swiper - show all overview of searches
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch-backward)
	 ("C-s-f" . swiper)
	 ("C-s-s" . swiper-all)
	 ("C-c C-r" . ivy-resume)
	 ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)))

;; counsel - enhanced default common commands
(use-package counsel
  :ensure t
  :diminish (counsel-mode)
  :hook (after-init . counsel-mode)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)))

(provide '+ivy)
;;; +ivy.el ends here
