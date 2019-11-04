;;; package --- Initialize ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ivy - Completion
;; SEEALSO: https://www.reddit.com/r/emacs/comments/6xc0im/ivy_counsel_swiper_company_helm_smex_and_evil/
;; https://github.com/myrjola/diminish.el
(when (cgfork/try-install 'ivy)
  (diminish 'ivy-mode)
  (add-hook 'after-init-hook 'ivy-mode)
  (with-eval-after-load 'ivy
    (setq enable-recursive-minibuffers t
          ivy-use-virtual-buffers t
	  ivy-count-format "%d/%d"
          ivy-display-style 'fancy
          ivy-format-function 'ivy-format-function-line
          ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy) 
				  (t . ivy--regex-plus))
          ivy-initial-inputs-alist nil)
    (define-key global-map (kbd "C-x b") 'ivy-switch-buffer)
    (define-key global-map (kbd "C-c C-r") 'ivy-resume)
    (define-key global-map (kbd "C-c v p") 'ivy-push-view)
    (define-key global-map (kbd "C-c v o") 'ivy-pop-view)
    (define-key global-map (kbd "C-c C .") 'ivy-switch-view)))

(when (cgfork/try-install 'swiper)
  (with-eval-after-load 'swiper
    (define-key global-map (kbd "C-s") 'swiper-isearch)
    (define-key global-map (kbd "C-r") 'swiper-isearch-backward)
    (define-key global-map (kbd "C-s-f") 'swiper)
    (define-key global-map (kbd "C-s-s") 'swiper-all)))

(when (cgfork/try-install 'counsel)
  (diminish 'counsel-mode)
  (add-hook 'after-init-hook 'counsel-mode)
  (with-eval-after-load 'counsel
    (define-key global-map (kbd "M-x") 'counsel-M-x)
    (define-key global-map (kbd "C-x C-f") 'counsel-find-file)))

(provide '+ivy)
;;; +ivy.el ends here
