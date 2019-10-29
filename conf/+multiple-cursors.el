;;; package --- summary
;;; Commentary:
;;; Code:

;; Mutliple cursors is very useful for editing.
;; Github: https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :bind (("M-3" . mc/mark-next-like-this)
	 ("M-4" . mc/mark-previous-like-this)
	 ("M-5" . mc/mark-all-like-this)
	 :map ctl-x-map
	 ("\C-m" . mc/mark-all-dwim)
	 ("<return>" . mule-keymap)))

(provide '+multiple-cursors)
;;; +multiple-cursors.el ends here
