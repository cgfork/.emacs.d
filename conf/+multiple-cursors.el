;;; package --- summary
;;; Commentary:
;;; Code:

;; Mutliple cursors is very useful for editing.
;; Github: https://github.com/magnars/multiple-cursors.el
(when (cgfork/try-install 'multiple-cursors)
  (with-eval-after-load 'multiple-cursors
    (define-key global-map (kbd "M-3") 'mc/mark-next-like-this)
    (define-key global-map (kbd "M-4") 'mc/mark-previous-like-this)
    (define-key global-map (kbd "M-5") 'mc/mark-all-like-this)
    (define-key ctl-x-map (kbd "C-m") 'mc/mark-all-dwim)
    (define-key ctl-x-map (kbd "<return>") 'mule-keymap)))

(provide '+multiple-cursors)
;;; +multiple-cursors.el ends here
