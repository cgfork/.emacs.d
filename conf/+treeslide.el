;;; package ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(cgfork/install 'neotree)

(global-set-key (kbd "<f8>") 'neotree-toggle)

(cgfork/after-load 'neotree
  ;; Must install `all-the-icons' and run `all-the-icons-install-fonts'.
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
	neo-window-width 30
	neo-smart-open t
	neo-window-fixed-size nil))

(provide '+treeslide)
;;; +treeslide.el ends here
