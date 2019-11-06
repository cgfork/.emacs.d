;;; package ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(cgfork/install 'neotree)

(global-set-key (kbd "<f8>") 'neotree-toggle)

(cgfork/after-load 'neotree
  ;; Must install `all-the-icons' and run `all-the-icons-install-fonts'.
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(provide '+treeslide)
;;; +treeslide.el ends here
