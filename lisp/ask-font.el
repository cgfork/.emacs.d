;;; ask-font.el --- Font setup -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork(cg.fork@gmail.com)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'subr-x))

(defvar ask-font-mono-fonts
  '("Menlo" "JetBrains Mono" "Droid Sans Mono" "Ubuntu Mono" "Fira Code" "Consolas" "SF Mono" "Monaco"))

(defvar ask-font-unicode-fonts
  '("Symbola" "Apple Symbols" "Symbol"))

(defvar ask-font-chinese-fonts
  '("WenQuanYi Micro Hei" "Microsoft Yahei" "HYKaiTiJ" "Pingfang SC"))

(defun ask-font-resolve-font (fonts)
  "Try resolving the first font in FONTS to be installed by the system."
  (let ((found nil))    
    (catch 'loop
      (dolist (font fonts)
	(when (member font (font-family-list))
	  (setq found font)
	  (throw 'loop t))))
    (cond
     (found found)
     (sys/win32p "Courier New")
     (sys/linuxp "Ubuntu Mono")
     (sys/macp "Monaco")
     (sys/cygwinp "Courier New")
     (t (error "unable to resolve the font")))))

(defun ask-font-display-ppi ()
  "Return the PPI of the display.

The function assumes that the display has the same pixel width in the
horizontal and vertical directions."
  (if (display-graphic-p)
      (let ((mm-h (cl-caddr (assoc 'mm-size (frame-monitor-attributes)))))
	(round (/ (display-pixel-height)
		  (/ mm-h 25.4))))
    (error "Attemp to calculate the dpi of a non-graphic display")))

(defun ask-font-display-auto-height ()
  "Return the font height according the monitor atrributes of the display."
  (if (display-graphic-p)
      (let ((width (nth 3 (assoc 'geometry (frame-monitor-attributes)))))
	(cond
	 ((<= width 1440) 130)
	 ((<= width 1920) 125)
	 (t 150)))
    (error "Attemp to calculate the scale of a non-graphic display")))

(defun ask-font-normalize-font (font)
  "Return FONT as a normalized font spec.

The font will be normalized (i.e. :weight, :slant, and :width will set to
'normal if not specified) before it is converted.
FONT can be a `font-spec', a font object, an XFT font string, or an XLFD font
string."
  ;; (cl-check-type font (or font string vector))
  (when (and (stringp font)
             (string-prefix-p "-" font))
    (setq font (x-decompose-font-name font)))
  (let* ((font
          (cond ((stringp font)
                 (dolist (prop '("weight" "slant" "width") (aref (font-info font) 0))
                   (unless (string-match-p (format ":%s=" prop) font)
                     (setq font (concat font ":" prop "=normal")))))
                ((fontp font)
                 (dolist (prop '(:weight :slant :width) (font-xlfd-name font))
                   (unless (font-get font prop)
                     (font-put font prop 'normal))))
                ((vectorp font)
                 (dolist (i '(1 2 3) (x-compose-font-name font))
                   (unless (aref font i)
                     (aset font i "normal"))))))
         (font (x-resolve-font-name font))
         (font (font-spec :name font)))
    (unless (font-get font :size)
      (font-put font :size
                (font-get (font-spec :name (face-font 'default))
                          :size)))
    font))

(when (display-graphic-p)
  ;; Mono
  (set-face-attribute 'default nil :font (ask-font-resolve-font ask-font-mono-fonts))
  ;; Unicode
  (set-fontset-font t 'unicode (ask-font-resolve-font ask-font-unicode-fonts) nil 'prepend)
  ;; Chinese font
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
		      charset
		      (font-spec :family (ask-font-resolve-font ask-font-chinese-fonts)))))

(provide 'ask-font)
;;; ask-font.el ends here
