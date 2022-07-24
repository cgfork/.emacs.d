;;; ask-theme.el --- Ask Dark Theme -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Copy from https://raw.githubusercontent.com/srcery-colors/srcery-emacs/master/srcery-theme.el
;;; Code:

(deftheme ask-dark "Ask Dark Theme")

(defgroup ask-dark nil
  "Ask Dark Theme options."
  :group 'faces)

(defcustom ask-dark-org-height t
  "Use varying text heights for org headings."
  :type 'boolean
  :group 'ask-dark)

(defcustom ask-dark-invert-matches nil
  "Use inverse video for search matches."
  :type 'boolean
  :group 'ask-dark)

(defcustom ask-dark-invert-region t
  "Use inverse video for region."
  :type 'boolean
  :group 'ask-dark)

(defcustom ask-dark-transparent-background nil
  "Sets black background color to nil in terminal."
  :type 'boolean
  :group 'ask-dark)

(let* ((ask-dark-class '((class color) (min-colors 257)))

       (ask-dark-256-class '((class color) (min-colors 89)))
       (ask-dark-black "#1e1e1e")
       (ask-dark-red "#ef2f27")
       (ask-dark-green "#519f50")
       (ask-dark-yellow "#fbb829")
       (ask-dark-blue "#2c78bf")
       (ask-dark-magenta "#E02C6D")
       (ask-dark-cyan "#0AAEB3")
       (ask-dark-white "#BAA67F")
       (ask-dark-orange "#FF5F00")       
       (ask-dark-bright-black "#918175")
       (ask-dark-bright-red "#F75341")
       (ask-dark-bright-green "#98BC37")
       (ask-dark-bright-yellow "#FED06E")
       (ask-dark-bright-blue "#68A8E4")
       (ask-dark-bright-magenta "#FF5C8F")
       (ask-dark-bright-cyan "#2BE4D0")
       (ask-dark-bright-white "#FCE8C3")
       (ask-dark-bright-orange "#FF8700")
       (ask-dark-dark-black "#121212")
       (ask-dark-dark-red "#5f0000")
       (ask-dark-dark-green "#005f00")
       (ask-dark-dark-blue "#00005f")
       (ask-dark-dark-cyan "#005f5f")
      
       (ask-dark-gray-1 "#262626")
       (ask-dark-gray-2 "#303030")
       (ask-dark-gray-3 "#3A3A3A")
       (ask-dark-gray-4 "#444444")
       (ask-dark-gray-5 "#4E4E4E")
       (ask-dark-gray-6 "#585858")
       (ask-dark-teal "#35CDAF")
       (ask-dark-green3"#00d700")
      

       (ask-dark-256-black          "black")
       (ask-dark-256-red            "red")
       (ask-dark-256-green          "green")
       (ask-dark-256-yellow         "yellow")
       (ask-dark-256-blue           "blue")
       (ask-dark-256-magenta        "magenta")
       (ask-dark-256-cyan           "cyan")
       (ask-dark-256-white          "white")
       (ask-dark-256-bright-black   "brightblack")
       (ask-dark-256-bright-red     "brightred")
       (ask-dark-256-bright-green   "brightgreen")
       (ask-dark-256-bright-yellow  "brightyellow")
       (ask-dark-256-bright-blue    "brightblue")
       (ask-dark-256-bright-magenta "brightmagenta")
       (ask-dark-256-bright-cyan    "brightcyan")
       (ask-dark-256-bright-white   "brightwhite")

       (ask-dark-256-orange          "color-202")
       (ask-dark-256-bright-orange   "color-208")
       (ask-dark-256-dark-black      "color-233")
       (ask-dark-256-gray-1          "color-235")
       (ask-dark-256-gray-2          "color-236")
       (ask-dark-256-gray-3          "color-237")
       (ask-dark-256-gray-4          "color-238")
       (ask-dark-256-gray-5          "color-239")
       (ask-dark-256-gray-6          "color-240")

       (ask-dark-256-dark-red       "color-52")
       (ask-dark-256-dark-green     "color-22")
       (ask-dark-256-dark-cyan      "color-23")
       (ask-dark-256-dark-blue      "color-17")

       (ask-dark-256-teal           "color-30")
       (ask-dark-256-green3         "color-40"))

  (custom-theme-set-faces
   'ask-dark

   ;;----------------------------------------------------------------------------
   ;; basics
   ;;----------------------------------------------------------------------------
   `(cursor
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black))))

   `(custom-button
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-bright-white :box (:line-width 2 :style released-button)))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-bright-white :box (:line-width 2 :style released-button)))))

   `(default
      ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-bright-white))
       (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-bright-white))))

   `(default-italic
      ((,ask-dark-class (:italic t))
       (,ask-dark-256-class (:italic t))))

   `(error
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(eval-sexp-fu-flash
     ((,ask-dark-class (:background ,ask-dark-green))
      (,ask-dark-256-class (:background ,ask-dark-256-green))))

   `(eval-sexp-fu-flash-error
     ((,ask-dark-class (:background ,ask-dark-red))
      (,ask-dark-256-class (:background ,ask-dark-256-red))))

   `(font-lock-builtin-face
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(font-lock-comment-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :italic t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :italic t))))

   `(font-lock-constant-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(font-lock-reference-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-blue))))

   `(font-lock-doc-face
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(font-lock-function-name-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(font-lock-keyword-face
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(font-lock-negation-char-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(font-lock-preprocessor-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(font-lock-string-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   `(font-lock-type-face
     ((,ask-dark-class (:foreground ,ask-dark-teal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-teal))))

   `(font-lock-variable-name-face
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(font-lock-warning-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(fringe
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(header-line
     ((,ask-dark-class (:background ,ask-dark-black))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(highlight
     ((,ask-dark-class ,(if ask-dark-invert-matches
                          `(:inverse-video t)
                        `(:background ,ask-dark-gray-5 :weight bold)))
      (,ask-dark-256-class ,(if ask-dark-invert-matches
                              `(:inverse-video t)
                            `(:background ,ask-dark-256-gray-5 :weight bold)))))

   `(hl-line
     ((,ask-dark-class (:background ,ask-dark-gray-2))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2))))

   `(isearch
     ((,ask-dark-class ,(if ask-dark-invert-matches
                          `(:inverse-video t)
                        `(:underline t :background ,ask-dark-gray-5 :weight bold)))
      (,ask-dark-256-class ,(if ask-dark-invert-matches
                              `(:inverse-video t)
                            `(:background ,ask-dark-256-gray-5 :weight bold)))))
   `(isearch-fail
    ((,ask-dark-class (:foreground ,ask-dark-red))
     (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(lazy-highlight
     ((,ask-dark-class ,(if ask-dark-invert-matches
                          `(:inverse-video t)
                        `(:background ,ask-dark-gray-5 :weight bold)))
      (,ask-dark-256-class ,(if ask-dark-invert-matches
                              `(:inverse-video t)
                            `(:background ,ask-dark-256-gray-5 :weight bold)))))

   `(link
     ((,ask-dark-class (:inherit font-lock-comment-face :underline t))
      (,ask-dark-256-class (:inherit font-lock-comment-face :underline t))))

   `(link-visited
     ((,ask-dark-class (:inherit font-lock-comment-face :underline t))
      (,ask-dark-256-class (:inherit font-lock-comment-face :underline t))))

   `(match
     ((,ask-dark-class ,(if ask-dark-invert-matches
                          `(:inverse-video t)
                        `(:background ,ask-dark-gray-5 :weight bold)))
      (,ask-dark-256-class ,(if ask-dark-invert-matches
                              `(:inverse-video t)
                            `(:background ,ask-dark-256-gray-5 :weight bold)))))

   `(minibuffer-prompt
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-yellow))))

   `(page-break-lines
     ((,ask-dark-class (:foreground ,ask-dark-gray-4))
      (,ask-dark-256-class (:foreground ,ask-dark-256-gray-4))))

   `(region
     ((,ask-dark-class ,(if ask-dark-invert-region
                          `(:inverse-video t)
                        `(:background ,ask-dark-gray-3)))
      (,ask-dark-256-class ,(if ask-dark-invert-region
                              `(:inverse-video t)
                            `(:background ,ask-dark-256-gray-3)))))

   `(secondary-selection
     ((,ask-dark-class (:background ,ask-dark-gray-3))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-3))))

   `(success
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(tooltip
     ((,ask-dark-class (:background ,ask-dark-bright-blue :foreground ,ask-dark-bright-white :bold nil :italic nil :underline nil))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-bright-white :bold nil :italic nil :underline nil))))

   `(vertical-border
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(warning
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   `(tool-bar
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))


   ;;----------------------------------------------------------------------------
   ;; ahs
   ;;----------------------------------------------------------------------------
   `(ahs-face
     ((,ask-dark-class (:background ,ask-dark-magenta))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta))))

   `(ahs-plugin-whole-buffer-face
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black))))

   `(ahs-edit-mode-face
     ((,ask-dark-class (:background ,ask-dark-bright-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-red :foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; anzu-mode
   ;;----------------------------------------------------------------------------
   `(anzu-mode-line
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; auto-complete
   ;;----------------------------------------------------------------------------
   `(ac-completion-face
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; avy
   ;;----------------------------------------------------------------------------
   `(avy-lead-face
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-magenta))))

   `(avy-lead-face-0
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-yellow))))

   `(avy-lead-face-1
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-green))))

   `(avy-lead-face-2
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-blue))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-blue))))


   ;;----------------------------------------------------------------------------
   ;; cider
   ;;----------------------------------------------------------------------------
   `(cider-enlightened
     ((,ask-dark-class (:background nil :box (:color ,ask-dark-yellow :line-width -1 :style nil) :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:background nil :box (:color ,ask-dark-256-yellow :line-width -1 :style nil) :foreground ,ask-dark-256-yellow))))

   `(cider-enlightened-face
     ((,ask-dark-class (:background nil :box (:color ,ask-dark-bright-black :line-width -1 :style nil) :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:background nil :box (:color ,ask-dark-256-bright-black :line-width -1 :style nil) :foreground ,ask-dark-256-blue))))

   `(cider-enlightened-local
     ((,ask-dark-class (:foreground ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-yellow))))

   `(cider-instrumented-face
     ((,ask-dark-class (:background nil :box (:color ,ask-dark-red :line-width -1 :style nil) :foreground ,ask-dark-red))
      (,ask-dark-256-class (:background nil :box (:color ,ask-dark-256-red :line-width -1 :style nil) :foreground ,ask-dark-256-red))))

   `(cider-result-overlay-face
     ((,ask-dark-class (:background nil :box (:color ,ask-dark-blue :line-width -1 :style nil) :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:background nil :box (:color ,ask-dark-256-blue :line-width -1 :style nil) :foreground ,ask-dark-256-blue))))

   `(cider-test-error-face
     ((,ask-dark-class (:background ,ask-dark-bright-orange :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-orange :foreground ,ask-dark-256-black))))

   `(cider-test-failure-face
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white))))

   `(cider-test-success-face
     ((,ask-dark-class (:background ,ask-dark-green :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-green :foreground ,ask-dark-256-black))))

   `(cider-traced-face
     ((,ask-dark-class :box (:color ,ask-dark-cyan :line-width -1 :style nil))
      (,ask-dark-256-class :box (:color ,ask-dark-256-cyan :line-width -1 :style nil))))

   `(cider-fringe-good-face
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(cider-fragile-button-face
     ((,ask-dark-class :foreground ,ask-dark-orange :box (:style released-button))
      (,ask-dark-256-class :foreground ,ask-dark-256-orange :box (:style released-button))))

   `(cider-stacktrace-promoted-button-face
     ((,ask-dark-class :foreground ,ask-dark-red :box (:style released-button))
      (,ask-dark-256-class :foreground ,ask-dark-256-red :box (:style released-button))))

   `(cider-stacktrace-suppressed-button-face
     ((,ask-dark-class :foreground ,ask-dark-bright-black :box (:style pressed-button))
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-black :box (:style pressed-button))))

   `(cider-enlightened-local-face
     ((,ask-dark-class :foreground ,ask-dark-yellow :weight bold)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow :weight bold)))

   `(cider-deprecated-face
     ((,ask-dark-class  :foreground ,ask-dark-bright-yellow :underline t)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-yellow :underline t)))

   `(cider-debug-code-overlay-face
     ((,ask-dark-class :background ,ask-dark-bright-blue :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-black)))

   `(cider-docview-table-border-face
     ((,ask-dark-class :foreground ,ask-dark-bright-black)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-black)))

   ;;----------------------------------------------------------------------------
   ;; clojure
   ;;----------------------------------------------------------------------------
   `(clojure-keyword-face
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   ;;----------------------------------------------------------------------------
   ;; company
   ;;----------------------------------------------------------------------------
   `(company-echo-common
     ((,ask-dark-class (:background ,ask-dark-bright-white :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-white :foreground ,ask-dark-256-black))))

   `(company-preview
     ((,ask-dark-class (:background ,ask-dark-dark-black :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black :foreground ,ask-dark-256-bright-white))))

   `(company-preview-common
     ((,ask-dark-class (:background ,ask-dark-dark-black :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black :foreground ,ask-dark-256-bright-white))))

   `(company-preview-search
     ((,ask-dark-class (:inherit match))
      (,ask-dark-256-class (:inherit match))))

   `(company-scrollbar-bg
     ((,ask-dark-class (:background ,ask-dark-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black))))

   `(company-scrollbar-fg
     ((,ask-dark-class (:background ,ask-dark-bright-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-black))))

   `(company-template-field
     ((,ask-dark-class (:inherit region))
      (,ask-dark-256-class (:inherit region))))

   `(company-tooltip
     ((,ask-dark-class (:background ,ask-dark-dark-black :foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black :foreground ,ask-dark-256-bright-black))))

   `(company-tooltip-annotation
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(company-tooltip-common
     ((,ask-dark-class (:background ,ask-dark-dark-black :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black :foreground ,ask-dark-256-bright-white))))

   `(company-tooltip-common-selection
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(company-tooltip-mouse
     ((,ask-dark-class (:inherit highlight))
      (,ask-dark-256-class (:inherit highlight))))

   `(company-tooltip-search
     ((,ask-dark-class (:inherit match))
      (,ask-dark-256-class (:inherit match))))

   `(company-tooltip-selection
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   ;;----------------------------------------------------------------------------
   ;; racer
   ;;----------------------------------------------------------------------------
   `(racer-tooltip
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,ask-dark-256-dark-black))))

   `(racer-help-heading-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; rust
   ;;----------------------------------------------------------------------------
   `(rust-builtin-formatting-macro-face
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(rust-question-mark-face
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight bold))))

   `(rust-string-interpolation-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-green :italic t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green :italic t))))

   `(rust-unsafe-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   ;;----------------------------------------------------------------------------
   ;; diff
   ;;----------------------------------------------------------------------------
   `(diff-added
     ((,ask-dark-class :background nil :foreground ,ask-dark-green)
      (,ask-dark-256-class :background nil :foreground ,ask-dark-256-green)))

   `(diff-changed
     ((,ask-dark-class :background nil :foreground ,ask-dark-red)
      (,ask-dark-256-class :background nil :foreground ,ask-dark-256-red)))

   `(diff-header
     ((,ask-dark-class :background ,ask-dark-gray-2 :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-yellow)))

   `(diff-indicator-added
     ((,ask-dark-class :background nil :foreground ,ask-dark-green)
      (,ask-dark-256-class :background nil :foreground ,ask-dark-256-green)))

   `(diff-indicator-changed
     ((,ask-dark-class :background nil :foreground ,ask-dark-red)
      (,ask-dark-256-class :background nil :foreground ,ask-dark-256-red)))

   `(diff-indicator-removed
     ((,ask-dark-class :background nil :foreground ,ask-dark-red)
      (,ask-dark-256-class :background nil :foreground ,ask-dark-256-red)))

   `(diff-refine-added
     ((,ask-dark-class :background ,ask-dark-green :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-green :foreground ,ask-dark-256-black)))

   `(diff-refine-changed
     ((,ask-dark-class :background ,ask-dark-blue :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-blue :foreground ,ask-dark-256-bright-white)))

   `(diff-refine-removed
     ((,ask-dark-class :background ,ask-dark-red :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white)))

   `(diff-removed
     ((,ask-dark-class :background nil :foreground ,ask-dark-red)
      (,ask-dark-256-class :background nil :foreground ,ask-dark-256-red)))

   ;;----------------------------------------------------------------------------
   ;; diff-hl
   ;;----------------------------------------------------------------------------
   `(diff-hl-change
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(diff-hl-delete
     ((,ask-dark-class :foreground ,ask-dark-red)
      (,ask-dark-256-class :foreground ,ask-dark-256-red)))

   `(diff-hl-insert
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   ;;----------------------------------------------------------------------------
   ;; dired
   ;;----------------------------------------------------------------------------
   `(dired-directory
     ((,ask-dark-class (:foreground ,ask-dark-blue :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(dired-flagged
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(dired-header
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(dired-ignored
     ((,ask-dark-class (:inherit shadow))
      (,ask-dark-256-class (:inherit shadow))))

   `(dired-mark
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(dired-marked
     ((,ask-dark-class (:foreground ,ask-dark-magenta :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :weight bold))))

   `(dired-perm-write
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :underline t))))

   `(dired-symlink
     ((,ask-dark-class (:foreground ,ask-dark-cyan :background ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :weight bold))))

   `(dired-warning
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   ;;----------------------------------------------------------------------------
   ;; Dired Plus
   ;;----------------------------------------------------------------------------
   `(diredp-date-time
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(diredp-number
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(diredp-file-name
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diredp-file-suffix
     ((,ask-dark-class (:foreground ,ask-dark-bright-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-blue))))

   `(diredp-dir-heading
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :underline t))))

   `(diredp-dir-heading
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :underline t))))

   `(diredp-dir-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diredp-read-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-yellow))))

   `(diredp-write-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-red))))

   `(diredp-write-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-red))))

   `(diredp-dir-name
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diredp-exec-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(diredp-symlink
     ((,ask-dark-class (:foreground ,ask-dark-bright-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-cyan))))

   `(diredp-tagged-autofile-name
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,ask-dark-256-magenta))))

   `(diredp-no-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(diredp-flag-mark
     ((,ask-dark-class (:background ,ask-dark-green :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-green :foreground ,ask-dark-256-black))))

   `(diredp-flag-mark-line
     ((,ask-dark-class (:background ,ask-dark-green :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-green :foreground ,ask-dark-256-black))))

   `(diredp-autofile-name
     ((,ask-dark-class (:background ,ask-dark-blue :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-blue :foreground ,ask-dark-256-bright-white))))

   `(diredp-deletion
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white))))

   `(diredp-ignored-file-name
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(diredp-link-priv
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(diredp-mode-line-marked
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(diredp-other-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diredp-rare-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; diredfl
   ;;----------------------------------------------------------------------------
   `(diredfl-autofile-name
     ((,ask-dark-class (:background ,ask-dark-blue))
      (,ask-dark-256-class (:background ,ask-dark-256-blue))))

   `(diredfl-compressed-file-name
     ((,ask-dark-class (:foreground ,ask-dark-green3))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green3))))

   `(diredfl-compressed-file-suffix
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(diredfl-date-time
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(diredfl-dir-heading
     ((,ask-dark-class (:foreground ,ask-dark-teal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-teal))))

   `(diredfl-dir-name
     ((,ask-dark-class (:foreground ,ask-dark-teal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-teal))))

   `(diredfl-dir-priv
     ((,ask-dark-class (:foreground ,ask-dark-teal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-teal))))

   `(diredfl-exec-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green :weight bold))))

   `(diredfl-executable-tag
     ((,ask-dark-class (:foreground ,ask-dark-bright-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green :weight bold))))

   `(diredfl-file-name
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diredfl-file-suffix
     ((,ask-dark-class (:foreground ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-yellow))))

   `(diredfl-flag-mark
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black))))

   `(diredfl-flag-mark-line
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(diredfl-ignored-file-name
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(diredfl-link-priv
     ((,ask-dark-class (:foreground ,ask-dark-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-orange))))

   `(diredfl-no-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(diredfl-number
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(diredfl-other-priv
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(diredfl-rare-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diredfl-read-priv
     ((,ask-dark-class (:foreground ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-yellow))))

   `(diredfl-symlink
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   `(diredfl-tagged-autofile-name
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(diredfl-write-priv
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   ;;----------------------------------------------------------------------------
   ;; ediff
   ;;----------------------------------------------------------------------------
   `(ediff-current-diff-A
     ((,ask-dark-class(:background ,ask-dark-dark-red))
      (,ask-dark-256-class(:background ,ask-dark-256-dark-red))))

   `(ediff-current-diff-Ancestor
     ((,ask-dark-class(:background ,ask-dark-dark-cyan))
      (,ask-dark-256-class(:background ,ask-dark-256-dark-cyan))))

   `(ediff-current-diff-B
     ((,ask-dark-class(:background ,ask-dark-dark-green))
      (,ask-dark-256-class(:background ,ask-dark-256-dark-green))))

   `(ediff-current-diff-C
     ((,ask-dark-class(:background ,ask-dark-dark-blue))
      (,ask-dark-256-class(:background ,ask-dark-256-dark-blue))))

   `(ediff-even-diff-A
     ((,ask-dark-class(:background ,ask-dark-gray-1))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-1))))

   `(ediff-even-diff-Ancestor
     ((,ask-dark-class(:background ,ask-dark-gray-1))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-1))))

   `(ediff-even-diff-B
     ((,ask-dark-class(:background ,ask-dark-gray-1))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-1))))

   `(ediff-even-diff-C
     ((,ask-dark-class(:background ,ask-dark-gray-1))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-1))))

   `(ediff-fine-diff-A
     ((,ask-dark-class(:background ,ask-dark-red :weight bold))
      (,ask-dark-256-class(:background ,ask-dark-256-red :weight bold))))

   `(ediff-fine-diff-Ancestor
     ((,ask-dark-class(:background ,ask-dark-cyan :weight bold))
      (,ask-dark-256-class(:background ,ask-dark-256-cyan :weight bold))))

   `(ediff-fine-diff-B
     ((,ask-dark-class(:background ,ask-dark-green :weight bold))
      (,ask-dark-256-class(:background ,ask-dark-256-green :weight bold))))

   `(ediff-fine-diff-C
     ((,ask-dark-class(:background ,ask-dark-blue :weight bold))
      (,ask-dark-256-class(:background ,ask-dark-256-blue :weight bold))))

   `(ediff-odd-diff-A
     ((,ask-dark-class(:background ,ask-dark-gray-3))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-3))))

   `(ediff-odd-diff-Ancestor
     ((,ask-dark-class(:background ,ask-dark-gray-3))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-3))))

   `(ediff-odd-diff-B
     ((,ask-dark-class(:background ,ask-dark-gray-3))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-3))))

   `(ediff-odd-diff-C
     ((,ask-dark-class(:background ,ask-dark-gray-3))
      (,ask-dark-256-class(:background ,ask-dark-256-gray-3))))

   ;;----------------------------------------------------------------------------
   ;; ein
   ;;----------------------------------------------------------------------------
   `(ein:cell-input-area
     ((,ask-dark-class (:background ,ask-dark-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black))))

   `(ein:cell-input-prompt
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(ein:cell-output-prompt
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(ein:notification-tab-normal
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(ein:notification-tab-selected
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   ;;----------------------------------------------------------------------------
   ;;eldoc
   ;;----------------------------------------------------------------------------
   `(eldoc-highlight-function-argument
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))


   ;;----------------------------------------------------------------------------
   ;; enh-ruby
   ;;----------------------------------------------------------------------------
   `(enh-ruby-string-delimiter-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(enh-ruby-op-face
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; erc
   ;;----------------------------------------------------------------------------
   `(erc-input-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(erc-my-nick-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(erc-nick-default-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(erc-nick-prefix-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(erc-notice-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(erc-prompt-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(erc-timestamp-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))


   ;;----------------------------------------------------------------------------
   ;; eshell
   ;;----------------------------------------------------------------------------
   `(eshell-ls-archive
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(eshell-ls-backup
     ((,ask-dark-class (:inherit font-lock-comment-face))
      (,ask-dark-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-clutter
     ((,ask-dark-class (:inherit font-lock-comment-face))
      (,ask-dark-256-class (:inherit font-lock-comment-face))))

   `(eshell-ls-directory
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(eshell-ls-executable
     ((,ask-dark-class (:foreground ,ask-dark-orange :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-orange :weight bold))))

   `(eshell-ls-missing
     ((,ask-dark-class (:inherit font-lock-warning-face))
      (,ask-dark-256-class (:inherit font-lock-warning-face))))

   `(eshell-ls-product
     ((,ask-dark-class (:inherit font-lock-doc-face))
      (,ask-dark-256-class (:inherit font-lock-doc-face))))

   `(eshell-ls-special
     ((,ask-dark-class (:foreground ,ask-dark-magenta :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :weight bold))))

   `(eshell-ls-symlink
     ((,ask-dark-class (:foreground ,ask-dark-cyan :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan :weight bold))))

   `(eshell-ls-unreadable
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(eshell-prompt
     ((,ask-dark-class (:foreground ,ask-dark-magenta :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; evil
   ;;----------------------------------------------------------------------------
   `(evil-ex-substitute-matches
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white))))

   `(evil-ex-substitute-replacement
     ((,ask-dark-class (:background ,ask-dark-bright-green :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-green :foreground ,ask-dark-256-black))))

   `(evil-search-highlight-persist-highlight-face
     ((,ask-dark-class ,(if ask-dark-invert-matches
                          `(:inverse-video t)
                        `(:background ,ask-dark-gray-5 :weight bold)))
      (,ask-dark-256-class ,(if ask-dark-invert-matches
                              `(:inverse-video t)
                            `(:background ,ask-dark-256-gray-5 :weight bold)))))

   `(flycheck-error
     ((,ask-dark-class (:foreground ,ask-dark-red :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :underline t))))

   `(flycheck-info
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :underline t))))

   `(flycheck-warning
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-bright-orange :underline t))))

   `(flycheck-error-list-checker-name
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(flycheck-fringe-error
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(flycheck-fringe-info
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(flycheck-fringe-warning
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; Flyspell
   ;;----------------------------------------------------------------------------
   `(flyspell-duplicate
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :underline t))))

   `(flyspell-incorrect
     ((,ask-dark-class (:foreground ,ask-dark-red :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :underline t))))

   ;;----------------------------------------------------------------------------
   ;; jabber
   ;;----------------------------------------------------------------------------
   `(jabber-activity-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-red))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-red))))

   `(jabber-activity-personal-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-blue))))

   `(jabber-chat-error
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-red))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-red))))

   `(jabber-chat-prompt-foreign
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-red))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-red))))

   `(jabber-chat-prompt-local
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-blue))))

   `(jabber-chat-prompt-system
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-green))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-green))))

   `(jabber-chat-text-foreign
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(jabber-chat-text-local
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(jabber-rare-time-face
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(jabber-roster-user-away
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(jabber-roster-user-chatty
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-green))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-green))))

   `(jabber-roster-user-dnd
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(jabber-roster-user-error
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(jabber-roster-user-offline
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(jabber-roster-user-online
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-green))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-green))))

   `(jabber-roster-user-xa
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   ;;----------------------------------------------------------------------------
   ;; git
   ;;----------------------------------------------------------------------------
   `(git-commit-summary
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(git-commit-note
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(git-commit-nonempty-second-line
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(diff-file-header
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(diff-hunk-header
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(diff-function
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(diff-header
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; git-gutter-fr
   ;;----------------------------------------------------------------------------
   `(git-gutter-fr:added
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(git-gutter-fr:deleted
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(git-gutter-fr:modified
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight bold))))

   `(git-gutter+-added
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(git-gutter+-deleted
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(git-gutter+-separator
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(git-gutter+-modified
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(git-gutter+-unchanged
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(git-gutter:added
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(git-gutter:modified
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(git-gutter:unchanged
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; git-timemachine
   ;;----------------------------------------------------------------------------
   `(git-timemachine-minibuffer-detail-face
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight bold :background ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight bold :background ,ask-dark-256-blue))))


   ;;----------------------------------------------------------------------------
   ;; gnus
   ;;----------------------------------------------------------------------------
   `(gnus-emphasis-highlight-words
     ((,ask-dark-class (:background ,ask-dark-green :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-green :foreground ,ask-dark-256-black))))

   `(gnus-header-content
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(gnus-header-from
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(gnus-header-name
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(gnus-header-subject
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(gnus-summary-cancelled
     ((,ask-dark-class (:background ,ask-dark-bright-orange :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-orange :foreground ,ask-dark-256-black))))

   ;;----------------------------------------------------------------------------
   ;; guide-key
   ;;----------------------------------------------------------------------------
   `(guide-key/highlight-command-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(guide-key/key-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(guide-key/prefix-command-face
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; helm
   ;;----------------------------------------------------------------------------
   `(helm-bookmark-directory
     ((,ask-dark-class (:inherit helm-ff-directory))
      (,ask-dark-256-class (:inherit helm-ff-directory))))

   `(helm-bookmark-file
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(helm-bookmark-gnus
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(helm-bookmark-info
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(helm-bookmark-man
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(helm-bookmark-w3m
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(helm-buffer-directory
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(helm-buffer-file
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-buffer-not-saved
     ((,ask-dark-class (:foreground ,ask-dark-green :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-buffer-process
     ((,ask-dark-class (:foreground ,ask-dark-red :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-buffer-saved-out
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-buffer-size
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-candidate-number
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-red :weight bold))))

   `(helm-ff-directory
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(helm-ff-dotted-directory
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(helm-ff-dotted-symlink-directory
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(helm-ff-executable
     ((,ask-dark-class (:foreground ,ask-dark-green :background ,ask-dark-black :weight normal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :weight normal))))

   `(helm-ff-file
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black :weight normal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :weight normal))))

   `(helm-ff-invalid-symlink
     ((,ask-dark-class (:foreground ,ask-dark-red :background ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :weight bold))))

   `(helm-ff-prefix
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-red :weight normal))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-red :weight normal))))

   `(helm-ff-symlink
     ((,ask-dark-class (:foreground ,ask-dark-cyan :background ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :weight bold))))

   `(helm-grep-cmd-line
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-grep-file
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-grep-finish
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-grep-lineno
     ((,ask-dark-class (:foreground ,ask-dark-bright-blue :background ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-blue :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :weight bold))))

   `(helm-grep-match
     ((,ask-dark-class (:foreground nil :background nil :inherit helm-match))
      (,ask-dark-256-class (:foreground nil :background nil :inherit helm-match))))

   `(helm-header
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black :underline nil :box nil))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black) :underline nil :box nil))))

   `(helm-header-line-left-margin
     ((,ask-dark-class (:foreground ,ask-dark-red :background ,nil))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :background ,nil))))

   `(helm-match
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(helm-match-item
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(helm-moccur-buffer
     ((,ask-dark-class (:foreground ,ask-dark-blue :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-selection
     ((,ask-dark-class (:background ,ask-dark-gray-2 :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :weight bold))))

   `(helm-selection-line
     ((,ask-dark-class (:background ,ask-dark-gray-2 :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :weight bold))))

   `(helm-separator
     ((,ask-dark-class (:foreground ,ask-dark-green :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-source-header
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-green :underline t))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-green :underline t))))

   `(helm-time-zone-current
     ((,ask-dark-class (:foreground ,ask-dark-red :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-time-zone-home
     ((,ask-dark-class (:foreground ,ask-dark-green :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-visible-mark
     ((,ask-dark-class (:foreground ,ask-dark-red :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))


   ;;----------------------------------------------------------------------------
   ;; helm-swoop
   ;;----------------------------------------------------------------------------
   `(helm-swoop-target-line-block-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(helm-swoop-target-line-face
     ((,ask-dark-class (:background ,ask-dark-gray-2 :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :weight bold))))

   `(helm-swoop-target-word-face
     ((,ask-dark-class (:foreground ,ask-dark-magenta :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :underline t))))

   ;;----------------------------------------------------------------------------
   ;; highlights
   ;;----------------------------------------------------------------------------
   `(hi-yellow
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(hi-green
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   ;;----------------------------------------------------------------------------
   ;; highlight-indentation
   ;;----------------------------------------------------------------------------
   `(highlight-indentation-face
     ((,ask-dark-class (:background ,ask-dark-bright-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-black))))

   ;;----------------------------------------------------------------------------
   ;; highlight-symbol
   ;;----------------------------------------------------------------------------
   `(highlight-symbol-face
     ((,ask-dark-class (:background ,ask-dark-gray-2))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2))))

   ;;----------------------------------------------------------------------------
   ;; hydra
   ;;----------------------------------------------------------------------------
   `(hydra-face-blue
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(hydra-face-red
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   ;;----------------------------------------------------------------------------
   ;; ido
   ;;----------------------------------------------------------------------------
   `(ido-first-match
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(ido-only-match
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(ido-subdir
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(ido-indicator
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white))))

   `(ido-vertical-match-face
     ((,ask-dark-class (:foreground ,ask-dark-green :underline nil))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :underline nil))))

   ;;----------------------------------------------------------------------------
   ;; info
   ;;----------------------------------------------------------------------------
   `(info-header-xref
     ((,ask-dark-class (:foreground ,ask-dark-yellow :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :underline t))))

   `(info-menu
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(info-node
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(info-quoted-name
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(info-reference-item
     ((,ask-dark-class (:background nil :underline t :weight bold))
      (,ask-dark-256-class (:background nil :underline t :weight bold))))

   `(info-string
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(info-title-1
     ((,ask-dark-class (:height 1.4 :weight bold))
      (,ask-dark-256-class (:height 1.4 :weight bold))))

   `(info-title-2
     ((,ask-dark-class (:height 1.3 :weight bold))
      (,ask-dark-256-class (:height 1.3 :weight bold))))

   `(info-title-3
     ((,ask-dark-class (:height 1.3))
      (,ask-dark-256-class (:height 1.3))))

   `(info-title-4
     ((,ask-dark-class (:height 1.2))
      (,ask-dark-256-class (:height 1.2))))

   ;;----------------------------------------------------------------------------
   ;; ivy
   ;;----------------------------------------------------------------------------
   `(ivy-current-match
     ((,ask-dark-class (:background ,ask-dark-gray-2 :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :weight bold))))

   `(ivy-minibuffer-match-face-1
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(ivy-minibuffer-match-face-2
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(ivy-minibuffer-match-face-3
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(ivy-minibuffer-match-face-4
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(ivy-remote
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(ivy-highlight-face
     ((,ask-dark-class (:underline t))
      (,ask-dark-256-class (:underline t))))

   ;;----------------------------------------------------------------------------
   ;; latex
   ;;----------------------------------------------------------------------------
   `(font-latex-bold-face
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(font-latex-italic-face
     ((,ask-dark-class (:foreground ,ask-dark-red :italic t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :italic t))))

   `(font-latex-match-reference-keywords
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(font-latex-match-variable-keywords
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(font-latex-sectioning-0-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-bright-green :height ,(if ask-dark-org-height 1.3 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-bright-green :height ,(if ask-dark-org-height 1.3 1.0)))))

   `(font-latex-sectioning-1-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-bright-yellow :height ,(if ask-dark-org-height 1.3 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-bright-yellow :height ,(if ask-dark-org-height 1.3 1.0)))))

   `(font-latex-sectioning-2-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-blue :height ,(if ask-dark-org-height 1.3 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-blue :height ,(if ask-dark-org-height 1.3 1.0)))))

   `(font-latex-sectioning-3-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-cyan :height ,(if ask-dark-org-height 1.2 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-cyan :height ,(if ask-dark-org-height 1.2 1.0)))))

   `(font-latex-sectioning-4-face
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-bright-green :height ,(if ask-dark-org-height 1.1 1.0)))
      (,ask-dark-class (:bold nil :foreground ,ask-dark-256-bright-green :height ,(if ask-dark-org-height 1.1 1.0)))))

   `(font-latex-sectioning-5-face
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-yellow))))

   `(font-latex-string-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   ;;----------------------------------------------------------------------------
   ;; Line numbers
   ;;----------------------------------------------------------------------------
   `(linum
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(linum-relative-current-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(nlinum-current-line
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(nlinum-relative-current-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(line-number
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(line-number-current-line
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; Git
   ;;----------------------------------------------------------------------------
   `(diff-context
     ((,ask-dark-class :foreground ,ask-dark-bright-black)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-black)))

   ;;----------------------------------------------------------------------------
   ;; magit
   ;;----------------------------------------------------------------------------
   `(magit-blame-culprit
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(magit-blame-header
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(magit-blame-sha1
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(magit-blame-subject
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(magit-blame-time
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(magit-blame-name
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(magit-blame-heading
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(magit-blame-hash
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(magit-blame-summary
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(magit-blame-date
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(magit-log-date
     ((,ask-dark-class :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white)))

   `(magit-log-graph
     ((,ask-dark-class :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white)))

   `(magit-reflog-amend
     ((,ask-dark-class :foreground ,ask-dark-magenta)
      (,ask-dark-256-class :foreground ,ask-dark-256-magenta)))

   `(magit-reflog-other
     ((,ask-dark-class :foreground ,ask-dark-cyan)
      (,ask-dark-256-class :foreground ,ask-dark-256-cyan)))

   `(magit-reflog-rebase
     ((,ask-dark-class :foreground ,ask-dark-magenta)
      (,ask-dark-256-class :foreground ,ask-dark-256-magenta)))

   `(magit-reflog-remote
     ((,ask-dark-class :foreground ,ask-dark-cyan)
      (,ask-dark-256-class :foreground ,ask-dark-256-cyan)))

   `(magit-reflog-reset
     ((,ask-dark-class :foreground ,ask-dark-red)
      (,ask-dark-256-class :foreground ,ask-dark-256-red)))

   `(magit-branch
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta :weight bold))))

   `(magit-branch-current
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-blue :weight bold :box t))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-blue :weight bold :box t))))

   `(magit-branch-local
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-blue :weight bold))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-blue :weight bold))))

   `(magit-branch-remote
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-orange :weight bold))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-orange :weight bold))))

   `(magit-diff-file-header
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(magit-diff-file-heading
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight light))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight light))))

   `(magit-diff-file-heading-highlight
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight bold))))

   `(magit-diff-file-heading-selection
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight bold :background ,ask-dark-gray-2))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight bold :background ,ask-dark-256-gray-2))))


   `(magit-diff-hunk-heading
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight light))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight light))))

   `(magit-diff-hunk-heading-highlight
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(magit-diff-hunk-heading-selection
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-black :weight bold))))


   `(magit-diff-added
     ((,ask-dark-class (:foreground ,ask-dark-green :weight light))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight light))))

   `(magit-diff-removed
     ((,ask-dark-class (:foreground ,ask-dark-red :weight light))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight light))))

   `(magit-diff-context
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :weight light))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :weight light))))

   `(magit-diff-added-highlight
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(magit-diff-removed-highlight
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(magit-diff-context-highlight
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :weight bold))))

   `(magit-diff-base
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :weight light))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :weight light))))

   `(magit-diff-base-highlight
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :weight bold))))

   `(magit-diff-lines-boundary
     ((,ask-dark-class (:background ,ask-dark-bright-black :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-black :foreground ,ask-dark-256-black))))

   `(magit-diff-lines-heading
     ((,ask-dark-class (:background ,ask-dark-bright-black :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-black :foreground ,ask-dark-256-black))))

   `(magit-hash
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(magit-item-highlight
     ((,ask-dark-class :background ,ask-dark-gray-2)
      (,ask-dark-256-class :background ,ask-dark-256-gray-2)))

   `(magit-log-author
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(magit-log-head-label-head
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black :weight bold))))

   `(magit-log-head-label-local
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-black :weight bold))))

   `(magit-log-head-label-remote
     ((,ask-dark-class (:background ,ask-dark-green :foreground ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-green :foreground ,ask-dark-256-black :weight bold))))

   `(magit-log-head-label-tags
     ((,ask-dark-class (:background ,ask-dark-magenta :foreground ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta :foreground ,ask-dark-256-black :weight bold))))

   `(magit-log-head-label-wip
     ((,ask-dark-class (:background ,ask-dark-cyan :foreground ,ask-dark-black :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-256-cyan :foreground ,ask-dark-256-black :weight bold))))

   `(magit-log-sha1
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(magit-process-ng
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange :weight bold))))

   `(magit-process-ok
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(magit-section-heading
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(magit-section-highlight
     ((,ask-dark-class (:weight bold))
      (,ask-dark-256-class (:weight bold))))

   `(section-heading-selection
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(magit-section-title
     ((,ask-dark-class (:background ,ask-dark-black :foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:background ,(if ask-dark-transparent-background nil ask-dark-256-black) :foreground ,ask-dark-256-red :weight bold))))

   `(magit-cherry-equivalent
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(magit-cherry-unmatched
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(magit-reflog-checkout
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(magit-reflog-cherry-pick
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(magit-bisect-bad
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(magit-bisect-good
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(magit-bisect-skip
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(magit-diff-conflict-heading
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(magit-dimmed
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(magithub-ci-no-status
     ((,ask-dark-class (:foreground ,ask-dark-gray-6))
      (,ask-dark-256-class (:foreground ,ask-dark-256-gray-6))))

   `(magithub-issue-number
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(magithub-notification-reason
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   ;;----------------------------------------------------------------------------
   ;; smerge
   ;;----------------------------------------------------------------------------
   `(smerge-base
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(smerge-markers
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(smerge-mine
     ((,ask-dark-class (:foreground nil))
      (,ask-dark-256-class (:foreground ,nil))))

   `(smerge-other
     ((,ask-dark-class (:background ,ask-dark-gray-2))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2))))

   `(smerge-refined-added
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(smerge-refined-changed
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(smerge-refined-removed
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(smerge-upper
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(smerge-lower
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   ;;----------------------------------------------------------------------------
   ;; man
   ;;----------------------------------------------------------------------------
   `(Man-overstrike
     ((,ask-dark-class (:foreground ,ask-dark-blue :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :weight bold))))

   `(Man-reverse
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(Man-underline
     ((,ask-dark-class (:foreground ,ask-dark-green :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :underline t))))


   ;;----------------------------------------------------------------------------
   ;; markdown
   ;;----------------------------------------------------------------------------
   `(markdown-header-face-1
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-blue :height ,(if ask-dark-org-height 1.3 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-blue :height ,(if ask-dark-org-height 1.3 1.0)))))

   `(markdown-header-face-2
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-bright-cyan :height ,(if ask-dark-org-height 1.2 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-bright-cyan :height ,(if ask-dark-org-height 1.2 1.0)))))

   `(markdown-header-face-3
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-bright-green :height ,(if ask-dark-org-height 1.1 1.0)))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-bright-green :height ,(if ask-dark-org-height 1.1 1.0)))))

   `(markdown-header-face-4
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-yellow))))

   `(markdown-header-face-5
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-blue))))

   `(markdown-header-face-6
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-cyan))))

   `(markdown-html-tag-delimiter-face
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-gray-6))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-gray-6))))

   `(markdown-list-face
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-gray-6))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-gray-6))))

   `(markdown-markup-face
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-gray-6))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-gray-6))))

   ;;----------------------------------------------------------------------------
   ;; mu4e
   ;;----------------------------------------------------------------------------
   `(mu4e-cited-1-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(mu4e-cited-7-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(mu4e-header-marks-face
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(mu4e-header-key-face
     ((,ask-dark-class (:foreground ,ask-dark-cyan :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan :weight bold))))

   `(mu4e-view-url-number-face
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(mu4e-unread-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; neotree
   ;;----------------------------------------------------------------------------
   `(neo-dir-link-face
     ((,ask-dark-class (:foreground ,ask-dark-red :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :weight bold))))

   `(neo-expand-btn-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(neo-file-link-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(neo-root-dir-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   ;;----------------------------------------------------------------------------
   ;; org
   ;;----------------------------------------------------------------------------
   `(org-agenda-clocking
     ((,ask-dark-class (:background ,ask-dark-magenta :foreground ,ask-dark-green))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta :foreground ,ask-dark-256-green))))

   `(org-agenda-date
     ((,ask-dark-class (:foreground ,ask-dark-blue :height ,(if ask-dark-org-height 1.1 1.0)))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue :height ,(if ask-dark-org-height 1.1 1.0)))))

   `(org-agenda-date-today
     ((,ask-dark-class (:foreground ,ask-dark-red :slant italic :weight bold :height ,(if ask-dark-org-height 1.3 1.0)))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red :slant italic :weight bold :height ,(if ask-dark-org-height 1.3 1.0)))))

   `(org-agenda-date-weekend
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-blue))))

   `(org-agenda-done
     ((,ask-dark-class (:foreground ,ask-dark-green :height ,(if ask-dark-org-height 1.2 1.0)))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :height ,(if ask-dark-org-height 1.2 1.0)))))

   `(org-agenda-structure
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-green))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-green))))

   `(org-block
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(org-block-begin-line
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(org-block-end-line
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(org-clock-overlay
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(org-code
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(org-column
     ((,ask-dark-class (:background ,ask-dark-magenta))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta))))

   `(org-column-title
     ((,ask-dark-class (:background ,ask-dark-magenta))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta))))

   `(org-date
     ((,ask-dark-class (:underline t :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:underline t :foreground ,ask-dark-256-blue))))

   `(org-date-selected
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black))))

   `(org-document-info-keyword
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(org-document-info
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(org-document-title
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold :height ,(if ask-dark-org-height 1.4 1.0)))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold :height ,(if ask-dark-org-height 1.4 1.0)))))

   `(org-done
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(org-ellipsis
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(org-footnote
     ((,ask-dark-class (:underline t :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:underline t :foreground ,ask-dark-256-bright-white))))

   `(org-hide
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(org-kbd
     ((,ask-dark-class (:inherit region :foreground ,ask-dark-bright-white :box (:line-width 1 :style released-button)))
      (,ask-dark-256-class (:inherit region :foreground ,ask-dark-256-bright-white :box (:line-width 1 :style released-button)))))

   `(org-level-1
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-bright-blue :height ,(if ask-dark-org-height 1.3 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-bright-blue :height ,(if ask-dark-org-height 1.3 1.0)))))

   `(org-level-2
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-green :height ,(if ask-dark-org-height 1.2 1.0)))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-green :height ,(if ask-dark-org-height 1.2 1.0)))))

   `(org-level-3
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-yellow :height ,(if ask-dark-org-height 1.1 1.0)))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-yellow :height ,(if ask-dark-org-height 1.1 1.0)))))

   `(org-level-4
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-blue))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-blue))))

   `(org-level-5
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-cyan))))

   `(org-level-6
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-green))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-green))))

   `(org-level-7
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-orange))))

   `(org-level-8
     ((,ask-dark-class (:bold nil :foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:bold nil :foreground ,ask-dark-256-bright-magenta))))

   `(org-link
     ((,ask-dark-class (:foreground ,ask-dark-bright-black :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black :underline t))))

   `(org-meta-line
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(org-mode-line-clock-overrun
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(org-mode-line-clock
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(org-priority
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange :weight bold))))

   `(org-quote
     ((,ask-dark-class (:inherit org-block :slant italic))
      (,ask-dark-256-class (:inherit org-block :slant italic))))

   `(org-scheduled
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(org-scheduled-today
     ((,ask-dark-class (:foreground ,ask-dark-yellow :height ,(if ask-dark-org-height 1.2 1.0)))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :height ,(if ask-dark-org-height 1.2 1.0)))))

   `(org-sexp-date
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(org-special-keyword
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(org-drawer
      ((,ask-dark-class (:foreground ,ask-dark-yellow))
       (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(org-table
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-gray-1))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,ask-dark-256-gray-1))))

   `(org-time-grid
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(org-todo
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(org-verbatim
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   `(org-verse
     ((,ask-dark-class (:inherit org-block :slant italic))
      (,ask-dark-256-class (:inherit org-block :slant italic))))

   `(org-warning
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(org-archived
    ((,ask-dark-class (:foreground ,ask-dark-gray-5))
     (,ask-dark-256-class (:foreground ,ask-dark-256-gray-5))))


   ;;----------------------------------------------------------------------------
   ;; perspective
   ;;----------------------------------------------------------------------------
   `(persp-selected-face
     ((,ask-dark-class (:weight bold :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; popup
   ;;----------------------------------------------------------------------------
   `(popup-face
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-white))))

   `(popup-tip-face
     ((,ask-dark-class (:background ,ask-dark-bright-blue :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-black))))

   `(popup-menu-face
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-white))))

   `(popup-menu-selection-face
     ((,ask-dark-class (:background ,ask-dark-bright-blue :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-black))))

   `(popup-menu-mouse-face
     ((,ask-dark-class (:inherit highlight))
      (,ask-dark-256-class (:inherit highlight))))

   `(popup-isearch-match
     ((,ask-dark-class (:inherit match))
      (,ask-dark-256-class (:inherit match))))

   `(popup-scroll-bar-foreground-face
     ((,ask-dark-class (:background ,ask-dark-gray-5))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-5))))

   `(popup-scroll-bar-background-face
     ((,ask-dark-class (:background ,ask-dark-gray-2))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2))))


   ;;----------------------------------------------------------------------------
   ;; mode-line
   ;;----------------------------------------------------------------------------
   `(powerline-active1
     ((,ask-dark-class (:background ,ask-dark-gray-4 :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-4 :foreground ,ask-dark-256-bright-white))))

   `(powerline-active2
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-gray-2))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,ask-dark-256-gray-2))))

   `(powerline-inactive1
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-black))))

   `(powerline-inactive2
     ((,ask-dark-class (:background ,ask-dark-gray-2 :foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :foreground ,ask-dark-256-bright-black))))

   `(mode-line
     ((,ask-dark-class (:foreground ,ask-dark-white :background ,ask-dark-gray-2))
      (,ask-dark-256-class (:foreground ,ask-dark-256-white :background ,ask-dark-256-gray-2))))

   `(mode-line-inactive
     ((,ask-dark-class (:foreground ,ask-dark-gray-6 :background ,ask-dark-gray-2))
      (,ask-dark-256-class (:foreground ,ask-dark-256-gray-6 :background ,ask-dark-256-gray-2))))

   `(mode-line-buffer-id
     ((,ask-dark-class (:weight bold))
      (,ask-dark-256-class (:weight bold))))

   `(mode-line-highlight
     ((,ask-dark-class (:background ,ask-dark-gray-2 :box (:color ,ask-dark-magenta :line-width 1)))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-2 :box (:color ,ask-dark-256-magenta :line-width 1)))))

   `(mode-line-buffer-id-inactive
     ((,ask-dark-class (:weight bold))
      (,ask-dark-256-class (:weight bold))))

   `(magit-mode-line-process
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   ;; `(mode-line-emphasis
   ;;   ((,ask-dark-class (:weight bold :foreground ,ask-dark-yellow))
   ;;    (,ask-dark-256-class (:weight bold :foreground ,ask-dark-256-yellow))))

   `(spaceline-python-venv
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(spaceline-flycheck-error
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(spaceline-flycheck-info
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(spaceline-flycheck-warning
     ((,ask-dark-class (:foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-orange))))

   `(spaceline-evil-normal
     ((,ask-dark-class (:background ,ask-dark-gray-5 :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-5 :foreground ,ask-dark-256-bright-white))))

   `(spaceline-evil-insert
     ((,ask-dark-class (:background ,ask-dark-bright-white :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-white :foreground ,ask-dark-256-black))))

   `(spaceline-evil-replace
     ((,ask-dark-class (:background ,ask-dark-bright-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-red :foreground ,ask-dark-256-bright-white))))

   `(spaceline-evil-visual
     ((,ask-dark-class (:background ,ask-dark-cyan :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-cyan :foreground ,ask-dark-256-black))))

   `(spaceline-evil-motion
     ((,ask-dark-class (:background ,ask-dark-bright-magenta :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-magenta :foreground ,ask-dark-256-black))))

   `(spaceline-evil-emacs
     ((,ask-dark-class (:background ,ask-dark-orange :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-orange :foreground ,ask-dark-256-bright-white))))

   `(spaceline-unmodified
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(spaceline-modified
     ((,ask-dark-class (:background ,ask-dark-bright-orange :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-orange :foreground ,ask-dark-256-black))))

   `(spaceline-read-only
     ((,ask-dark-class (:background ,ask-dark-dark-black :foreground ,ask-dark-orange))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black :foreground ,ask-dark-256-orange))))

   `(spaceline-highlight-face
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black))))


   ;;----------------------------------------------------------------------------
   ;; rainbow-delimiters
   ;;----------------------------------------------------------------------------
   `(rainbow-delimiters-depth-1-face
     ((,ask-dark-class :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white)))

   `(rainbow-delimiters-depth-2-face
     ((,ask-dark-class :foreground ,ask-dark-bright-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-blue)))

   `(rainbow-delimiters-depth-3-face
     ((,ask-dark-class :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white)))

   `(rainbow-delimiters-depth-4-face
     ((,ask-dark-class :foreground ,ask-dark-bright-cyan)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-cyan)))

   `(rainbow-delimiters-depth-5-face
     ((,ask-dark-class :foreground ,ask-dark-bright-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-green)))

   `(rainbow-delimiters-depth-6-face
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(rainbow-delimiters-depth-7-face
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(rainbow-delimiters-depth-8-face
     ((,ask-dark-class :foreground ,ask-dark-yellow)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow)))

   `(rainbow-delimiters-unmatched-face
     ((,ask-dark-class :foreground ,ask-dark-red)
      (,ask-dark-256-class :foreground ,ask-dark-256-red)))

   `(rainbow-delimiters-mismatched-face
     ((,ask-dark-class :foreground ,ask-dark-bright-red)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-red)))

   ;; `(rainbow-delimiters-unmatched-face
   ;;   ((,ask-dark-class :foreground ,ask-dark-red :overline t :inhert bold)
   ;;    (,ask-dark-256-class :foreground ,ask-dark-256-red :overline t :inhert bold)))

   ;; `(rainbow-delimiters-mismatched-face
   ;;   ((,ask-dark-class :foreground ,ask-dark-red :overline t :weight bold)
   ;;    (,ask-dark-256-class :foreground ,ask-dark-256-red :overline t :weight bold)))


   ;;----------------------------------------------------------------------------
   ;; sh
   ;;----------------------------------------------------------------------------
   `(sh-heredoc
     ((,ask-dark-class (:foreground ,ask-dark-green :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :weight bold))))

   `(sh-quoted-exec
     ((,ask-dark-class (:foreground ,ask-dark-orange))
      (,ask-dark-256-class (:foreground ,ask-dark-256-orange))))

   ;;----------------------------------------------------------------------------
   ;; shm
   ;;----------------------------------------------------------------------------
   `(shm-current-face
     ((,ask-dark-class (:background ,ask-dark-green, :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-green, :foreground ,ask-dark-256-black))))

   `(shm-quarantine-face
     ((,ask-dark-class (:background ,ask-dark-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-dark-black))))


   ;;----------------------------------------------------------------------------
   ;; show-paren
   ;;----------------------------------------------------------------------------
   `(show-paren-match
     ((,ask-dark-class (:background ,ask-dark-magenta :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta :foreground ,ask-dark-256-bright-white))))

   `(show-paren-mismatch
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white))))


   ;;----------------------------------------------------------------------------
   ;; paren-face
   ;;----------------------------------------------------------------------------
   `(parenthesis
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))


   ;;----------------------------------------------------------------------------
   ;; smartparens
   ;;----------------------------------------------------------------------------
   `(sp-pair-overlay-face
     ((,ask-dark-class (:background ,ask-dark-gray-5 :foreground nil))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-5 :foreground nil))))

   `(sp-show-pair-match-face
     ((,ask-dark-class (:background ,ask-dark-magenta :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta :foreground ,ask-dark-256-bright-white))))

   ;;----------------------------------------------------------------------------
   ;; evil-snipe
   ;;----------------------------------------------------------------------------
   `(evil-snipe-first-match-face
     ((,ask-dark-class (:foreground ,ask-dark-magenta :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :weight bold))))

   `(evil-snipe-matches-face
     ((,ask-dark-class (:foreground ,ask-dark-magenta :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :weight bold))))


   ;;----------------------------------------------------------------------------
   ;; spacemacs
   ;;----------------------------------------------------------------------------
   `(spacemacs-normal-face
     ((,ask-dark-class (:background ,ask-dark-gray-5 :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-gray-5 :foreground ,ask-dark-256-bright-white))))

   `(spacemacs-insert-face
     ((,ask-dark-class (:background ,ask-dark-bright-white :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-white :foreground ,ask-dark-256-black))))

   `(spacemacs-replace-face
     ((,ask-dark-class (:background ,ask-dark-bright-red :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-red :foreground ,ask-dark-256-bright-white))))

   `(spacemacs-visual-face
     ((,ask-dark-class (:background ,ask-dark-bright-cyan :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-cyan :foreground ,ask-dark-256-black))))

   `(spacemacs-motion-face
     ((,ask-dark-class (:background ,ask-dark-magenta :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-magenta :foreground ,ask-dark-256-bright-white))))

   `(spacemacs-emacs-face
     ((,ask-dark-class (:background ,ask-dark-orange :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-orange :foreground ,ask-dark-256-bright-white))))

   `(spacemacs-hybrid-face
     ((,ask-dark-class (:background ,ask-dark-bright-orange :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-orange :foreground ,ask-dark-256-black))))

   `(spacemacs-lisp-face
     ((,ask-dark-class (:background ,ask-dark-green :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-green :foreground ,ask-dark-256-black))))

   `(spacemacs-evilified-face
     ((,ask-dark-class (:background ,ask-dark-bright-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-yellow :foreground ,ask-dark-256-black))))

   `(spacemacs-helm-navigation-ms-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(spacemacs-transient-state-title-face
     ((,ask-dark-class (:background nil :foreground ,ask-dark-green :box nil :weight bold))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-green :box nil :weight bold))))

   `(spacemacs-ido-navigation-ts-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(spacemacs-iedit-face
     ((,ask-dark-class (:background ,ask-dark-blue :foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:background ,ask-dark-256-blue :foreground ,ask-dark-256-bright-white))))

   `(spacemacs-iedit-insert-face
     ((,ask-dark-class (:background ,ask-dark-bright-blue :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-black))))

   `(spacemacs-micro-state-binding-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   ;; spacemacs-ido-navigation-ts-face

   ;;----------------------------------------------------------------------------
   ;; swiper
   ;;----------------------------------------------------------------------------
   `(swiper-line-face
     ((,ask-dark-class (:background ,ask-dark-gray-2 :weight bold))
      (,ask-dark-256-class (:background ,ask-dark-gray-2 :weight bold))))

   `(swiper-match-face-1
     ((,ask-dark-class (:weight bold))
      (,ask-dark-256-class (:weight bold))))

   `(swiper-match-face-2
     ((,ask-dark-class (:foreground ,ask-dark-magenta :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta :underline t))))

   `(swiper-match-face-3
     ((,ask-dark-class (:foreground ,ask-dark-yellow :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :underline t))))

   `(swiper-match-face-4
     ((,ask-dark-class (:foreground ,ask-dark-bright-green :underline t))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green :underline t))))


   ;;----------------------------------------------------------------------------
   ;; term
   ;;----------------------------------------------------------------------------
   `(term
     ((,ask-dark-class (:foreground ,ask-dark-bright-white :background ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white :background ,(if ask-dark-transparent-background nil ask-dark-256-black)))))

   `(term-color-black
     ((,ask-dark-class (:foreground ,ask-dark-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black))))

   `(term-color-blue
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(term-color-cyan
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(term-color-green
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(term-color-magenta
     ((,ask-dark-class (:foreground ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-magenta))))

   `(term-color-red
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(term-color-white
     ((,ask-dark-class (:foreground ,ask-dark-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-white))))

   `(term-color-yellow
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))


   ;;----------------------------------------------------------------------------
   ;; web-mode
   ;;----------------------------------------------------------------------------
   `(web-mode-builtin-face
     ((,ask-dark-class (:inherit ,font-lock-builtin-face))
      (,ask-dark-256-class (:inherit ,font-lock-builtin-face))))

   `(web-mode-comment-face
     ((,ask-dark-class (:inherit ,font-lock-comment-face))
      (,ask-dark-256-class (:inherit ,font-lock-comment-face))))

   `(web-mode-constant-face
     ((,ask-dark-class (:inherit ,font-lock-constant-face))
      (,ask-dark-256-class (:inherit ,font-lock-constant-face))))

   `(web-mode-doctype-face
     ((,ask-dark-class (:inherit ,font-lock-comment-face))
      (,ask-dark-256-class (:inherit ,font-lock-comment-face))))

   `(web-mode-function-name-face
     ((,ask-dark-class (:inherit ,font-lock-function-name-face))
      (,ask-dark-256-class (:inherit ,font-lock-function-name-face))))

   `(web-mode-html-attr-name-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   `(web-mode-html-attr-value-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(web-mode-html-tag-face
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(web-mode-html-tag-bracket-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-bright-black))))

   `(web-mode-keyword-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(web-mode-string-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(web-mode-symbol-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-blue))))

   `(web-mode-type-face
     ((,ask-dark-class (:inherit ,font-lock-type-face))
      (,ask-dark-256-class (:inherit ,font-lock-type-face))))

   `(web-mode-warning-face
     ((,ask-dark-class (:inherit ,font-lock-warning-face))
      (,ask-dark-256-class (:inherit ,font-lock-warning-face))))

   ;;----------------------------------------------------------------------------
   ;; CSS
   ;;----------------------------------------------------------------------------
   `(css-selector
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(css-property
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; XML
   ;;----------------------------------------------------------------------------
   `(nxml-element-local-name
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(nxml-attribute-local-name
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; which-key
   ;;----------------------------------------------------------------------------
   `(which-key-command-description-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(which-key-group-description-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(which-key-key-face
     ((,ask-dark-class (:foreground ,ask-dark-yellow :weight bold))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow :weight bold))))

   `(which-key-separator-face
     ((,ask-dark-class (:background nil :foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-bright-green))))

   `(which-key-special-key-face
     ((,ask-dark-class (:background ,ask-dark-yellow :foreground ,ask-dark-black))
      (,ask-dark-256-class (:background ,ask-dark-256-yellow :foreground ,ask-dark-256-black))))


   ;;----------------------------------------------------------------------------
   ;; which-function-mode
   ;;----------------------------------------------------------------------------
   `(which-func
     ((,ask-dark-class (:foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-yellow))))


   ;;----------------------------------------------------------------------------
   ;; whitespace-mode
   ;;----------------------------------------------------------------------------
   `(whitespace-empty
     ((,ask-dark-class (:background nil :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-yellow))))

   `(whitespace-indentation
     ((,ask-dark-class (:background nil :foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-bright-orange))))

   `(whitespace-line
     ((,ask-dark-class (:background nil :foreground ,ask-dark-green))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-green))))

   `(whitespace-newline
     ((,ask-dark-class (:background nil :foreground ,ask-dark-green))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-green))))

   `(whitespace-space
     ((,ask-dark-class (:background nil :foreground ,ask-dark-gray-5))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-gray-5))))

   `(whitespace-space-after-tab
     ((,ask-dark-class (:background nil :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-yellow))))

   `(whitespace-space-before-tab
     ((,ask-dark-class (:background nil :foreground ,ask-dark-yellow))
      (,ask-dark-256-class (:background nil :foreground ,ask-dark-256-yellow))))

   `(whitespace-tab
     ((,ask-dark-class (:background nil))
      (,ask-dark-256-class (:background nil))))

   `(whitespace-trailing
     ((,ask-dark-class (:background ,ask-dark-red :foreground ,ask-dark-bright-orange))
      (,ask-dark-256-class (:background ,ask-dark-256-red :foreground ,ask-dark-256-bright-orange))))


   ;;----------------------------------------------------------------------------
   ;; ctbl
   ;;----------------------------------------------------------------------------
   `(ctbl:face-cell-select
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-magenta))))

   `(ctbl:face-continue-bar
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-yellow))))

   `(ctbl:face-row-select
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-blue))))

   ;;----------------------------------------------------------------------------
   ;; hlt
   ;;----------------------------------------------------------------------------
   `(hlt-property-highlight
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-yellow))))

   `(hlt-regexp-level-1
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-magenta))))

   `(hlt-regexp-level-2
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-green))))

   `(hlt-regexp-level-3
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-magenta))))

   `(hlt-regexp-level-4
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-yellow))))

   `(hlt-regexp-level-5
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-green))))

   `(hlt-regexp-level-6
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-blue))))

   `(hlt-regexp-level-7
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-cyan))))

   `(hlt-regexp-level-8
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-blue))))

   ;;----------------------------------------------------------------------------
   ;; reb
   ;;----------------------------------------------------------------------------
   `(reb-match-0
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-blue))))

   `(reb-match-1
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-bright-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-bright-cyan))))

   `(reb-match-2
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-green))))

   `(reb-match-3
     ((,ask-dark-class (:foreground ,ask-dark-black :background ,ask-dark-yellow))
      (,ask-dark-256-class (:foreground ,ask-dark-256-black :background ,ask-dark-256-yellow))))

   ;;----------------------------------------------------------------------------
   ;; other, need more work
   ;;----------------------------------------------------------------------------
   `(ac-completion-face
     ((,ask-dark-class (:underline t :foreground ,ask-dark-red))
      (,ask-dark-256-class (:underline t :foreground ,ask-dark-256-red))))

   `(epc:face-title
     ((,ask-dark-class :foreground ,ask-dark-blue :weight bold)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue :weight bold)))

   `(ffap
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(flx-highlight-face
     ((,ask-dark-class (:foreground ,ask-dark-green :underline nil))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green :underline nil))))

   `(icompletep-determined
     ((,ask-dark-class :foreground ,ask-dark-red)
      (,ask-dark-256-class :foreground ,ask-dark-256-red)))

   `(js2-external-variable
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(js2-function-param
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(js2-jsdoc-html-tag-delimiter
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(js2-jsdoc-html-tag-name
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(js2-jsdoc-value
     ((,ask-dark-class (:foreground ,ask-dark-bright-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-green))))

   `(js2-private-function-call
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(js2-private-member
     ((,ask-dark-class (:foreground ,ask-dark-bright-white))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-white))))

   `(js2-object-property
     ((,ask-dark-class (:foreground ,ask-dark-cyan))
      (,ask-dark-256-class (:foreground ,ask-dark-256-cyan))))

   `(js3-error-face
     ((,ask-dark-class (:underline ,ask-dark-bright-orange))
      (,ask-dark-256-class (:underline ,ask-dark-256-bright-orange))))

   `(js3-external-variable-face
     ((,ask-dark-class (:foreground ,ask-dark-blue))
      (,ask-dark-256-class (:foreground ,ask-dark-256-blue))))

   `(js3-function-param-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(js3-instance-member-face
     ((,ask-dark-class (:foreground ,ask-dark-bright-magenta))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-magenta))))

   `(js3-jsdoc-tag-face
     ((,ask-dark-class (:foreground ,ask-dark-red))
      (,ask-dark-256-class (:foreground ,ask-dark-256-red))))

   `(js3-warning-face
     ((,ask-dark-class (:underline ,ask-dark-red))
      (,ask-dark-256-class (:underline ,ask-dark-256-red))))

   `(slime-repl-inputed-output-face
     ((,ask-dark-class (:foreground ,ask-dark-green))
      (,ask-dark-256-class (:foreground ,ask-dark-256-green))))

   `(trailing-whitespace
     ((,ask-dark-class :foreground nil :background ,ask-dark-red)
      (,ask-dark-256-class :foreground nil :background ,ask-dark-256-red)))

   `(undo-tree-visualizer-current-face
     ((,ask-dark-class :foreground ,ask-dark-red)
      (,ask-dark-256-class :foreground ,ask-dark-256-red)))

   `(undo-tree-visualizer-default-face
     ((,ask-dark-class :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white)))

   `(undo-tree-visualizer-register-face
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(undo-tree-visualizer-unmodified-face
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(undo-tree-visualizer-active-branch-face
     ((,ask-dark-class :foreground ,ask-dark-bright-magenta)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-magenta)))

   `(persp-face-lighter-buffer-not-in-persp
     ((,ask-dark-class :background ,ask-dark-red :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white)))

   `(pulse-highlight-face
     ((,ask-dark-class :background ,ask-dark-green :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-green :foreground ,ask-dark-256-black)))

   `(pulse-highlight-start-face
     ((,ask-dark-class :background ,ask-dark-bright-green :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-bright-green :foreground ,ask-dark-256-black)))

   `(custom-invalid
     ((,ask-dark-class :background ,ask-dark-bright-red :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-bright-red :foreground ,ask-dark-256-bright-white)))

   `(holiday
     ((,ask-dark-class :background ,ask-dark-bright-magenta :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-bright-magenta :foreground ,ask-dark-256-bright-white)))

   `(whitespace-trailing
     ((,ask-dark-class :background ,ask-dark-red :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-red :foreground ,ask-dark-256-bright-white)))

   `(whitespace-big-indent
     ((,ask-dark-class :background ,ask-dark-bright-red :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-bright-red :foreground ,ask-dark-256-bright-white)))

   `(whitespace-hspace
     ((,ask-dark-class :background ,ask-dark-bright-blue :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-bright-white)))

   ;;----------------------------------------------------------------------------
   ;; Slack
   ;;----------------------------------------------------------------------------
   `(lui-button-face
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(lui-highlight-face
     ((,ask-dark-class :foreground ,ask-dark-magenta)
      (,ask-dark-256-class :foreground ,ask-dark-256-magenta)))

   `(lui-time-stamp-face
     ((,ask-dark-class :foreground ,ask-dark-bright-black)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-black)))

   `(slack-profile-image-face
     ((,ask-dark-class :background ,ask-dark-bright-white :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-bright-white :foreground ,ask-dark-256-black)))

   `(slack-preview-face
     ((,ask-dark-class :foreground ,ask-dark-cyan)
      (,ask-dark-256-class :foreground ,ask-dark-256-cyan)))

   `(slack-message-output-header
     ((,ask-dark-class :foreground ,ask-dark-yellow :weight bold)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow :weight bold)))

   ;;----------------------------------------------------------------------------
   ;; Message
   ;;----------------------------------------------------------------------------
   `(message-header-cc
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(message-header-newsgroups
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(message-header-subject
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(message-header-to
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   ;;----------------------------------------------------------------------------
   ;; Alert
   ;;----------------------------------------------------------------------------
   `(alert-low-face
     ((,ask-dark-class :foreground ,ask-dark-blue :weight bold)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue :weight bold)))

   `(alert-moderate-face
     ((,ask-dark-class :foreground ,ask-dark-yellow :weight bold)
      (,ask-dark-256-class :foreground ,ask-dark-256-yellow :weight bold)))

   ;;----------------------------------------------------------------------------
   ;; Custom
   ;;----------------------------------------------------------------------------
   `(custom-comment-tag
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(custom-face-tag
     ((,ask-dark-class :foreground ,ask-dark-blue :weight bold)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue :weight bold)))

   `(custom-group-tag
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(custom-state
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(custom-set
     ((,ask-dark-class :background ,ask-dark-bright-blue :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-bright-blue :foreground ,ask-dark-256-black)))

   `(custom-modified
     ((,ask-dark-class :background ,ask-dark-blue :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-blue :foreground ,ask-dark-256-bright-white)))

   `(custom-themed
     ((,ask-dark-class :background ,ask-dark-blue :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-blue :foreground ,ask-dark-256-black)))

   `(custom-variable-tag
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(custom-changed
     ((,ask-dark-class :background ,ask-dark-blue :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-blue :foreground ,ask-dark-256-black)))

   `(custom-comment
     ((,ask-dark-class :background ,ask-dark-bright-black :foreground ,ask-dark-black)
      (,ask-dark-256-class :background ,ask-dark-256-bright-black :foreground ,ask-dark-256-black)))

   ;;----------------------------------------------------------------------------
   ;; widget
   ;;----------------------------------------------------------------------------
   `(widget-field
     ((,ask-dark-class :background ,ask-dark-gray-3 :foreground ,ask-dark-bright-white)
      (,ask-dark-256-class :background ,ask-dark-256-gray-3 :foreground ,ask-dark-256-bright-white)))

   `(widget-documentation
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   ;;----------------------------------------------------------------------------
   ;; Misc
   ;;----------------------------------------------------------------------------
   `(epa-string
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(imenu-list-entry-face-0
     ((,ask-dark-class :foreground ,ask-dark-magenta)
      (,ask-dark-256-class :foreground ,ask-dark-256-magenta)))

   `(imenu-list-entry-face-1
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(imenu-list-entry-face-2
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(imenu-list-entry-face-3
     ((,ask-dark-class :foreground ,ask-dark-blue)
      (,ask-dark-256-class :foreground ,ask-dark-256-blue)))

   `(lv-separator
     ((,ask-dark-class :background ,ask-dark-white :foreground ,ask-dark-bright-black)
      (,ask-dark-256-class :background ,ask-dark-256-white :foreground ,ask-dark-256-bright-black)))

   ;;----------------------------------------------------------------------------
   ;; mmm
   ;;----------------------------------------------------------------------------
   `(mmm-default-submode-face
     ((,ask-dark-class :background ,(if ask-dark-transparent-background nil ask-dark-256-black))
      (,ask-dark-256-class :background nil)))

   ;;----------------------------------------------------------------------------
   ;; rst
   ;;----------------------------------------------------------------------------
   `(rst-level-1
     ((,ask-dark-class :inherit org-level-1)
      (,ask-dark-256-class :inherit org-level-1)))

   `(rst-level-2
     ((,ask-dark-class :inherit org-level-2)
      (,ask-dark-256-class :inherit org-level-2)))

   `(rst-level-3
     ((,ask-dark-class :inherit org-level-3)
      (,ask-dark-256-class :inherit org-level-3)))

   `(rst-level-4
     ((,ask-dark-class :inherit org-level-4)
      (,ask-dark-256-class :inherit org-level-4)))

   `(rst-level-5
     ((,ask-dark-class :inherit org-level-5)
      (,ask-dark-256-class :inherit org-level-5)))

   `(rst-level-6
     ((,ask-dark-class :inherit org-level-6)
      (,ask-dark-256-class :inherit org-level-6)))

   `(rst-adornment
     ((,ask-dark-class :foreground ,ask-dark-white)
      (,ask-dark-256-class :foreground ,ask-dark-256-white)))

   ;;----------------------------------------------------------------------------
   ;; lsp
   ;;----------------------------------------------------------------------------

   `(lsp-ui-doc-background
     ((,ask-dark-class :background ,ask-dark-gray-2)
      (,ask-dark-256-class :background ,ask-dark-256-gray-2)))

   `(lsp-ui-doc-header
     ((,ask-dark-class :foreground ,ask-dark-green)
      (,ask-dark-256-class :foreground ,ask-dark-256-green)))

   `(lsp-ui-peek-footer
     ((,ask-dark-class :foreground ,ask-dark-bright-white :background ,ask-dark-gray-3)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white :background ,ask-dark-256-gray-3)))

   `(lsp-ui-peek-header
     ((,ask-dark-class :foreground ,ask-dark-bright-white :background ,ask-dark-gray-4)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white :background ,ask-dark-256-gray-4)))

   `(lsp-ui-peek-highlight
     ((,ask-dark-class :foreground ,ask-dark-bright-white :background ,ask-dark-gray-5)
      (,ask-dark-256-class :foreground ,ask-dark-256-bright-white :background ,ask-dark-256-gray-5)))

   `(lsp-ui-peek-line-number
     ((,ask-dark-class (:foreground ,ask-dark-bright-black))
      (,ask-dark-256-class (:foreground ,ask-dark-256-bright-black))))

   `(lsp-ui-peek-list
     ((,ask-dark-class (:inherit lsp-ui-doc-background))
      (,ask-dark-256-class (:inherit lsp-ui-doc-background))))

   `(lsp-ui-peek-peek
     ((,ask-dark-class (:inherit lsp-ui-doc-background))
      (,ask-dark-256-class (:inherit lsp-ui-doc-background))))

   ;;----------------------------------------------------------------------------
   ;; Flymake
   ;;----------------------------------------------------------------------------
   `(flymake-error
     ((,ask-dark-class (:inherit flycheck-error))
      (,ask-dark-256-class (:inherit flycheck-error))))

   `(flymake-note
     ((,ask-dark-class (:inherit flycheck-info))
      (,ask-dark-256-class (:inherit flycheck-info))))

   `(flymake-warning
     ((,ask-dark-class (:inherit flycheck-warning))
      (,ask-dark-256-class (:inherit flycheck-warning))))
   )

  (custom-theme-set-variables
   'ask-dark
   `(ansi-color-names-vector [,ask-dark-black ,ask-dark-red ,ask-dark-green ,ask-dark-bright-yellow ,ask-dark-blue ,ask-dark-magenta ,ask-dark-cyan ,ask-dark-gray-6])
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ask-dark)
(provide 'ask-dark-theme)
;;; ask-dark-theme.el ends here
