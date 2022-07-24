;;; ask-acme-theme.el --- Acme theme -*- lexical-binding: t -*-

;; Copyright (C) 2021 cgfork

;; Author: cgfork
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.3"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Copy from https://github.com/ianyepan/acme-emacs-theme/blob/master/acme-theme.el
;;; Code:

(defgroup ask-acme-theme nil
  "Options for ask-acme theme."
  :group 'faces)

(defcustom ask-acme-theme-black-fg nil
  "If non-nil, foreground will be pure black instead of the default dark grey."
  :group 'ask-acme-theme
  :type 'boolean)

(deftheme ask-acme "A color theme based on Ask-Acme & Sam")

;;; Color palette

(let ((class '((class color) (min-colors 89)))
      (bg              "#FFFFE8") ; default bg
      (bg-alt          "#EFEFD8")
      (bg-dark         "#E5E5D0")
      (fg              (if ask-acme-theme-black-fg "#000000" "#444444")) ; default fg
      (fg-alt          "#B8B09A")
      (fg-dark         "#988D6D")
      (fg-light        "#CCCCB7")
      (highlight       "#E8EB98")
      (highlight-alt   "#E8EBC8")

      ;; Standardized palette
      (ask-acme-cyan            "#007777")
      (ask-acme-cyan-light      "#A8EFEB")
      (ask-acme-red             "#880000")
      (ask-acme-red-light       "#F8E8E8")
      (ask-acme-yellow          "#888838")
      (ask-acme-yellow-light    "#F8FCE8")
      (ask-acme-green           "#005500")
      (ask-acme-green-alt       "#006600")
      (ask-acme-green-light     "#E8FCE8")
      (ask-acme-blue            "#1054AF")
      (ask-acme-blue-light      "#E1FAFF")
      (ask-acme-purple          "#555599")
      (ask-acme-purple-light    "#FFEAFF"))

;;; Theme Faces
  (custom-theme-set-faces
   'ask-acme

;;;; Built-in

;;;;; basic coloring
   `(button                                       ((t (:underline t))))
   `(link                                         ((t (:foreground "#0066cc":weight normal))))
   `(highlight                                    ((t (:inherit link :underline t)))) ; link hover
   `(link-visited                                 ((t (:foreground ,ask-acme-purple :underline t :weight normal))))
   `(default                                      ((t (:foreground ,fg :background ,bg))))
   `(cursor                                       ((t (:foreground ,bg :background ,fg))))
   `(escape-glyph                                 ((t (:foreground ,ask-acme-cyan-light :bold nil))))
   `(fringe                                       ((t (:foreground ,fg :background ,bg))))
   `(line-number                                  ((t (:foreground ,fg :background ,bg-alt))))
   `(line-number-current-line                     ((t (:foreground ,fg :background ,bg-alt))))
   `(header-line                                  ((t (:foreground ,fg :background ,ask-acme-blue-light :box t))))
   `(success                                      ((t (:foreground ,ask-acme-green :weight normal))))
   `(warning                                      ((t (:foreground ,ask-acme-red :weight normal))))
   `(error                                        ((t (:foreground ,ask-acme-red :bold t))))

;;;;; compilation
   `(compilation-column-face                      ((t (:foreground ,ask-acme-yellow :background ,ask-acme-yellow-light))))
   `(compilation-column-number                    ((t (:foreground ,ask-acme-yellow :background ,ask-acme-yellow-light))))
   `(compilation-error-face                       ((t (:foreground ,ask-acme-red :weight normal :underline t))))
   `(compilation-face                             ((t (:foreground ,fg))))
   `(compilation-info-face                        ((t (:foreground ,ask-acme-blue))))
   `(compilation-info                             ((t (:foreground ,ask-acme-blue :underline t))))
   `(compilation-line-face                        ((t (:foreground ,ask-acme-purple))))
   `(compilation-line-number                      ((t (:foreground ,ask-acme-yellow :background ,ask-acme-yellow-light))))
   `(compilation-message-face                     ((t (:foreground ,ask-acme-blue))))
   `(compilation-warning-face                     ((t (:foreground ,ask-acme-yellow :weight normal :underline t))))
   `(compilation-mode-line-exit                   ((t (:foreground ,ask-acme-cyan :weight normal))))
   `(compilation-mode-line-fail                   ((t (:foreground ,ask-acme-red :weight normal))))
   `(compilation-mode-line-run                    ((t (:foreground ,ask-acme-purple :weight normal))))

;;;;; grep
   `(grep-context-face                            ((t (:foreground ,fg-alt))))
   `(grep-error-face                              ((t (:foreground ,ask-acme-red :weight normal :underline t))))
   `(grep-hit-face                                ((t (:foreground ,ask-acme-purple :weight normal))))
   `(grep-match-face                              ((t (:foreground ,ask-acme-cyan :weight normal))))
   `(match                                        ((t (:background ,ask-acme-cyan :foreground ,ask-acme-cyan-light))))

;;;;; ag
   `(ag-hit-face                                  ((t (:foreground ,ask-acme-green :weight normal))))
   `(ag-match-face                                ((t (:foreground ,ask-acme-cyan :background ,ask-acme-cyan-light :weight normal))))

;;;;; isearch
   `(isearch                                      ((t (:foreground ,fg :weight normal :background ,ask-acme-cyan-light))))
   `(isearch-fail                                 ((t (:foreground ,fg :weight normal :background ,ask-acme-red))))
   `(lazy-highlight                               ((t (:foreground ,fg :weight normal :background ,ask-acme-blue-light))))

   `(menu                                         ((t (:foreground ,bg :background ,fg))))
   `(minibuffer-prompt                            ((t (:foreground ,fg :weight normal))))
   `(region                                       ((,class (:foreground ,fg :background ,highlight :extend nil))))
   `(secondary-selection                          ((t (:background ,ask-acme-green-light))))
   `(trailing-whitespace                          ((t (:background ,ask-acme-red-light))))
   `(vertical-border                              ((t (:foreground ,ask-acme-cyan))))

;;;;; font lock
   `(font-lock-builtin-face                       ((t (:foreground ,fg :weight normal))))
   `(font-lock-function-name-face                 ((t (:foreground ,fg :weight normal))))
   `(font-lock-string-face                        ((t (:foreground ,ask-acme-red))))
   `(font-lock-keyword-face                       ((t (:foreground ,ask-acme-blue :weight bold)))) ; if, else, for, while, return...
   `(font-lock-type-face                          ((t (:foreground ,fg :weight bold)))) ; int, float, string, void...
   `(font-lock-constant-face                      ((t (:foreground ,fg :weight bold)))) ; NULL, nullptr, true, false...
   `(font-lock-variable-name-face                 ((t (:foreground ,fg :weight normal))))
   `(font-lock-comment-face                       ((t (:foreground ,ask-acme-green :italic nil))))
   `(font-lock-comment-delimiter-face             ((t (:foreground ,ask-acme-green :italic nil))))
   `(font-lock-doc-face                           ((t (:foreground ,ask-acme-yellow :italic nil))))
   `(font-lock-negation-char-face                 ((t (:foreground ,ask-acme-red :weight normal))))
   `(font-lock-preprocessor-face                  ((t (:foreground ,ask-acme-red :weight normal))))
   `(font-lock-regexp-grouping-construct          ((t (:foreground ,ask-acme-purple :weight normal))))
   `(font-lock-regexp-grouping-backslash          ((t (:foreground ,ask-acme-purple :weight normal))))
   `(font-lock-warning-face                       ((t (:foreground ,ask-acme-red :weight normal))))

;;;;; table
   `(table-cell                                   ((t (:background ,bg-alt))))

;;;;; ledger
   `(ledger-font-directive-face                   ((t (:foreground ,ask-acme-cyan))))
   `(ledger-font-periodic-xact-face               ((t (:inherit ledger-font-directive-face))))
   `(ledger-font-posting-account-face             ((t (:foreground ,ask-acme-blue))))
   `(ledger-font-posting-amount-face              ((t (:foreground ,ask-acme-red))))
   `(ledger-font-posting-date-face                ((t (:foreground ,ask-acme-red :weight normal))))
   `(ledger-font-payee-uncleared-face             ((t (:foreground ,ask-acme-purple))))
   `(ledger-font-payee-cleared-face               ((t (:foreground ,fg))))
   `(ledger-font-payee-pending-face               ((t (:foreground ,ask-acme-yellow))))
   `(ledger-font-xact-highlight-face              ((t (:background ,bg-alt))))

;;;; Third-party


;;;;; anzu
   `(anzu-mode-line                               ((t (:foreground ,ask-acme-yellow :background ,ask-acme-yellow-light :weight normal))))

;;;;; clojure-mode
   `(clojure-interop-method-face                  ((t (:inherit font-lock-function-name-face))))

;;;;; clojure-test-mode
   `(clojure-test-failure-face                    ((t (:foreground ,ask-acme-red :weight normal :underline t))))
   `(clojure-test-error-face                      ((t (:foreground ,ask-acme-red :weight normal :underline t))))
   `(clojure-test-success-face                    ((t (:foreground ,ask-acme-green :weight normal :underline t))))

;;;;; diff
   `(diff-added                                   ((,class (:foreground ,fg :background ,ask-acme-green-light))
                                                   (t (:foreground ,fg :background ,ask-acme-green-light))))
   `(diff-changed                                 ((t (:foreground ,ask-acme-yellow))))
   `(diff-context                                 ((t (:foreground ,fg))))
   `(diff-removed                                 ((,class (:foreground ,fg :background ,ask-acme-red-light))
                                                   (t (:foreground ,fg :background ,ask-acme-red-light))))
   `(diff-refine-added                            ((t :inherit diff-added :background ,ask-acme-green-light :weight bold :underline t)))
   `(diff-refine-change                           ((t :inherit diff-changed :weight normal)))
   `(diff-refine-removed                          ((t :inherit diff-removed :background ,ask-acme-red-light :weight bold :underline t)))
   `(diff-header                                  ((,class (:foreground ,fg :weight normal))
                                                   (t (:foreground ,ask-acme-purple-light :weight normal))))
   `(diff-file-header                             ((,class (:foreground ,fg :background ,ask-acme-cyan-light :weight normal))
                                                   (t (:foreground ,fg :background ,ask-acme-cyan-light :weight normal))))
   `(diff-hunk-header                             ((,class (:foreground ,ask-acme-green :weight normal))
                                                   (t (:foreground ,ask-acme-green :weight normal))))
;;;;; dired/dired+/dired-subtree
   `(dired-directory                              ((t (:foreground ,ask-acme-blue :weight bold))))
   `(diredp-display-msg                           ((t (:foreground ,ask-acme-blue))))
   `(diredp-compressed-file-suffix                ((t (:foreground ,ask-acme-purple))))
   `(diredp-date-time                             ((t (:foreground ,ask-acme-green))))
   `(diredp-deletion                              ((t (:foreground ,ask-acme-red))))
   `(diredp-deletion-file-name                    ((t (:foreground ,ask-acme-red))))
   `(diredp-dir-heading                           ((t (:foreground ,ask-acme-blue :background ,ask-acme-blue-light :weight bold))))
   `(diredp-dir-priv                              ((t (:foreground ,ask-acme-blue))))
   `(diredp-exec-priv                             ((t (:foreground ,ask-acme-yellow))))
   `(diredp-executable-tag                        ((t (:foreground ,ask-acme-yellow))))
   `(diredp-file-name                             ((t (:foreground ,fg))))
   `(diredp-file-suffix                           ((t (:foreground ,ask-acme-yellow))))
   `(diredp-flag-mark                             ((t (:foreground ,ask-acme-cyan))))
   `(diredp-flag-mark-line                        ((t (:foreground ,ask-acme-cyan))))
   `(diredp-ignored-file-name                     ((t (:foreground ,fg-light))))
   `(diredp-link-priv                             ((t (:foreground ,ask-acme-purple))))
   `(diredp-mode-line-flagged                     ((t (:foreground ,ask-acme-yellow))))
   `(diredp-mode-line-marked                      ((t (:foreground ,ask-acme-yellow))))
   `(diredp-no-priv                               ((t (:foreground ,fg))))
   `(diredp-number                                ((t (:foreground ,ask-acme-blue))))
   `(diredp-other-priv                            ((t (:foreground ,fg))))
   `(diredp-rare-priv                             ((t (:foreground ,fg))))
   `(diredp-read-priv                             ((t (:foreground ,fg))))
   `(diredp-symlink                               ((t (:foreground ,fg :background ,ask-acme-blue-light))))
   `(diredp-write-priv                            ((t (:foreground ,fg))))
   `(diredp-dir-name                              ((t (:foreground ,ask-acme-blue :weight bold))))
   `(dired-subtree-depth-1-face                   ((t (:background ,bg))))
   `(dired-subtree-depth-2-face                   ((t (:background ,bg))))
   `(dired-subtree-depth-3-face                   ((t (:background ,bg))))

;;;;; elfeed
   `(elfeed-search-date-face                      ((t (:foreground ,ask-acme-blue))))
   `(elfeed-search-title-face                     ((t (:foreground ,fg))))
   `(elfeed-search-unread-title-face              ((t (:foreground ,fg))))
   `(elfeed-search-feed-face                      ((t (:foreground ,ask-acme-green))))
   `(elfeed-search-tag-face                       ((t (:foreground ,ask-acme-red))))
   `(elfeed-search-unread-count-face              ((t (:foreground ,fg))))

;;;;; erc
   `(erc-default-face                             ((t (:foreground ,fg))))
   `(erc-header-line                              ((t (:inherit header-line))))
   `(erc-action-face                              ((t (:inherit erc-default-face))))
   `(erc-bold-face                                ((t (:inherit erc-default-face :weight normal))))
   `(erc-underline-face                           ((t (:underline t))))
   `(erc-error-face                               ((t (:inherit font-lock-warning-face))))
   `(erc-prompt-face                              ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light :weight normal))))
   `(erc-timestamp-face                           ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light))))
   `(erc-direct-msg-face                          ((t (:inherit erc-default))))
   `(erc-notice-face                              ((t (:foreground ,fg-light))))
   `(erc-highlight-face                           ((t (:background ,highlight))))
   `(erc-input-face                               ((t (:foreground ,fg :background ,bg-alt))))
   `(erc-current-nick-face                        ((t (:foreground ,fg :background ,ask-acme-cyan-light :weight normal
                                                                   :box (:line-width 1 :style released-button)))))
   `(erc-nick-default-face                        ((t (:weight normal :background ,bg-alt))))
   `(erc-my-nick-face                             ((t (:foreground ,fg :background ,ask-acme-cyan-light :weight normal
                                                                   :box (:line-width 1 :style released-button)))))
   `(erc-nick-msg-face                            ((t (:inherit erc-default))))
   `(erc-fool-face                                ((t (:inherit erc-default))))
   `(erc-pal-face                                 ((t (:foreground ,ask-acme-purple :weight normal))))
   `(erc-dangerous-host-face                      ((t (:inherit font-lock-warning-face))))
   `(erc-keyword-face                             ((t (:foreground ,ask-acme-yellow :weight normal))))

  ;;;;; evil
   `(evil-search-highlight-persist-highlight-face ((t (:inherit lazy-highlight))))

;;;;; flx
   `(flx-highlight-face                           ((t (:foreground ,ask-acme-yellow :background ,ask-acme-green-light
                                                                   :weight normal :underline t))))

;;;;; company
   `(company-tooltip                              ((t (:background ,ask-acme-blue-light))))
   `(company-tooltip-selection                    ((t (:background ,ask-acme-cyan-light))))
   `(company-tooltip-common                       ((t (:foreground ,ask-acme-blue :bold t))))
   `(company-tooltip-annotation                   ((t (:foreground ,ask-acme-yellow :italic t)))) ; parameter hints etc.
   `(company-scrollbar-fg                         ((t (:background ,ask-acme-cyan))))
   `(company-scrollbar-bg                         ((t (:background ,ask-acme-cyan-light))))
   `(company-preview-common                       ((t (:foreground ,fg :background ,ask-acme-cyan-light))))

;;;;; highlight-symbol
   `(highlight-symbol-face                        ((t (:background ,bg-alt))))

;;;;; highlight-numbers
   `(highlight-numbers-number                     ((t (:foreground ,ask-acme-blue))))

;;;;; highlight-operators
   `(highlight-operators-face                     ((t (:foreground ,fg))))

;;;;; hl-todo
   `(hl-todo                                      ((t (:inverse-video t))))

;;;;; hl-line-mode
   `(hl-line                                      ((,class (:background ,bg-alt))))

;;;;; hl-sexp
   `(hl-sexp-face                                 ((,class (:background ,bg-alt))))

;;;;; ido-mode
   `(ido-first-match                              ((t (:foreground ,fg :weight normal))))
   `(ido-only-match                               ((t (:foreground ,fg :weight normal))))
   `(ido-subdir                                   ((t (:foreground ,ask-acme-blue))))
   `(ido-indicator                                ((t (:foreground ,ask-acme-yellow))))

;;;;; ido-vertical
   `(ido-vertical-first-match-face                ((t (:foreground ,fg :background ,ask-acme-cyan-light :weight normal))))
   `(ido-vertical-only-match-face                 ((t (:foreground ,ask-acme-red :background ,ask-acme-red-light :weight normal))))
   `(ido-vertical-match-face                      ((t (:foreground ,fg :background ,ask-acme-green-light
                                                                   :weight normal :underline t))))

;;;;; indent-guide
   `(indent-guide-face                            ((t (:foreground ,highlight))))

;;;;; ivy
   `(ivy-current-match                            ((t (:background ,ask-acme-blue-light :underline t :extend t))))
   `(ivy-minibuffer-match-face-1                  ((t (:background ,bg-alt))))
   `(ivy-minibuffer-match-face-2                  ((t (:background ,ask-acme-cyan-light))))
   `(ivy-minibuffer-match-face-3                  ((t (:background ,ask-acme-purple-light))))
   `(ivy-minibuffer-match-face-3                  ((t (:background ,ask-acme-blue-light))))

;;;;; js2-mode
   `(js2-warning                                  ((t (:underline ,ask-acme-yellow))))
   `(js2-error                                    ((t (:foreground ,ask-acme-red :weight normal))))
   `(js2-jsdoc-tag                                ((t (:foreground ,ask-acme-purple))))
   `(js2-jsdoc-type                               ((t (:foreground ,ask-acme-blue))))
   `(js2-jsdoc-value                              ((t (:foreground ,ask-acme-cyan))))
   `(js2-function-param                           ((t (:foreground ,fg))))
   `(js2-external-variable                        ((t (:foreground ,ask-acme-cyan))))

;;;;; linum-mode
   `(linum                                        ((t (:foreground ,fg-light))))

;;;;; lsp-mode
   `(lsp-face-highlight-textual                   ((t (:background ,bg-dark))))
   `(lsp-face-highlight-read                      ((t (:background ,ask-acme-purple-light))))
   `(lsp-face-highlight-write                     ((t (:background ,ask-acme-green-light))))

;;;;; magit
   `(magit-section-heading                        ((t (:foreground ,ask-acme-cyan :background ,ask-acme-blue-light
                                                                   :weight normal :underline t))))
   `(magit-section-highlight                      ((t (:background ,bg-alt))))
   `(magit-section-heading-selection              ((t (:background ,highlight))))
   `(magit-filename                               ((t (:foreground ,fg))))
   `(magit-hash                                   ((t (:foreground ,ask-acme-yellow :weight normal))))
   `(magit-tag                                    ((t (:foreground ,ask-acme-purple :weight normal))))
   `(magit-refname                                ((t (:foreground ,ask-acme-purple :weight normal))))
   `(magit-head                                   ((t (:foreground ,ask-acme-green :weight normal))))
   `(magit-branch-local                           ((t (:foreground ,ask-acme-blue :background ,ask-acme-blue-light
                                                                   :weight normal))))
   `(magit-branch-remote                          ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light
                                                                   :weight normal))))
   `(magit-branch-current                         ((t (:foreground ,ask-acme-cyan :background ,ask-acme-cyan-light
                                                                   :weight normal
                                                                   :box (:line-width 1 :color ,ask-acme-cyan)))))
   `(magit-diff-file-heading                      ((t (:foreground ,fg :weight normal))))
   `(magit-diff-file-heading-highlight            ((t (:background ,bg-alt))))
   `(magit-diff-file-heading-selection            ((t (:foreground ,ask-acme-red :background ,highlight))))
   `(magit-diff-hunk-heading                      ((t (:foreground ,ask-acme-blue :background ,ask-acme-blue-light :weight normal :underline t))))
   `(magit-diff-hunk-heading-highlight            ((t (:background ,ask-acme-cyan-light))))
   `(magit-diff-added                             ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light))))
   `(magit-diff-removed                           ((t (:foreground ,ask-acme-red :background ,ask-acme-red-light))))
   `(magit-diff-context                           ((t (:foreground ,fg-dark :background nil))))
   `(magit-diff-added-highlight                   ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light))))
   `(magit-diff-removed-highlight                 ((t (:foreground ,ask-acme-red :background ,ask-acme-red-light))))
   `(magit-diff-context-highlight                 ((t (:foreground ,fg-dark :background ,bg-alt))))
   `(magit-diffstat-added                         ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light :weight normal))))
   `(magit-diffstat-removed                       ((t (:foreground ,ask-acme-red :background ,ask-acme-red-light :weight normal))))
   `(magit-log-author                             ((t (:foreground ,ask-acme-blue :weight normal))))
   `(magit-log-date                               ((t (:foreground ,ask-acme-purple :weight normal))))
   `(magit-log-graph                              ((t (:foreground ,ask-acme-red :weight normal))))
   `(magit-blame-heading                          ((t (:foreground ,fg-dark :background ,bg-alt))))

;;;;; paren-face
   `(parenthesis                                  ((t (:foreground "#CCCCB7"))))

;;;;; project-explorer
   `(pe/file-face                                 ((t (:foreground ,fg))))
   `(pe/directory-face                            ((t (:foreground ,ask-acme-blue :weight normal))))

;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face              ((t (:foreground ,ask-acme-green))))
   `(rainbow-delimiters-depth-2-face              ((t (:foreground ,ask-acme-blue))))
   `(rainbow-delimiters-depth-3-face              ((t (:foreground ,ask-acme-red))))

;;;;; show-paren
   `(show-paren-mismatch                          ((t (:foreground ,ask-acme-yellow :background ,ask-acme-red :weight normal))))
   `(show-paren-match                             ((t (:foreground ,fg :background ,ask-acme-cyan-light :weight normal))))

;;;;; mode-line/sml-mode-line
   `(mode-line                                    ((,class (:foreground ,fg :background ,ask-acme-blue-light :box t))))
   `(mode-line-inactive                           ((t (:foreground ,fg :background ,bg-dark :box t))))
   `(mode-line-buffer-id                          ((t (:foreground ,fg :weight bold)))) ; associated buffer/file name
   `(sml/global                                   ((t (:foreground ,fg))))
   `(sml/modes                                    ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light))))
   `(sml/filename                                 ((t (:foreground ,ask-acme-red))))
   `(sml/folder                                   ((t (:foreground ,fg))))
   `(sml/prefix                                   ((t (:foreground ,fg))))
   `(sml/read-only                                ((t (:foreground ,fg))))
   `(sml/modified                                 ((t (:foreground ,ask-acme-red :weight normal))))
   `(sml/outside-modified                         ((t (:background ,ask-acme-red :foreground ,ask-acme-red-light :weight normal))))
   `(sml/line-number                              ((t (:foreground ,fg :weight normal))))
   `(sml/col-number                               ((t (:foreground ,fg :weight normal))))
   `(sml/vc                                       ((t (:foreground ,fg :weight normal))))
   `(sml/vc-edited                                ((t (:foreground ,ask-acme-red :weight normal))))
   `(sml/git                                      ((t (:foreground ,fg :weight normal))))

;;;;; sh
   `(sh-heredoc-face                              ((t (:foreground ,ask-acme-purple))))

;;;;; web-mode
   `(web-mode-builtin-face                        ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face                        ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face                       ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face                        ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face                         ((t (:underline t))))
   `(web-mode-function-name-face                  ((t (:foreground ,fg :weight normal))))
   `(web-mode-html-attr-name-face                 ((t (:foreground ,fg))))
   `(web-mode-html-attr-value-face                ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face                       ((t (:foreground ,ask-acme-blue))))
   `(web-mode-keyword-face                        ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face                   ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face                         ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face                           ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face                  ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face              ((t (:background ,ask-acme-green-light))))
   `(web-mode-server-comment-face                 ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face                  ((t (:foreground ,ask-acme-red))))
   `(web-mode-symbol-face                         ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face                        ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face                    ((t (:background ,ask-acme-red-light))))
   `(web-mode-block-face                          ((t (:background ,ask-acme-green-light))))
   `(web-mode-current-element-highlight-face      ((t (:foreground ,fg :background ,ask-acme-blue-light))))
   `(web-mode-json-key-face                       ((,class (:inherit font-lock-string-face))))
   `(web-mode-json-context-face                   ((,class (:inherit font-lock-string-face :bold t))))

;;;;; which-func-mode
   `(which-func                                   ((t (:foreground ,ask-acme-purple :background ,ask-acme-purple-light))))

;;;;; yascroll
   `(yascroll:thumb-text-area                     ((t (:background ,highlight))))
   `(yascroll:thumb-fringe                        ((t (:background ,bg :foreground ,bg
                                                                   :box (:line-width 1 :style released-button)))))

;;;;; Org
   `(org-level-1                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-blue :weight bold :overline t))))
   `(org-level-2                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-cyan :weight bold :overline t))))
   `(org-level-3                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-blue :weight bold :overline t))))
   `(org-level-4                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-cyan))))
   `(org-level-5                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-blue))))
   `(org-level-6                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-cyan))))
   `(org-level-7                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-blue))))
   `(org-level-8                                  ((t (:background ,ask-acme-blue-light :foreground ,ask-acme-cyan))))
   `(org-document-title                           ((t (:height 1.2 :foreground ,ask-acme-blue :weight bold :underline t)))) ; #TITLE
   `(org-meta-line                                ((t (:foreground ,ask-acme-green))))
   `(org-document-info                            ((t (:foreground ,ask-acme-cyan :weight normal))))
   `(org-document-info-keyword                    ((t (:foreground ,ask-acme-cyan))))
   `(org-todo                                     ((t (:foreground ,ask-acme-yellow :background ,bg-alt :weight normal :box (:line-width 1 :style released-button)))))
   `(org-done                                     ((t (:foreground ,ask-acme-green :background ,ask-acme-green-light :weight normal :box (:style released-button)))))
   `(org-date                                     ((t (:foreground ,ask-acme-purple))))
   `(org-table                                    ((t (:foreground ,ask-acme-purple))))
   `(org-formula                                  ((t (:foreground ,ask-acme-blue :background ,bg-alt))))
   `(org-code                                     ((t (:foreground ,ask-acme-red :background ,bg-alt))))
   `(org-verbatim                                 ((t (:foreground ,fg :background ,bg-alt :underline t))))
   `(org-special-keyword                          ((t (:foreground ,ask-acme-cyan))))
   `(org-agenda-date                              ((t (:foreground ,ask-acme-cyan))))
   `(org-agenda-structure                         ((t (:foreground ,ask-acme-purple))))
   `(org-block                                    ((t (:foreground ,fg :background ,bg-alt :extend t))))
   `(org-block-background                         ((t (:background ,bg-alt :extend t))))
   `(org-block-begin-line                         ((t (:foreground ,fg-alt :background ,bg-dark :italic t :extend t))))
   `(org-block-end-line                           ((t (:foreground ,fg-alt :background ,bg-dark :italic t :extend t))))

;;;;; origami
   `(origami-fold-replacement-face                ((t (:foreground ,ask-acme-red :background ,ask-acme-red-light
                                                                   :box (:line-width -1)))))

;;;;; git-gutter
   `(git-gutter:added                             ((t (:background ,ask-acme-green-alt :foreground ,ask-acme-green-alt :weight normal))))
   `(git-gutter:deleted                           ((t (:background ,ask-acme-red :foreground ,ask-acme-red :weight normal))))
   `(git-gutter:modified                          ((t (:background ,ask-acme-yellow :foreground ,ask-acme-yellow :weight normal))))
   `(git-gutter-fr:added                          ((t (:background ,ask-acme-green-alt :foreground ,ask-acme-green-alt :weight normal))))
   `(git-gutter-fr:deleted                        ((t (:background ,ask-acme-red :foreground ,ask-acme-red :weight normal))))
   `(git-gutter-fr:modified                       ((t (:background ,ask-acme-yellow :foreground ,ask-acme-yellow :weight normal))))

;;;;; diff-hl
   `(diff-hl-insert                               ((t (:background ,ask-acme-green-alt :foreground ,ask-acme-green-alt))))
   `(diff-hl-delete                               ((t (:background ,ask-acme-red :foreground ,ask-acme-red))))
   `(diff-hl-change                               ((t (:background ,ask-acme-yellow :foreground ,ask-acme-yellow))))

;;;;; mu4e, mail
   `(mu4e-header-highlight-face                   ((t (:background ,highlight))))
   `(mu4e-unread-face                             ((t (:foreground ,ask-acme-blue :weight normal))))
   `(mu4e-flagged-face                            ((t (:foreground ,ask-acme-red :background ,ask-acme-red-light :weight normal))))
   `(mu4e-compose-separator-face                  ((t (:foreground ,ask-acme-green))))
   `(mu4e-header-value-face                       ((t (:foreground ,fg))))
   `(message-header-name                          ((t (:foreground ,ask-acme-purple :weight normal))))
   `(message-header-to                            ((t (:foreground ,ask-acme-blue))))
   `(message-header-subject                       ((t (:foreground ,ask-acme-blue))))
   `(message-header-other                         ((t (:foreground ,ask-acme-blue))))
   `(message-cited-text                           ((t (:inherit font-lock-comment-face))))

;;;;; term-mode (vterm too)
   `(term                                         ((,class (:foreground ,fg :background ,bg))))
   `(term-color-black                             ((,class (:foreground ,fg :background ,fg))))
   `(term-color-blue                              ((,class (:foreground ,ask-acme-blue :background ,ask-acme-blue))))
   `(term-color-red                               ((,class (:foreground ,ask-acme-red :background ,ask-acme-red))))
   `(term-color-green                             ((,class (:foreground ,ask-acme-green :background ,ask-acme-green))))
   `(term-color-yellow                            ((,class (:foreground ,ask-acme-yellow :background ,ask-acme-yellow))))
   `(term-color-magenta                           ((,class (:foreground ,ask-acme-purple :background ,ask-acme-purple))))
   `(term-color-cyan                              ((,class (:foreground ,ask-acme-cyan :background ,ask-acme-cyan))))
   `(term-color-white                             ((,class (:foreground ,fg :background ,fg))))

;;;;; fill-column-indicator
   `(fci-rule-color                               ((t (:foreground ,highlight-alt))))
   `(fill-column-indicator                        ((t (:foreground ,highlight-alt))))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'ask-acme)
(provide 'ask-acme-theme)
;;; ask-acme-theme.el ends here
