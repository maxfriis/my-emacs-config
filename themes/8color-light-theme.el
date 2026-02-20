;;; 8color-light-theme.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 8 colors: #???.
;; |------+-----------------+------+-----------------|
;; | #edc | background      | #210 | default text    |
;; |------+-----------------+------+-----------------|
;; | #987 | shadow/hl-line  | #00f | comment/tag     |
;; | #090 | success/done    | #f00 | error/todo      |
;; | #b0b | link/timestamp  | #940 | warning/heading |
;; |------+-----------------+------+-----------------|
;; The colors are based on f, b, 7 and 0 adjusting grays to brownish.
;; Unspecified faces are handled by the light part of the vanilla theme.
;; ----------------------------------------------------------------------------
(deftheme 8color-light)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   '8color-light
;;;; Special faces.
   `(default
     ((,class (:foreground "#210" :background "#edc"))))
   `(error
     ((,class (:foreground "#f00" :underline t))))
   `(warning
     ((,class (:foreground "#940"))))
   `(success
     ((,class (:foreground "#090"))))
   ;; ----------------------------------------------------------------------------
;;;; font-lock.
   `(font-lock-comment-face
     ((,class (:foreground "#00f" :slant italic))))
   `(font-lock-string-face
     ((,class (:foreground "#00f"))))
   `(font-lock-builtin-face
     ((,class (:foreground "#b0f"))))
   `(font-lock-type-face
     ((,class (:foreground "#b0f"))))
   `(font-lock-constant-face
     ((,class (:foreground "#b0f"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#090"))))
   `(font-lock-variable-name-face
     ((,class (:foreground "#090"))))
   `(font-lock-keyword-face
     ((,class (:foreground "#940"))))
   `(font-lock-number-face
     ((,class (:foreground "#f00"))))
   `(font-lock-warning-face
     ((,class (:underline nil))))
   ;; ----------------------------------------------------------------------------
;;;; Decorations.
   `(vertical-border
     ((,class (:foreground "#987"))))
   `(scroll-bar
     ((,class (:foreground "#987"))))
   `(region
     ((,class (:background "#987"))))
   `(shadow
     ((,class (:foreground "#987"))))
   `(match
     ((,class (:background "#987"))))
   `(highlight
     ((,class (:background "#987"))))
   `(lazy-highlight
     ((,class (:background "#987"))))
   `(isearch
     ((,class (:foreground "#edc" :background "#210"))))
   `(fringe
     ((,class (:foreground "#987" :background "#edc"))))
   `(line-number
     ((,class (:foreground "#987" :background "#edc" :height .8))))
   `(line-number-current-line
     ((,class (:foreground "#edc" :background "#987" :weight bold))))
   `(mode-line
     ((,class (:foreground "#edc" :background "#987" :box nil :height .8))))
   `(mode-line-inactive
     ((,class (:foreground "#987" :background "#edc" :box nil :overline t :height .8))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color "#00f" :style released-button)))))

   ;; ============================================================================
;;; Org
   ;; ============================================================================
   `(org-document-title
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-1
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-2
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-3
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-4
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-5
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-6
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-7
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-level-8
     ((,class (:foreground "#940" :weight bold :height 1.25))))
   `(org-todo
     ((,class (:foreground "#f00" :height .8))))
   `(org-done
     ((,class (:foreground "#090" :height .8))))
   `(org-headline-done
     ((,class (:foreground "#00f"))))
   `(org-ellipsis
     ((,class (:foreground "#00f" :weight normal :underline nil :height .8))))
   `(org-document-info-keyword
     ((,class (:foreground "#00f" :weight normal :height .8))))
   `(org-special-keyword
     ((,class (:foreground "#00f" :weight normal :height .8))))
   `(org-checkbox
     ((,class (:foreground "#00f" :background "#edc" :box nil :height .8))))
   `(org-tag
     ((,class (:foreground "#00f" :weight normal))))
   `(org-code
     ((,class (:foreground "#b0f"))))
   `(org-verbatim
     ((,class (:foreground "#b0f" :box (:line-width 1 :color "#00f" :style released-button)))))
   `(org-table
     ((,class (:foreground "#00f" :height .8))))
   `(org-formula
     ((,class (:foreground "#00f" :height .8))))
   `(org-block
     ((,class (:foreground "#210"))))
   `(org-block-begin-line
     ((,class (:foreground "#b0f" :height .8))))
   `(org-block-end-line
     ((,class (:foreground "#b0f"))))
   `(org-drawer
     ((,class (:foreground "#b0f" :height .8))))
   `(org-priority
     ((,class (:foreground "#b0f" :height .8))))
   `(org-footnote
     ((,class (:foreground "#b0f" :underline nil :height .8))))
   `(org-date
     ((,class (:foreground "#b0f" :underline nil :height .8))))
   `(org-link
     ((,class (:foreground "#b0f"))))
   ;; ----------------------------------------------------------------------------
;;;; Bullets.
   `(org-superstar-leading
     ((,class (:foreground "#987" :height .8))))
   `(org-superstar-header-bullet
     ((,class (:foreground "#00f" :slant normal :height .8))))
   `(org-superstar-item
     ((,class (:foreground "#940" :height .8))))
   ;; ============================================================================
;;;; Agenda
   ;; ============================================================================
   `(header-line
     ((,class (:foreground "#940" :background "#edc" :weight bold :height 1.25))))
   `(org-agenda-structure
     ((,class (:foreground "#940" :background "#edc" :box nil :weight bold :height 1.25))))
   `(org-column
     ((,class (:background "#edc"))))
   `(org-warning
     ((,class (:foreground "#f00"))))
   `(org-agenda-done
     ((,class (:foreground "#00f" :slant normal))))
   `(org-time-grid
     ((,class (:foreground "#00f"))))
   `(calendar-weekday-header
     ((,class (:foreground "#00f"))))
   `(org-agenda-calendar-event
     ((,class (:foreground "#210"))))
   `(org-agenda-clocking
     ((,class (:foreground "#00f" :background "#987"))))
   `(org-agenda-date
     ((,class (:foreground "#b0f" :background "#edc" :box nil :weight normal))))
   `(org-agenda-date-weekend
     ((,class (:foreground "#b0f" :background "#edc" :box nil :weight normal :underline nil))))
   `(org-agenda-date-today
     ((,class (:foreground "#940" :background "#edc" :box nil :weight normal :slant normal :inverse-video nil))))
   `(org-upcoming-distant-deadline
     ((,class (:foreground "#090"))))
   `(org-upcoming-deadline
     ((,class (:foreground "#090"))))
   `(org-imminent-deadline
     ((,class (:foreground "#940" :weight normal))))
   `(org-scheduled
     ((,class (:foreground "#090"))))
   `(org-scheduled-today
     ((,class (:foreground "#090"))))
   `(org-scheduled-previously
     ((,class (:foreground "#940"))))
   ;; ----------------------------------------------------------------------------
;;;; Habit.
   `(org-habit-alert-face
     ((,class (:foreground "#f00" :background "#940" :weight bold))))
   `(org-habit-alert-future-face
     ((,class (:background "#940"))))
   `(org-habit-overdue-face
     ((,class (:foreground "#940" :background "#f00" :weight bold))))
   `(org-habit-overdue-future-face
     ((,class (:background "#987"))))
   `(org-habit-ready-face
     ((,class (:foreground "#940" :background "#090" :weight bold))))
   `(org-habit-ready-future-face
     ((,class (:background "#090"))))
   `(org-habit-clear-face
     ((,class (:foreground "#940" :background "#987" :weight bold))))
   `(org-habit-clear-future-face
     ((,class (:background "#987"))))

   ;; ============================================================================
;;; Misc. other packages
   ;; ============================================================================
   `(show-paren-match
     ((,class (:foreground "#210" :background "#987" :weight bold))))
   `(keycast-key
     ((,class (:foreground "#edc" :background "#987" :box t :height .8))))
   `(corfu-default
     ((,class (:foreground "#987" :background "#edc"))))
   `(corfu-current
     ((,class (:foreground "#edc" :background "#987"))))
   `(aw-leading-char-face
     ((,class (:foreground "#940" :height 1.0))))
   `(ivy-current-match ; `counsel use this face.
     ((,class (:foreground "#090" :background "#987"))))
   `(indent-guide-face
     ((,class (:foreground "#987"))))
   `(dired-ignored
     ((,class (:foreground "#00f"))))
   `(dired-subtree-depth-1-face
     ((,class (:background "#987"))))
   `(dired-subtree-depth-2-face
     ((,class (:background "#edc"))))
   `(dired-subtree-depth-3-face
     ((,class (:background "#987"))))
   `(dired-subtree-depth-4-face
     ((,class (:background "#edc"))))
   `(dired-subtree-depth-5-face
     ((,class (:background "#987"))))
   `(marginalia-documentation
     ((,class (:foreground "#00f"))))
   `(corfu-default
     ((,class (:foreground "#987" :background "#edc"))))
   `(corfu-current
     ((,class (:foreground "#edc" :background "#987"))))
   `(flyspell-duplicate
     ((,class (:underline (:style wave :color "#940")))))
   `(flyspell-incorrect
     ((,class (:underline (:style wave :color "#f00")))))
   `(rainbow-delimiters-base-error-face
     ((,class (:foreground "#210" :background "#f00" :weight bold))))
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground "#090"))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground "#b0f"))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground "#940"))))
   `(tab-bar
     ((,class (:foreground "#987" :background "#edc" :weight bold :box nil :height .8 :inherit default))))
   `(tab-bar-tab
     ((,class (:foreground "#edc" :background "#987" :box t))))
   `(tab-bar-tab-inactive
     ((,class (:foreground "#987" :background "#edc" :box nil))))
   (unless (version<= emacs-version "31.0")
     `(tab-bar-tab-highlight
       ((,class (:foreground "#edc" :background "#987" :box (:line-width 1 :color "#00f" :style released-button))))))
   `(tab-line
     ((,class (:foreground "#987" :background "#edc" :overline t :box nil))))
   `(tab-line-tab ;; active tab in another frame
     ((,class (:box nil :inherit tab-line))))
   `(tab-line-tab-current
     ((,class (:foreground "#edc" :background "#987" :box nil))))
   `(tab-line-tab-modified
     ((,class (:foreground "#edc" :background "#987" :box nil))))
   `(tab-line-tab-inactive
     ((,class (:foreground "#987" :background "#edc" :box nil))))
   `(tab-line-highlight ;; mouseover
     ((,class (:foreground "#987" :background "#edc" :box (:line-width 1 :color "#00f" :style released-button)))))))

(custom-theme-set-variables
 '8color-light
 '(ansi-color-names-vector ["#000" "#f00" "#0f0" "#ff0" "#00f" "#f0f" "#0ff" "#fff"]))

(provide-theme '8color-light)
;;; 8color-light-theme.el ends here
