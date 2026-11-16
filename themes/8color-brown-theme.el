;;; 8color-brown-theme.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; Dark, warm, systematic, simple and aesthetically pleasing.  No #73f.
;; Unspecified faces are handled by the dark part of the vanilla theme.
;; ----------------------------------------------------------------------------
(deftheme 8color-brown
  "8 #??? colors: combining f(15), 7 and 3 plus some dusty brownish colors.
\n| #210 | background      | #dcb | default text    |
| #f73 | warning/heading | #f37 | error/todo      |
| #432 | shadow/hl-line  | #7f3 | success/done    |
| #3f7 | other/timestamp | #37f | comment/tag     |"
  :background-mode 'dark
  :kind 'color-scheme
  :family '8color)

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   '8color-brown
   ;; ----------------------------------------------------------------------------
   ;; Special faces.
   `(default
     ((,class (:foreground "#dcb" :background "#210"))))
   `(error
     ((,class (:foreground "#f37" :underline (:style wave :color "#f73")))))
   `(warning
     ((,class (:foreground "#f73"))))
   `(success
     ((,class (:foreground "#7f3"))))
   ;; ----------------------------------------------------------------------------
   ;; font-lock.
   `(font-lock-comment-face
     ((,class (:foreground "#37f" :slant italic))))
   `(font-lock-string-face
     ((,class (:foreground "#37f"))))
   `(font-lock-builtin-face
     ((,class (:foreground "#3f7"))))
   `(font-lock-type-face
     ((,class (:foreground "#3f7"))))
   `(font-lock-constant-face
     ((,class (:foreground "#3f7"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#7f3"))))
   `(font-lock-variable-name-face
     ((,class (:foreground "#7f3"))))
   `(font-lock-keyword-face
     ((,class (:foreground "#f73"))))
   `(font-lock-number-face
     ((,class (:foreground "#f37"))))
   `(font-lock-warning-face
     ((,class (:underline nil))))
   ;; ----------------------------------------------------------------------------
   ;; Decorations.
   `(vertical-border
     ((,class (:foreground "#432"))))
   `(scroll-bar
     ((,class (:foreground "#432"))))
   `(region
     ((,class (:background "#432"))))
   `(shadow
     ((,class (:foreground "#432"))))
   `(match
     ((,class (:background "#432"))))
   `(highlight
     ((,class (:background "#432"))))
   `(lazy-highlight
     ((,class (:background "#432"))))
   `(isearch
     ((,class (:foreground "#210" :background "#dcb"))))
   `(isearch-fail
     ((,class (:foreground "#210" :background "#f73"))))
   `(fringe
     ((,class (:foreground "#432" :background "#210"))))
   `(line-number
     ((,class (:foreground "#432" :background "#210" :height .8))))
   `(line-number-current-line
     ((,class (:inherit 'line-number :foreground "#210" :background "#432" :weight bold))))
   `(mode-line
     ((,class (:foreground "#210" :background "#432" :box nil :height .8))))
   `(mode-line-inactive
     ((,class (:foreground "#432" :background "#210" :box nil :overline t :height .8))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color "#37f" :style released-button)))))

   ;; ============================================================================
;;; Org
   ;; ============================================================================
   `(org-document-title
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-1
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-2
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-3
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-4
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-5
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-6
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-7
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-level-8
     ((,class (:foreground "#f73" :weight bold :height 1.25))))
   `(org-todo
     ((,class (:foreground "#f37" :height .8))))
   `(org-done
     ((,class (:foreground "#7f3" :height .8))))
   `(org-headline-done
     ((,class (:foreground "#37f"))))
   `(org-ellipsis
     ((,class (:foreground "#37f" :weight normal :underline nil :height .8))))
   `(org-document-info-keyword
     ((,class (:foreground "#37f" :weight normal :height .8))))
   `(org-special-keyword
     ((,class (:foreground "#37f" :weight normal :height .8))))
   `(org-checkbox
     ((,class (:foreground "#37f" :background "#210" :box nil :height .8))))
   `(org-tag
     ((,class (:foreground "#37f" :weight normal))))
   `(org-code
     ((,class (:foreground "#3f7"))))
   `(org-verbatim
     ((,class (:foreground "#3f7" :box (:line-width 1 :color "#37f" :style released-button)))))
   `(org-table
     ((,class (:foreground "#37f" :height .8))))
   `(org-formula
     ((,class (:foreground "#37f" :height .8))))
   `(org-block
     ((,class (:foreground "#dcb"))))
   `(org-block-begin-line
     ((,class (:foreground "#3f7" :height .8))))
   `(org-block-end-line
     ((,class (:foreground "#3f7"))))
   `(org-drawer
     ((,class (:foreground "#3f7" :height .8))))
   `(org-priority
     ((,class (:foreground "#3f7" :height .8))))
   `(org-footnote
     ((,class (:foreground "#3f7" :underline nil :height .8))))
   `(org-date
     ((,class (:foreground "#3f7" :underline nil :height .8))))
   `(org-link
     ((,class (:foreground "#3f7"))))
   ;; ----------------------------------------------------------------------------
   ;; Bullets.
   `(org-superstar-leading
     ((,class (:foreground "#432" :height .8))))
   `(org-superstar-header-bullet
     ((,class (:foreground "#37f" :slant normal :height .8))))
   `(org-superstar-item
     ((,class (:foreground "#f73" :height .8))))

   ;; ============================================================================
;;; Agenda
   ;; ============================================================================
   `(header-line
     ((,class (:foreground "#f73" :background "#210" :weight bold :height 1.25))))
   `(org-agenda-structure
     ((,class (:foreground "#f73" :background "#210" :box nil :weight bold :height 1.25))))
   `(org-column
     ((,class (:background "#210"))))
   `(org-warning
     ((,class (:foreground "#f37"))))
   `(org-agenda-done
     ((,class (:foreground "#37f" :slant normal))))
   `(org-time-grid
     ((,class (:foreground "#37f"))))
   `(calendar-weekday-header
     ((,class (:foreground "#37f"))))
   `(org-agenda-calendar-event
     ((,class (:foreground "#dcb"))))
   `(org-agenda-clocking
     ((,class (:foreground "#37f" :background "#432"))))
   `(org-agenda-date
     ((,class (:foreground "#37f" :background "#210" :box nil :weight normal))))
   `(org-agenda-date-weekend
     ((,class (:foreground "#37f" :background "#210" :box nil :weight normal :underline nil))))
   `(org-agenda-date-today
     ((,class (:foreground "#3f7" :background "#210" :box nil :weight normal :underline nil :slant normal :inverse-video nil))))
   `(org-upcoming-distant-deadline
     ((,class (:foreground "#7f3"))))
   `(org-upcoming-deadline
     ((,class (:foreground "#7f3"))))
   `(org-imminent-deadline
     ((,class (:foreground "#f73" :weight normal))))
   `(org-scheduled
     ((,class (:foreground "#7f3"))))
   `(org-scheduled-today
     ((,class (:foreground "#7f3"))))
   `(org-scheduled-previously
     ((,class (:foreground "#f73"))))
   ;; ----------------------------------------------------------------------------
   ;; Habit.
   `(org-habit-alert-face
     ((,class (:foreground "#37f" :background "#f73" :weight bold))))
   `(org-habit-alert-future-face
     ((,class (:background "#f73"))))
   `(org-habit-overdue-face
     ((,class (:foreground "#37f" :background "#f37" :weight bold))))
   `(org-habit-overdue-future-face
     ((,class (:background "#3f7"))))
   `(org-habit-ready-face
     ((,class (:foreground "#37f" :background "#7f3" :weight bold))))
   `(org-habit-ready-future-face
     ((,class (:background "#7f3"))))
   `(org-habit-clear-face
     ((,class (:foreground "#37f" :background "#432" :weight bold))))
   `(org-habit-clear-future-face
     ((,class (:background "#432"))))

   ;; ============================================================================
;;; Misc. other packages
   ;; ============================================================================
   `(help-key-binding
     ((,class (:foreground "#dcb" :background "#432" :weight bold))))
   `(show-paren-match
     ((,class (:foreground "#dcb" :background "#432" :weight bold))))
   `(keycast-key
     ((,class (:foreground "#210" :background "#432" :box t :height .8))))
   `(dired-ignored
     ((,class (:foreground "#37f"))))
   `(dired-subtree-depth-1-face
     ((,class (:background "#432"))))
   `(dired-subtree-depth-2-face
     ((,class (:background "#210"))))
   `(dired-subtree-depth-3-face
     ((,class (:background "#432"))))
   `(dired-subtree-depth-4-face
     ((,class (:background "#210"))))
   `(dired-subtree-depth-5-face
     ((,class (:background "#432"))))
   `(aw-leading-char-face
     ((,class (:foreground "#f73" :height 1.0))))
   `(ivy-current-match ; `counsel' use this face.
     ((,class (:foreground "#7f3" :background "#432"))))
   `(indent-guide-face
     ((,class (:inherit 'shadow))))
   `(dired-ignored
     ((,class (:foreground "#37f"))))
   `(marginalia-documentation
     ((,class (:foreground "#37f"))))
   `(corfu-default
     ((,class (:foreground "#432" :background "#210"))))
   `(corfu-current
     ((,class (:foreground "#210" :background "#432"))))
   `(flyspell-duplicate
     ((,class (:underline (:style wave :color "#f73")))))
   `(flyspell-incorrect
     ((,class (:underline (:style wave :color "#f37")))))
   `(rainbow-delimiters-base-error-face
     ((,class (:foreground "#dcb" :background "#f37" :weight bold))))
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground "#7f3"))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground "#f73"))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground "#3f7"))))
   `(tab-bar
     ((,class (:foreground "#432" :background "#210" :weight bold :box nil :height .8))))
   `(tab-bar-tab
     ((,class (:foreground "#210" :background "#432" :box t))))
   `(tab-bar-tab-inactive
     ((,class (:foreground "#432" :background "#210" :box nil))))
   (unless (version<= emacs-version "31.0")
     `(tab-bar-tab-highlight
       ((,class (:foreground "#210" :background "#432" :box (:line-width 1 :color "#37f" :style released-button))))))
   `(tab-line
     ((,class (:foreground "#432" :background "#210" :overline t :box nil))))
   `(tab-line-tab ;; active tab in another frame
     ((,class (:box nil :inherit 'tab-line))))
   `(tab-line-tab-current
     ((,class (:foreground "#210" :background "#432" :box nil))))
   `(tab-line-tab-modified
     ((,class (:foreground "#210" :background "#432" :box nil))))
   `(tab-line-tab-inactive
     ((,class (:foreground "#432" :background "#210" :box nil))))
   `(tab-line-highlight ;; mouseover
     ((,class (:foreground "#432" :background "#210" :box (:line-width 1 :color "#37f" :style released-button)))))))

(custom-theme-set-variables
 '8color-brown
 '(ansi-color-names-vector ["#000" "#f00" "#0f0" "#ff0" "#00f" "#f0f" "#0ff" "#fff"]))

(provide-theme '8color-brown)

;;; 8color-brown-theme.el ends here
