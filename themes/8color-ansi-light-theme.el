;;; 8color-ansi-light-theme.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Faces
;; ============================================================================
;; These ansi colors work in an 8 or 16 color terminal.
;; Error/warning/success have the traffic light colors red/yellow/green.
;; The low contrast blue on black is shadow/decorations.
;; ----------------------------------------------------------------------------
(deftheme 8color-ansi-light
  "8 \"#???\" color theme combining f and 0.
\n| #fff | background      | #000 | default text    |
| #f00 | error/todo      | #f0f | warning/heading |
| #00f | success/done    | #0ff | comment/tag     |
| #0f0 | shadow/hl-line  | #ff0 | other/timestamp |"
  :background-mode 'dark
  :kind 'color-scheme
  :family '8color)

(let ((class '((class color) (min-colors 8))))
  (custom-theme-set-faces
   '8color-ansi-light
   ;; ----------------------------------------------------------------------------
   ;; Special faces.
   `(default
     ((,class (:foreground "#000" :background "#fff"))))
   `(error
     ((,class (:foreground "#f00" :underline (:style wave :color "#f0f")))))
   `(warning
     ((,class (:foreground "#f0f"))))
   `(success
     ((,class (:foreground "#00f"))))
   ;; ----------------------------------------------------------------------------
   ;; font-lock.
   `(font-lock-comment-face
     ((,class (:foreground "#0ff" :slant italic))))
   `(font-lock-string-face
     ((,class (:foreground "#0ff"))))
   `(font-lock-builtin-face
     ((,class (:foreground "#ff0"))))
   `(font-lock-type-face
     ((,class (:foreground "#ff0"))))
   `(font-lock-constant-face
     ((,class (:foreground "#ff0"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#00f"))))
   `(font-lock-variable-name-face
     ((,class (:foreground "#00f"))))
   `(font-lock-keyword-face
     ((,class (:foreground "#f0f"))))
   `(font-lock-number-face
     ((,class (:foreground "#f00"))))
   `(font-lock-warning-face
     ((,class (:underline nil))))
   ;; ----------------------------------------------------------------------------
   ;; Decorations.
   `(vertical-border
     ((,class (:foreground "#0f0"))))
   `(scroll-bar
     ((,class (:foreground "#0f0"))))
   `(region
     ((,class (:background "#0f0"))))
   `(shadow
     ((,class (:foreground "#0f0"))))
   `(match
     ((,class (:background "#0f0"))))
   `(highlight
     ((,class (:background "#0f0"))))
   `(lazy-highlight
     ((,class (:background "#0f0"))))
   `(isearch
     ((,class (:foreground "#fff" :background "#000"))))
   `(isearch-fail
     ((,class (:foreground "#fff" :background "#f0f"))))
   `(fringe
     ((,class (:foreground "#0f0" :background "#fff"))))
   `(line-number
     ((,class (:foreground "#0f0" :background "#fff" :height .8))))
   `(line-number-current-line
     ((,class (:inherit 'line-number :foreground "#fff" :background "#0f0" :weight bold))))
   `(mode-line
     ((,class (:foreground "#fff" :background "#0f0" :box nil :height .8))))
   `(mode-line-inactive
     ((,class (:foreground "#0f0" :background "#fff" :box nil :overline t :height .8))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color "#0ff" :style released-button)))))

   ;; ============================================================================
;;; Org
   ;; ============================================================================
   `(org-document-title
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-1
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-2
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-3
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-4
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-5
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-6
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-7
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-level-8
     ((,class (:foreground "#f0f" :weight bold :height 1.25))))
   `(org-todo
     ((,class (:foreground "#f00" :height .8))))
   `(org-done
     ((,class (:foreground "#00f" :height .8))))
   `(org-headline-done
     ((,class (:foreground "#0ff"))))
   `(org-ellipsis
     ((,class (:foreground "#0ff" :weight normal :underline nil :height .8))))
   `(org-document-info-keyword
     ((,class (:foreground "#0ff" :weight normal :height .8))))
   `(org-special-keyword
     ((,class (:foreground "#0ff" :weight normal :height .8))))
   `(org-checkbox
     ((,class (:foreground "#0ff" :background "#fff" :box nil :height .8))))
   `(org-tag
     ((,class (:foreground "#0ff" :weight normal))))
   `(org-code
     ((,class (:foreground "#ff0"))))
   `(org-verbatim
     ((,class (:foreground "#ff0" :box (:line-width 1 :color "#0ff" :style released-button)))))
   `(org-table
     ((,class (:foreground "#0ff" :height .8))))
   `(org-formula
     ((,class (:foreground "#0ff" :height .8))))
   `(org-block
     ((,class (:foreground "#000"))))
   `(org-block-begin-line
     ((,class (:foreground "#ff0" :height .8))))
   `(org-block-end-line
     ((,class (:foreground "#ff0"))))
   `(org-drawer
     ((,class (:foreground "#ff0" :height .8))))
   `(org-priority
     ((,class (:foreground "#ff0" :height .8))))
   `(org-footnote
     ((,class (:foreground "#ff0" :underline nil :height .8))))
   `(org-date
     ((,class (:foreground "#ff0" :underline nil :height .8))))
   `(org-link
     ((,class (:foreground "#ff0"))))
   ;; ----------------------------------------------------------------------------
   ;; Bullets.
   `(org-superstar-leading
     ((,class (:foreground "#0f0" :height .8))))
   `(org-superstar-header-bullet
     ((,class (:foreground "#0ff" :slant normal :height .8))))
   `(org-superstar-item
     ((,class (:foreground "#f0f" :height .8))))

   ;; ============================================================================
;;; Agenda
   ;; ============================================================================
   `(header-line
     ((,class (:foreground "#f0f" :background "#fff" :weight bold :height 1.25))))
   `(org-agenda-structure
     ((,class (:foreground "#f0f" :background "#fff" :box nil :weight bold :height 1.25))))
   `(org-column
     ((,class (:background "#fff"))))
   `(org-warning
     ((,class (:foreground "#f00"))))
   `(org-agenda-done
     ((,class (:foreground "#0ff" :slant normal))))
   `(org-time-grid
     ((,class (:foreground "#0ff"))))
   `(calendar-weekday-header
     ((,class (:foreground "#0ff"))))
   `(org-agenda-calendar-event
     ((,class (:foreground "#000"))))
   `(org-agenda-clocking
     ((,class (:foreground "#0ff" :background "#0f0"))))
   `(org-agenda-date
     ((,class (:foreground "#0ff" :background "#fff" :box nil :weight normal))))
   `(org-agenda-date-weekend
     ((,class (:foreground "#0ff" :background "#fff" :box nil :weight normal :underline nil))))
   `(org-agenda-date-today
     ((,class (:foreground "#ff0" :background "#fff" :box nil :weight normal :underline nil :slant normal :inverse-video nil))))
   `(org-upcoming-distant-deadline
     ((,class (:foreground "#00f"))))
   `(org-upcoming-deadline
     ((,class (:foreground "#00f"))))
   `(org-imminent-deadline
     ((,class (:foreground "#f0f" :weight normal))))
   `(org-scheduled
     ((,class (:foreground "#00f"))))
   `(org-scheduled-today
     ((,class (:foreground "#00f"))))
   `(org-scheduled-previously
     ((,class (:foreground "#f0f"))))
   ;; ----------------------------------------------------------------------------
   ;; Habit.
   `(org-habit-alert-face
     ((,class (:foreground "#0ff" :background "#f0f" :weight bold))))
   `(org-habit-alert-future-face
     ((,class (:background "#f0f"))))
   `(org-habit-overdue-face
     ((,class (:foreground "#0ff" :background "#f00" :weight bold))))
   `(org-habit-overdue-future-face
     ((,class (:background "#ff0"))))
   `(org-habit-ready-face
     ((,class (:foreground "#0ff" :background "#00f" :weight bold))))
   `(org-habit-ready-future-face
     ((,class (:background "#00f"))))
   `(org-habit-clear-face
     ((,class (:foreground "#0ff" :background "#0f0" :weight bold))))
   `(org-habit-clear-future-face
     ((,class (:background "#0f0"))))

   ;; ============================================================================
;;; Misc. other packages
   ;; ============================================================================
   `(help-key-binding
     ((,class (:foreground "#000" :background "#0f0" :weight bold))))
   `(show-paren-match
     ((,class (:foreground "#000" :background "#0f0" :weight bold))))
   `(keycast-key
     ((,class (:foreground "#fff" :background "#0f0" :box t :height .8))))
   `(corfu-default
     ((,class (:foreground "#0f0" :background "#fff"))))
   `(corfu-current
     ((,class (:foreground "#fff" :background "#0f0"))))
   `(aw-leading-char-face
     ((,class (:foreground "#f0f" :height 1.0))))
   `(ivy-current-match ; `counsel' use this face.
     ((,class (:foreground "#00f" :background "#0f0"))))
   `(indent-guide-face
     ((,class (:inherit 'shadow))))
   `(dired-ignored
     ((,class (:foreground "#0ff"))))
   `(dired-subtree-depth-1-face
     ((,class (:background "#0f0"))))
   `(dired-subtree-depth-2-face
     ((,class (:background "#fff"))))
   `(dired-subtree-depth-3-face
     ((,class (:background "#0f0"))))
   `(dired-subtree-depth-4-face
     ((,class (:background "#fff"))))
   `(dired-subtree-depth-5-face
     ((,class (:background "#0f0"))))
   `(marginalia-documentation
     ((,class (:foreground "#0ff"))))
   `(corfu-default
     ((,class (:foreground "#0f0" :background "#fff"))))
   `(corfu-current
     ((,class (:foreground "#fff" :background "#0f0"))))
   `(flyspell-duplicate
     ((,class (:underline (:style wave :color "#f0f")))))
   `(flyspell-incorrect
     ((,class (:underline (:style wave :color "#f00")))))
   `(rainbow-delimiters-base-error-face
     ((,class (:foreground "#000" :background "#f00" :weight bold))))
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground "#00f"))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground "#f0f"))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground "#ff0"))))
   `(tab-bar
     ((,class (:foreground "#0f0" :background "#fff" :weight bold :box nil :height .8))))
   `(tab-bar-tab
     ((,class (:foreground "#fff" :background "#0f0" :box t))))
   `(tab-bar-tab-inactive
     ((,class (:foreground "#0f0" :background "#fff" :box nil))))
   (unless (version<= emacs-version "31.0")
     `(tab-bar-tab-highlight
       ((,class (:foreground "#fff" :background "#0f0" :box (:line-width 1 :color "#0ff" :style released-button))))))
   `(tab-line
     ((,class (:foreground "#0f0" :background "#fff" :overline t :box nil))))
   `(tab-line-tab
     ((,class (:box nil :inherit 'tab-line))))
   `(tab-line-tab-current
     ((,class (:foreground "#fff" :background "#0f0" :box nil))))
   `(tab-line-tab-modified
     ((,class (:foreground "#fff" :background "#0f0" :box nil))))
   `(tab-line-tab-inactive
     ((,class (:foreground "#0f0" :background "#fff" :box nil))))
   `(tab-line-highlight
     ((,class (:foreground "#0f0" :background "#fff" :box (:line-width 1 :color "#0ff" :style released-button)))))))

(custom-theme-set-variables
 '8color-ansi-light
 '(ansi-color-names-vector ["#000" "#f00" "#0f0" "#ff0" "#00f" "#f0f" "#0ff" "#fff"]))

(provide-theme '8color-ansi-light)

;;; 8color-ansi-light-theme.el ends here
