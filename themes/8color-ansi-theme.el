;;; 8color-ansi-theme.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Faces
;; ============================================================================
;; These ansi colors work in an 8 or 16 color terminal.
;; Error/warning/success have the traffic light colors red/yellow/green.
;; The lowest contrast blue on black is shadow/hl-line/decorations.
;; ----------------------------------------------------------------------------
(deftheme 8color-ansi
  "8 \"#???\" color theme combining f and 0.
\n| #000 | background      | #fff | default text    |
| #f00 | error/todo      | #ff0 | warning/heading |
| #0f0 | success/done    | #0ff | comment/tag     |
| #00f | shadow/hl-line  | #f0f | other/timestamp |"
  :background-mode 'dark
  :kind 'color-scheme
  :family '8color)

(let ((class '((class color) (min-colors 8))))
  (custom-theme-set-faces
   '8color-ansi
   ;; ----------------------------------------------------------------------------
   ;; Special faces.
   `(default
     ((,class (:foreground "#fff" :background "#000"))))
   `(error
     ((,class (:foreground "#f00" :underline (:style wave :color "#ff0")))))
   `(warning
     ((,class (:foreground "#ff0"))))
   `(success
     ((,class (:foreground "#0f0"))))
   ;; ----------------------------------------------------------------------------
   ;; font-lock.
   `(font-lock-comment-face
     ((,class (:foreground "#0ff" :slant italic))))
   `(font-lock-string-face
     ((,class (:foreground "#0ff"))))
   `(font-lock-builtin-face
     ((,class (:foreground "#f0f"))))
   `(font-lock-type-face
     ((,class (:foreground "#f0f"))))
   `(font-lock-constant-face
     ((,class (:foreground "#f0f"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#0f0"))))
   `(font-lock-variable-name-face
     ((,class (:foreground "#0f0"))))
   `(font-lock-keyword-face
     ((,class (:foreground "#ff0"))))
   `(font-lock-number-face
     ((,class (:foreground "#f00"))))
   `(font-lock-warning-face
     ((,class (:underline nil))))
   ;; ----------------------------------------------------------------------------
   ;; Decorations.
   `(vertical-border
     ((,class (:foreground "#00f"))))
   `(scroll-bar
     ((,class (:foreground "#00f"))))
   `(region
     ((,class (:background "#00f"))))
   `(shadow
     ((,class (:foreground "#00f"))))
   `(match
     ((,class (:background "#00f"))))
   `(highlight
     ((,class (:background "#00f"))))
   `(lazy-highlight
     ((,class (:background "#00f"))))
   `(isearch
     ((,class (:foreground "#000" :background "#fff"))))
   `(isearch-fail
     ((,class (:foreground "#000" :background "#ff0"))))
   `(fringe
     ((,class (:foreground "#00f" :background "#000"))))
   `(line-number
     ((,class (:foreground "#00f" :background "#000" :height .8))))
   `(line-number-current-line
     ((,class (:inherit 'line-number :foreground "#000" :background "#00f" :weight bold))))
   `(mode-line
     ((,class (:foreground "#000" :background "#00f" :box nil :height .8))))
   `(mode-line-inactive
     ((,class (:foreground "#00f" :background "#000" :box nil :overline t :height .8))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color "#0ff" :style released-button)))))

   ;; ============================================================================
;;; Org
   ;; ============================================================================
   `(org-document-title
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-1
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-2
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-3
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-4
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-5
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-6
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-7
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-level-8
     ((,class (:foreground "#ff0" :weight bold :height 1.25))))
   `(org-todo
     ((,class (:foreground "#f00" :height .8))))
   `(org-done
     ((,class (:foreground "#0f0" :height .8))))
   `(org-headline-done
     ((,class (:foreground "#0ff"))))
   `(org-ellipsis
     ((,class (:foreground "#0ff" :weight normal :underline nil :height .8))))
   `(org-document-info-keyword
     ((,class (:foreground "#0ff" :weight normal :height .8))))
   `(org-special-keyword
     ((,class (:foreground "#0ff" :weight normal :height .8))))
   `(org-checkbox
     ((,class (:foreground "#0ff" :background "#000" :box nil :height .8))))
   `(org-tag
     ((,class (:foreground "#0ff" :weight normal))))
   `(org-code
     ((,class (:foreground "#f0f"))))
   `(org-verbatim
     ((,class (:foreground "#f0f" :box (:line-width 1 :color "#0ff" :style released-button)))))
   `(org-table
     ((,class (:foreground "#0ff" :height .8))))
   `(org-formula
     ((,class (:foreground "#0ff" :height .8))))
   `(org-block
     ((,class (:foreground "#fff"))))
   `(org-block-begin-line
     ((,class (:foreground "#f0f" :height .8))))
   `(org-block-end-line
     ((,class (:foreground "#f0f"))))
   `(org-drawer
     ((,class (:foreground "#f0f" :height .8))))
   `(org-priority
     ((,class (:foreground "#f0f" :height .8))))
   `(org-footnote
     ((,class (:foreground "#f0f" :underline nil :height .8))))
   `(org-date
     ((,class (:foreground "#f0f" :underline nil :height .8))))
   `(org-link
     ((,class (:foreground "#f0f"))))
   ;; ----------------------------------------------------------------------------
   ;; Bullets.
   `(org-superstar-leading
     ((,class (:foreground "#00f" :height .8))))
   `(org-superstar-header-bullet
     ((,class (:foreground "#0ff" :slant normal :height .8))))
   `(org-superstar-item
     ((,class (:foreground "#ff0" :height .8))))

   ;; ============================================================================
;;; Agenda
   ;; ============================================================================
   `(header-line
     ((,class (:foreground "#ff0" :background "#000" :weight bold :height 1.25))))
   `(org-agenda-structure
     ((,class (:foreground "#ff0" :background "#000" :box nil :weight bold :height 1.25))))
   `(org-column
     ((,class (:background "#000"))))
   `(org-warning
     ((,class (:foreground "#f00"))))
   `(org-agenda-done
     ((,class (:foreground "#0ff" :slant normal))))
   `(org-time-grid
     ((,class (:foreground "#0ff"))))
   `(calendar-weekday-header
     ((,class (:foreground "#0ff"))))
   `(org-agenda-calendar-event
     ((,class (:foreground "#fff"))))
   `(org-agenda-clocking
     ((,class (:foreground "#0ff" :background "#00f"))))
   `(org-agenda-date
     ((,class (:foreground "#0ff" :background "#000" :box nil :weight normal))))
   `(org-agenda-date-weekend
     ((,class (:foreground "#0ff" :background "#000" :box nil :weight normal :underline nil))))
   `(org-agenda-date-today
     ((,class (:foreground "#f0f" :background "#000" :box nil :weight normal :underline nil :slant normal :inverse-video nil))))
   `(org-upcoming-distant-deadline
     ((,class (:foreground "#0f0"))))
   `(org-upcoming-deadline
     ((,class (:foreground "#0f0"))))
   `(org-imminent-deadline
     ((,class (:foreground "#ff0" :weight normal))))
   `(org-scheduled
     ((,class (:foreground "#0f0"))))
   `(org-scheduled-today
     ((,class (:foreground "#0f0"))))
   `(org-scheduled-previously
     ((,class (:foreground "#ff0"))))
   ;; ----------------------------------------------------------------------------
   ;; Habit.
   `(org-habit-alert-face
     ((,class (:foreground "#0ff" :background "#ff0" :weight bold))))
   `(org-habit-alert-future-face
     ((,class (:background "#ff0"))))
   `(org-habit-overdue-face
     ((,class (:foreground "#0ff" :background "#f00" :weight bold))))
   `(org-habit-overdue-future-face
     ((,class (:background "#f0f"))))
   `(org-habit-ready-face
     ((,class (:foreground "#0ff" :background "#0f0" :weight bold))))
   `(org-habit-ready-future-face
     ((,class (:background "#0f0"))))
   `(org-habit-clear-face
     ((,class (:foreground "#0ff" :background "#00f" :weight bold))))
   `(org-habit-clear-future-face
     ((,class (:background "#00f"))))

   ;; ============================================================================
;;; Misc. other packages
   ;; ============================================================================
   `(help-key-binding
     ((,class (:foreground "#fff" :background "#00f" :weight bold))))
   `(show-paren-match
     ((,class (:foreground "#fff" :background "#00f" :weight bold))))
   `(keycast-key
     ((,class (:foreground "#000" :background "#00f" :box t :height .8))))
   `(corfu-default
     ((,class (:foreground "#00f" :background "#000"))))
   `(corfu-current
     ((,class (:foreground "#000" :background "#00f"))))
   `(aw-leading-char-face
     ((,class (:foreground "#ff0" :height 1.0))))
   `(ivy-current-match ; `counsel' use this face.
     ((,class (:foreground "#0f0" :background "#00f"))))
   `(indent-guide-face
     ((,class (:inherit 'shadow))))
   `(dired-ignored
     ((,class (:foreground "#0ff"))))
   `(dired-subtree-depth-1-face
     ((,class (:background "#00f"))))
   `(dired-subtree-depth-2-face
     ((,class (:background "#000"))))
   `(dired-subtree-depth-3-face
     ((,class (:background "#00f"))))
   `(dired-subtree-depth-4-face
     ((,class (:background "#000"))))
   `(dired-subtree-depth-5-face
     ((,class (:background "#00f"))))
   `(marginalia-documentation
     ((,class (:foreground "#0ff"))))
   `(corfu-default
     ((,class (:foreground "#00f" :background "#000"))))
   `(corfu-current
     ((,class (:foreground "#000" :background "#00f"))))
   `(flyspell-duplicate
     ((,class (:underline (:style wave :color "#ff0")))))
   `(flyspell-incorrect
     ((,class (:underline (:style wave :color "#f00")))))
   `(rainbow-delimiters-base-error-face
     ((,class (:foreground "#fff" :background "#f00" :weight bold))))
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground "#0f0"))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground "#ff0"))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground "#f0f"))))
   `(tab-bar
     ((,class (:foreground "#00f" :background "#000" :weight bold :box nil :height .8))))
   `(tab-bar-tab
     ((,class (:foreground "#000" :background "#00f" :box t))))
   `(tab-bar-tab-inactive
     ((,class (:foreground "#00f" :background "#000" :box nil))))
   (unless (version<= emacs-version "31.0")
     `(tab-bar-tab-highlight
       ((,class (:foreground "#000" :background "#00f" :box (:line-width 1 :color "#0ff" :style released-button))))))
   `(tab-line
     ((,class (:foreground "#00f" :background "#000" :overline t :box nil))))
   `(tab-line-tab
     ((,class (:box nil :inherit 'tab-line))))
   `(tab-line-tab-current
     ((,class (:foreground "#000" :background "#00f" :box nil))))
   `(tab-line-tab-modified
     ((,class (:foreground "#000" :background "#00f" :box nil))))
   `(tab-line-tab-inactive
     ((,class (:foreground "#00f" :background "#000" :box nil))))
   `(tab-line-highlight
     ((,class (:foreground "#00f" :background "#000" :box (:line-width 1 :color "#0ff" :style released-button)))))))

(custom-theme-set-variables
 '8color-ansi
 '(ansi-color-names-vector ["#000" "#f00" "#0f0" "#ff0" "#00f" "#f0f" "#0ff" "#fff"]))

(provide-theme '8color-ansi)

;;; 8color-ansi-theme.el ends here
