;;; 8color-ansi-theme.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 8 colors: #??? combining f and 0.
;; |------+-----------------+------+-----------------|
;; | #000 | background      | #fff | default text    |
;; |------+-----------------+------+-----------------|
;; | #f00 | shadow/hl-line  | #0ff | comment/tag     |
;; | #0f0 | success/done    | #f0f | error/todo      |
;; | #00f | link/timestamp  | #ff0 | warning/heading |
;; |------+-----------------+------+-----------------|
;; These colors will display in an 8 or 16 color ansi terminal.
;; Unspecified faces are handled by the dark vanilla theme.
(deftheme 8color-ansi)
(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   '8color-ansi
   ;; ----------------------------------------------------------------------------
;;;; Special faces.
   `(default
     ((,class (:foreground "#fff" :background "#000"))))
   `(error
     ((,class (:foreground "#f0f" :underline t))))
   `(warning
     ((,class (:foreground "#ff0"))))
   `(success
     ((,class (:foreground "#0f0"))))
   ;; ----------------------------------------------------------------------------
;;;; font-lock.
   `(font-lock-comment-face
     ((,class (:foreground "#0ff" :slant italic))))
   `(font-lock-string-face
     ((,class (:foreground "#0ff"))))
   `(font-lock-builtin-face
     ((,class (:foreground "#00f"))))
   `(font-lock-type-face
     ((,class (:foreground "#00f"))))
   `(font-lock-constant-face
     ((,class (:foreground "#00f"))))
   `(font-lock-function-name-face
     ((,class (:foreground "#0f0"))))
   `(font-lock-variable-name-face
     ((,class (:foreground "#0f0"))))
   `(font-lock-keyword-face
     ((,class (:foreground "#ff0"))))
   `(font-lock-number-face
     ((,class (:foreground "#f0f"))))
   `(font-lock-warning-face
     ((,class (:underline nil))))
   ;; ----------------------------------------------------------------------------
;;;; Decorations.
   `(vertical-border
     ((,class (:foreground "#f00"))))
   `(scroll-bar
     ((,class (:foreground "#f00"))))
   `(region
     ((,class (:background "#f00"))))
   `(shadow
     ((,class (:foreground "#f00"))))
   `(match
     ((,class (:background "#f00"))))
   `(highlight
     ((,class (:background "#f00"))))
   `(lazy-highlight
     ((,class (:background "#f00"))))
   `(isearch
     ((,class (:foreground "#000" :background "#fff"))))
   `(fringe
     ((,class (:foreground "#f00" :background "#000"))))
   `(line-number
     ((,class (:foreground "#f00" :background "#000" :height .8))))
   `(line-number-current-line
     ((,class (:foreground "#000" :background "#f00" :weight bold :height .8))))
   `(mode-line
     ((,class (:foreground "#000" :background "#f00" :box nil :height .8))))
   `(mode-line-inactive
     ((,class (:foreground "#f00" :background "#000" :box nil :overline t :height .8))))
   `(mode-line-highlight
     ((,class (:box (:line-width 1 :color "#0ff" :style released-button))))) ; (:line-width 1 :color "#0ff" :style released-button)

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
     ((,class (:foreground "#f0f" :height .8))))
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
     ((,class (:foreground "#00f"))))
   `(org-verbatim
     ((,class (:foreground "#00f" :box (:line-width 1 :color "#0ff" :style released-button)))))
   `(org-table
     ((,class (:foreground "#0ff" :height .8))))
   `(org-formula
     ((,class (:foreground "#0ff" :height .8))))
   `(org-block
     ((,class (:foreground "#fff"))))
   `(org-block-begin-line
     ((,class (:foreground "#00f" :height .8))))
   `(org-block-end-line
     ((,class (:foreground "#00f"))))
   `(org-drawer
     ((,class (:foreground "#00f" :height .8))))
   `(org-priority
     ((,class (:foreground "#00f" :height .8))))
   `(org-footnote
     ((,class (:foreground "#00f" :underline nil :height .8))))
   `(org-date
     ((,class (:foreground "#00f" :underline nil :height .8))))
   `(org-link
     ((,class (:foreground "#00f"))))
   ;; ----------------------------------------------------------------------------
;;;; Bullets.
   `(org-superstar-leading
     ((,class (:foreground "#f00" :height .8))))
   `(org-superstar-header-bullet
     ((,class (:foreground "#0ff" :slant normal :height .8))))
   `(org-superstar-item
     ((,class (:foreground "#ff0" :height .8))))
   ;; ============================================================================
;;;; Agenda
   ;; ============================================================================
   `(header-line
     ((,class (:foreground "#ff0" :background "#000" :weight bold :height 1.25))))
   `(org-agenda-structure
     ((,class (:foreground "#ff0" :background "#000" :box nil :weight bold :height 1.25))))
   `(org-column
     ((,class (:background "#000"))))
   `(org-warning
     ((,class (:foreground "#f0f"))))
   `(org-agenda-done
     ((,class (:foreground "#0ff" :slant normal))))
   `(org-time-grid
     ((,class (:foreground "#0ff"))))
   `(calendar-weekday-header
     ((,class (:foreground "#0ff"))))
   `(org-agenda-calendar-event
     ((,class (:foreground "#fff"))))
   `(org-agenda-clocking
     ((,class (:foreground "#0ff" :background "#f00"))))
   `(org-agenda-date
     ((,class (:foreground "#00f" :background "#000" :box nil :weight normal))))
   `(org-agenda-date-weekend
     ((,class (:foreground "#00f" :background "#000" :box nil :weight normal :underline nil))))
   `(org-agenda-date-today
     ((,class (:foreground "#ff0" :background "#000" :box nil :weight normal :slant normal :inverse-video nil))))
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
;;;; Habit.
   `(org-habit-alert-face
     ((,class (:foreground "#f0f" :background "#ff0" :weight bold))))
   `(org-habit-alert-future-face
     ((,class (:background "#ff0"))))
   `(org-habit-overdue-face
     ((,class (:foreground "#ff0" :background "#f0f" :weight bold))))
   `(org-habit-overdue-future-face
     ((,class (:background "#f00"))))
   `(org-habit-ready-face
     ((,class (:foreground "#ff0" :background "#0f0" :weight bold))))
   `(org-habit-ready-future-face
     ((,class (:background "#0f0"))))
   `(org-habit-clear-face
     ((,class (:foreground "#ff0" :background "#f00" :weight bold))))
   `(org-habit-clear-future-face
     ((,class (:background "#f00"))))

   ;; ============================================================================
;;; Misc. other packages
   ;; ============================================================================
   `(show-paren-match
     ((,class (:foreground "#fff" :background "#f00" :weight bold))))
   `(keycast-key
     ((,class (:foreground "#000" :background "#f00" :box t :height .8))))
   `(corfu-default
     ((,class (:foreground "#f00" :background "#000"))))
   `(corfu-current
     ((,class (:foreground "#000" :background "#f00"))))
   `(aw-leading-char-face
     ((,class (:foreground "#ff0" :height 1.0))))
   `(ivy-current-match
     ((,class (:foreground "#0f0" :background "#f00"))))
   `(indent-guide-face
     ((,class (:foreground "#f00"))))
   `(dired-ignored
     ((,class (:foreground "#0ff"))))
   `(dired-subtree-depth-1-face
     ((,class (:background "#f00"))))
   `(dired-subtree-depth-2-face
     ((,class (:background "#000"))))
   `(dired-subtree-depth-3-face
     ((,class (:background "#f00"))))
   `(dired-subtree-depth-4-face
     ((,class (:background "#000"))))
   `(dired-subtree-depth-5-face
     ((,class (:background "#f00"))))
   `(marginalia-documentation
     ((,class (:foreground "#0ff"))))
   `(corfu-default
     ((,class (:foreground "#f00" :background "#000"))))
   `(corfu-current
     ((,class (:foreground "#000" :background "#f00"))))
   `(flyspell-duplicate
     ((,class (:underline (:style wave :color "#ff0")))))
   `(flyspell-incorrect
     ((,class (:underline (:style wave :color "#f0f")))))
   `(rainbow-delimiters-base-error-face
     ((,class (:foreground "#fff" :background "#f0f" :weight bold))))
   `(rainbow-delimiters-depth-1-face
     ((,class (:foreground "#0f0"))))
   `(rainbow-delimiters-depth-2-face
     ((,class (:foreground "#00f"))))
   `(rainbow-delimiters-depth-3-face
     ((,class (:foreground "#ff0"))))
   `(tab-bar
     ((,class (:foreground "#f00" :background "#000" :weight bold :box nil :height .8 :inherit 'default))))
   `(tab-bar-tab
     ((,class (:foreground "#000" :background "#f00" :box t))))
   `(tab-bar-tab-inactive
     ((,class (:foreground "#f00" :background "#000" :box nil))))
   (unless (version<= emacs-version "31.0")
     `(tab-bar-tab-highlight
       ((,class (:foreground "#000" :background "#f00" :box (:line-width 1 :color "#0ff" :style released-button))))))
   `(tab-line
     ((,class (:foreground "#f00" :background "#000" :overline t :box nil))))
   `(tab-line-tab
     ((,class (:box nil :inherit 'tab-line))))
   `(tab-line-tab-current
     ((,class (:foreground "#000" :background "#f00" :box nil))))
   `(tab-line-tab-modified
     ((,class (:foreground "#000" :background "#f00" :box nil))))
   `(tab-line-tab-inactive
     ((,class (:foreground "#f00" :background "#000" :box nil))))
   `(tab-line-highlight
     ((,class (:foreground "#f00" :background "#000" :box (:line-width 1 :color "#0ff" :style released-button)))))))

(custom-theme-set-variables
 '8color-ansi
 '(ansi-color-names-vector ["#000" "#f00" "#0f0" "#ff0" "#00f" "#f0f" "#0ff" "#fff"]))

(provide-theme '8color-ansi)
;;; 8color-ansi-theme.el ends here
