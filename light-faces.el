;;; my-ansi-faces.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 sizes:  small (4/5=.8), normal (1.0) and large (5/4=1.25).
;; 8 colors: #???.
;; |------+-----------------+------+-----------------|
;; | #cba | background      | #000 | default text    |
;; |------+-----------------+------+-----------------|
;; | #876 | shadow/hl-line  | #00f | comment/tag     |
;; | #0b0 | success/done    | #f00 | error/todo      |
;; | #b0b | link/timestamp  | #b70 | warning/heading |
;; |------+-----------------+------+-----------------|
;; The colors are based on f, b, 7 and 0 adjusting grays to brownish.
;; Unspecified faces are handled by the light part of the vanilla theme.
;; ----------------------------------------------------------------------------
;;;; Special faces.
(set-face-attribute
 'default
 nil :foreground "#000" :background "#cba")
(set-face-attribute
 'error
 nil :foreground "#f00" :underline t)
(set-face-attribute
 'warning
 nil :foreground "#b70")
(set-face-attribute
 'success
 nil :foreground "#0b0")
;; ----------------------------------------------------------------------------
;;;; font-lock.
(set-face-attribute
 'font-lock-comment-face
 nil :foreground "#00f" :slant 'italic)
(set-face-attribute
 'font-lock-string-face
 nil :foreground "#00f")
(set-face-attribute
 'font-lock-builtin-face
 nil :foreground "#b0f")
(set-face-attribute
 'font-lock-type-face
 nil :foreground "#b0f")
(set-face-attribute
 'font-lock-constant-face
 nil :foreground "#b0f")
(set-face-attribute
 'font-lock-function-name-face
 nil :foreground "#0b0")
(set-face-attribute
 'font-lock-variable-name-face
 nil :foreground "#0b0")
(set-face-attribute
 'font-lock-keyword-face
 nil :foreground "#b70")
(set-face-attribute
 'font-lock-number-face
 nil :foreground "#f00")
(set-face-attribute
 'font-lock-warning-face
 nil :underline nil)
;; ----------------------------------------------------------------------------
;;;; Decorations.
(set-face-attribute
 'vertical-border
 nil :foreground "#876")
(set-face-attribute
 'scroll-bar
 nil :foreground "#876")
(set-face-attribute
 'region
 nil :background "#876")
(set-face-attribute
 'shadow
 nil :foreground "#876")
(set-face-attribute
 'match
 nil :background "#876")
(set-face-attribute
 'highlight
 nil :background "#876")
(set-face-attribute
 'lazy-highlight
 nil :background "#876")
(set-face-attribute
 'isearch
 nil :foreground "#cba" :background "#000")
(set-face-attribute
 'fringe
 nil :foreground "#876" :background "#cba")
(set-face-attribute
 'line-number
 nil :foreground "#876" :background "#cba" :height .8)
(set-face-attribute
 'line-number-current-line
 nil :foreground "#cba" :background "#876" :weight 'bold)
(set-face-attribute
 'mode-line
 nil :foreground "#cba" :background "#876" :box nil :height .8)
(set-face-attribute
 'mode-line-inactive
 nil :foreground "#876" :background "#cba" :box nil :overline t)
(set-face-attribute
 'mode-line-highlight
 nil :box '(:line-width 1 :color "#00f" :style released-button))

;; ============================================================================
;;; Org
;; ============================================================================
(with-eval-after-load 'org
  (set-face-attribute
   'org-document-title
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-1
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-2
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-3
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-4
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-5
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-6
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-7
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-8
   nil :foreground "#b70" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-todo
   nil :foreground "#f00" :height .8)
  (set-face-attribute
   'org-done
   nil :foreground "#0b0" :height .8)
  (set-face-attribute
   'org-headline-done
   nil :foreground "#00f")
  (set-face-attribute
   'org-ellipsis
   nil :foreground "#00f" :weight 'normal :underline nil :height .8)
  (set-face-attribute
   'org-document-info-keyword
   nil :foreground "#00f" :weight 'normal :height .8)
  (set-face-attribute
   'org-special-keyword
   nil :foreground "#00f" :weight 'normal :height .8)
  (set-face-attribute
   'org-checkbox
   nil :foreground "#00f" :background "#cba" :box nil :height .8)
  (set-face-attribute
   'org-tag
   nil :foreground "#00f" :weight 'normal)
  (set-face-attribute
   'org-code
   nil :foreground "#b0f")
  (set-face-attribute
   'org-verbatim
   nil :foreground "#b0f" :box '(:line-width 1 :color "#00f" :style released-button))
  (set-face-attribute
   'org-table
   nil :foreground "#00f" :height .8)
  (set-face-attribute
   'org-formula
   nil :foreground "#00f" :height .8)
  (set-face-attribute
   'org-block
   nil :foreground "#000")
  (set-face-attribute
   'org-block-begin-line
   nil :foreground "#b0f" :height .8)
  (set-face-attribute
   'org-block-end-line
   nil :foreground "#b0f")
  (set-face-attribute
   'org-drawer
   nil :foreground "#b0f" :height .8)
  (set-face-attribute
   'org-priority
   nil :foreground "#b0f" :height .8)
  (set-face-attribute
   'org-footnote
   nil :foreground "#b0f" :underline nil :height .8)
  (set-face-attribute
   'org-date
   nil :foreground "#b0f" :underline nil :height .8)
  (set-face-attribute
   'org-link
   nil :foreground "#b0f"))
;; ----------------------------------------------------------------------------
;;;; Bullets.
(with-eval-after-load 'org-superstar
  (set-face-attribute
   'org-superstar-leading
   nil :foreground "#876" :height .8) ; the dots marking the deapt
  (set-face-attribute
   'org-superstar-header-bullet
   nil :foreground "#00f" :slant 'normal :height .8) ; the header bullet face
  (set-face-attribute
   'org-superstar-item
   nil :foreground "#b70" :height .8)) ; the item bullet face
;; ============================================================================
;;;; Agenda
;; ============================================================================
(with-eval-after-load 'org-agenda
  (set-face-attribute
   'header-line
   nil :foreground "#b70" :background "#cba" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-agenda-structure
   nil :foreground "#b70" :background "#cba" :box nil :weight 'bold :height 1.25)
  (set-face-attribute
   'org-column
   nil :background "#cba")
  (set-face-attribute
   'org-warning
   nil :foreground "#f00")
  (set-face-attribute
   'org-agenda-done
   nil :foreground "#00f" :slant 'normal)
  (set-face-attribute
   'org-time-grid
   nil :foreground "#00f")
  (set-face-attribute
   'calendar-weekday-header
   nil :foreground "#00f")
  (set-face-attribute
   'org-agenda-calendar-event
   nil :foreground "#000")
  (set-face-attribute
   'org-agenda-clocking
   nil :foreground "#00f" :background "#876")
  (set-face-attribute
   'org-agenda-date
   nil :foreground "#b0f" :background "#cba" :box nil :weight 'normal :height .8)
  (set-face-attribute
   'org-agenda-date-weekend
   nil :foreground "#b0f" :background "#cba" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today
   nil :foreground "#b70" :background "#cba" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-upcoming-distant-deadline
   nil :foreground "#0b0")
  (set-face-attribute
   'org-upcoming-deadline
   nil :foreground "#0b0")
  (set-face-attribute
   'org-imminent-deadline
   nil :foreground "#b70" :weight 'normal)
  (set-face-attribute
   'org-scheduled
   nil :foreground "#0b0")
  (set-face-attribute
   'org-scheduled-today
   nil :foreground "#0b0")
  (set-face-attribute
   'org-scheduled-previously
   nil :foreground "#b70"))
;; ----------------------------------------------------------------------------
;;;; Habit.
(with-eval-after-load 'org-habit
  (set-face-attribute
   'org-habit-alert-face
   nil :foreground "#f00" :background "#b70" :weight 'bold)
  (set-face-attribute
   'org-habit-alert-future-face
   nil :background "#b70")
  (set-face-attribute
   'org-habit-overdue-face
   nil :foreground "#b70" :background "#f00" :weight 'bold)
  (set-face-attribute
   'org-habit-overdue-future-face
   nil :background "#876")
  (set-face-attribute
   'org-habit-ready-face
   nil :foreground "#b70" :background "#0b0" :weight 'bold)
  (set-face-attribute
   'org-habit-ready-future-face
   nil :background "#0b0")
  (set-face-attribute
   'org-habit-clear-face
   nil :foreground "#b70" :background "#876" :weight 'bold)
  (set-face-attribute
   'org-habit-clear-future-face
   nil :background "#876"))

;; ============================================================================
;;; Misc. other packages
;; ============================================================================
(set-face-attribute ; matching parenthesis get cursor colors
 'show-paren-match
 nil :foreground "#000" :background "#876" :weight 'bold) ; :slant 'italic
(with-eval-after-load 'keycast
  (set-face-attribute
   'keycast-key
   nil :foreground "#cba" :background "#876" :box t :height .8))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#876" :background "#cba")
  (set-face-attribute
   'corfu-current
   nil :foreground "#cba" :background "#876"))
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face
   nil :foreground "#b70" :height 1.0))
(with-eval-after-load 'counsel
  (set-face-attribute
   'ivy-current-match ; counsel use this face
   nil :foreground "#0b0" :background "#876"))
(with-eval-after-load 'indent-guide
  (set-face-attribute
   'indent-guide-face
   nil :foreground "#876"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#00f"))
(with-eval-after-load 'dired-subtree
  (set-face-attribute
   'dired-subtree-depth-1-face
   nil :background "#876")
  (set-face-attribute
   'dired-subtree-depth-2-face
   nil :background "#cba")
  (set-face-attribute
   'dired-subtree-depth-3-face
   nil :background "#876")
  (set-face-attribute
   'dired-subtree-depth-4-face
   nil :background "#cba")
  (set-face-attribute
   'dired-subtree-depth-5-face
   nil :background "#876"))
(with-eval-after-load 'marginalia
  (set-face-attribute
   'marginalia-documentation
   nil :foreground "#00f"))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#876" :background "#cba")
  (set-face-attribute
   'corfu-current
   nil :foreground "#cba" :background "#876"))
(with-eval-after-load 'flyspell
  (set-face-attribute
   'flyspell-duplicate
   nil :underline '(:style wave :color "#b70"))
  (set-face-attribute
   'flyspell-incorrect
   nil :underline '(:style wave :color "#f00")))
(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute
   'rainbow-delimiters-base-error-face
   nil :foreground "#000" :background "#f00" :weight 'bold)
  (set-face-attribute
   'rainbow-delimiters-depth-1-face
   nil :foreground "#0b0")
  (set-face-attribute
   'rainbow-delimiters-depth-2-face
   nil :foreground "#b0f")
  (set-face-attribute
   'rainbow-delimiters-depth-3-face
   nil :foreground "#b70"))
(with-eval-after-load 'tab-bar
  (set-face-attribute
   'tab-bar
   nil :foreground "#876" :background "#cba" :weight 'bold :box nil :height .8 :inherit 'default)
  (set-face-attribute
   'tab-bar-tab
   nil :foreground "#cba" :background "#876" :box t)
  (set-face-attribute
   'tab-bar-tab-inactive
   nil :foreground "#876" :background "#cba" :box nil)
  (unless (version<= emacs-version "31.0")
    (set-face-attribute
     'tab-bar-tab-highlight
     nil :foreground "#cba" :background "#876" :box '(:line-width 1 :color "#00f" :style released-button))))
(with-eval-after-load 'tab-line
  (set-face-attribute
   'tab-line
   nil :foreground "#876" :background "#cba" :overline t :box nil)
  (set-face-attribute
   'tab-line-tab ;; active tab in another frame
   nil :box nil :inherit 'tab-line)
  (set-face-attribute
   'tab-line-tab-current
   nil :foreground "#cba" :background "#876" :box nil)
  (set-face-attribute
   'tab-line-tab-modified
   nil :foreground "#cba" :background "#876" :box nil)
  (set-face-attribute
   'tab-line-tab-inactive
   nil :foreground "#876" :background "#cba" :box nil)
  (set-face-attribute
   'tab-line-highlight ;; mouseover
   nil :foreground "#876" :background "#cba" :box '(:line-width 1 :color "#00f" :style released-button)))
;;; my-ansi-faces.el ends here
