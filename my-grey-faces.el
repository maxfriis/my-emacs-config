;; -*- lexical-binding: t; -*-
;; #+title: my-faces.el

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 `sizes': small (4/5=0.8), normal (1.0) and large (5/4=1.25).
;; |------+-----------------+------+-----------------|
;; | #000 | background      | #fff | default text    |
;; |------+-----------------+------+-----------------|
;; | #555 | error/todo      | #999 | warning/heading |
;; | #ddd | success/done    | #bbb | comment/tag     |
;; | #777 | link/timestamp  | #333 | shadow/hl-line  |
;; |------+-----------------+------+-----------------|
;; Dark, simple, and systematic.
;; ----------------------------------------------------------------------------
;; When I configure `evil' I add 7 cursor colors indicating the evil state.
;; Unspecified faces are handled by the dark part of the vanilla theme.
;; ----------------------------------------------------------------------------
;; Special faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'default
 nil :foreground "#fff" :background "#000")
(set-face-attribute
 'error
 nil :foreground "#555" :underline t)
(set-face-attribute
 'warning
 nil :foreground "#999")
(set-face-attribute
 'success
 nil :foreground "#ddd")
;; ----------------------------------------------------------------------------
;; Decorations
;; ----------------------------------------------------------------------------
(set-face-attribute
 'region
 nil :background "#333")
(set-face-attribute
 'shadow
 nil :foreground "#333")
(set-face-attribute
 'match
 nil :background "#333")
(set-face-attribute
 'highlight
 nil :background "#333")
(set-face-attribute
 'vertical-border
 nil :foreground "#333")
(set-face-attribute
 'fringe
 nil :foreground "#333" :background "#000")
(set-face-attribute
 'line-number
 nil :foreground "#333" :background "#000" :height 0.8)
(set-face-attribute
 'line-number-current-line
 nil :foreground "#000" :background "#333" :weight 'bold)
(set-face-attribute
 'mode-line
 nil :foreground "#000" :background "#333" :box nil :height 0.8)
(set-face-attribute
 'mode-line-inactive
 nil :foreground "#333" :background "#000" :box nil :overline t)
;; ----------------------------------------------------------------------------
;; font-lock faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'font-lock-builtin-face
 nil :foreground "#ddd")
(set-face-attribute
 'font-lock-comment-face
 nil :foreground "#bbb")
(set-face-attribute
 'font-lock-constant-face
 nil :foreground "#999")
(set-face-attribute
 'font-lock-function-name-face
 nil :foreground "#ddd")
(set-face-attribute
 'font-lock-keyword-face
 nil :foreground "#777")
(set-face-attribute
 'font-lock-string-face
 nil :foreground "#bbb")
(set-face-attribute
 'font-lock-warning-face
 nil :underline nil)
;; ----------------------------------------------------------------------------
;; Define more keywords
;; ----------------------------------------------------------------------------
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("add-hook"           . font-lock-keyword-face)
   ("add-to-list"        . font-lock-keyword-face)
   ("set-face-attribute" . font-lock-keyword-face)))

;; ============================================================================
;;; Org faces
;; ============================================================================
(with-eval-after-load 'org
  (set-face-attribute
   'org-document-title
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-1
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-2
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-3
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-4
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-5
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-6
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-7
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-8
   nil :foreground "#999" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-todo
   nil :foreground "#555" :height 0.8)
  (set-face-attribute
   'org-done
   nil :foreground "#ddd" :height 0.8)
  (set-face-attribute
   'org-headline-done
   nil :foreground "#bbb")
  (set-face-attribute
   'org-ellipsis
   nil :foreground "#bbb" :weight 'normal :underline nil :height 0.8)
  (set-face-attribute
   'org-document-info-keyword
   nil :foreground "#bbb" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-special-keyword
   nil :foreground "#bbb" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-checkbox
   nil :foreground "#bbb" :background "#000" :box nil :height 0.8)
  (set-face-attribute
   'org-tag
   nil :foreground "#bbb" :weight 'normal)
  (set-face-attribute
   'org-formula
   nil :foreground "#bbb" :height 0.8)
  (set-face-attribute
   'org-code
   nil :foreground "#bbb" :height 0.8)
  (set-face-attribute
   'org-verbatim
   nil :foreground "#bbb" :height 0.8)
  (set-face-attribute
   'org-table
   nil :foreground "#bbb" :height 0.8)
  (set-face-attribute
   'org-block
   nil :foreground "#fff")
  (set-face-attribute
   'org-block-begin-line
   nil :foreground "#777" :height 0.8)
  (set-face-attribute
   'org-block-end-line
   nil :foreground "#777")
  (set-face-attribute
   'org-drawer
   nil :foreground "#777" :height 0.8)
  (set-face-attribute
   'org-footnote
   nil :foreground "#777" :underline nil :height 0.8)
  (set-face-attribute
   'org-date
   nil :foreground "#777" :underline nil :height 0.8)
  (set-face-attribute
   'org-link
   nil :foreground "#777"))
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-superstar
  (set-face-attribute
   'org-superstar-leading
   nil :foreground "#333" :height 0.8) ; the dots marking the deapt
  (set-face-attribute
   'org-superstar-header-bullet
   nil :foreground "#999" :height 0.8) ; the header bullet face
  (set-face-attribute
   'org-superstar-item
   nil :foreground "#999" :height 0.8)) ; the item bullet face
;; ============================================================================
;;;; Agenda
;; ============================================================================
(with-eval-after-load 'org-agenda
  (set-face-attribute
   'header-line
   nil :foreground "#999" :background "#000" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-agenda-structure
   nil :foreground "#999" :background "#000" :box nil :weight 'bold :height 1.25)
  (set-face-attribute
   'org-column
   nil :background "#000")
  (set-face-attribute
   'org-warning
   nil :foreground "#555")
  (set-face-attribute
   'org-agenda-done
   nil :foreground "#bbb" :slant 'normal)
  (set-face-attribute
   'org-time-grid
   nil :foreground "#bbb")
  (set-face-attribute
   'calendar-weekday-header
   nil :foreground "#bbb")
  (set-face-attribute
   'org-agenda-calendar-event
   nil :foreground "#fff")
  (set-face-attribute
   'org-agenda-clocking
   nil :foreground "#bbb" :background "#333")
  (set-face-attribute
   'org-agenda-date
   nil :foreground "#777" :background "#000" :box nil :weight 'normal :height 0.8)
  (set-face-attribute
   'org-agenda-date-weekend
   nil :foreground "#777" :background "#000" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today
   nil :foreground "#999" :background "#000" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-upcoming-distant-deadline
   nil :foreground "#ddd")
  (set-face-attribute
   'org-upcoming-deadline
   nil :foreground "#ddd")
  (set-face-attribute
   'org-imminent-deadline
   nil :foreground "#999" :weight 'normal)
  (set-face-attribute
   'org-scheduled
   nil :foreground "#ddd")
  (set-face-attribute
   'org-scheduled-today
   nil :foreground "#ddd")
  (set-face-attribute
   'org-scheduled-previously
   nil :foreground "#999"))
;; ----------------------------------------------------------------------------
;; Habit
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-habit
  (set-face-attribute
   'org-habit-alert-face
   nil :foreground "#555" :background "#999" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-alert-future-face
   nil :background "#999" :height 0.8)
  (set-face-attribute
   'org-habit-overdue-face
   nil :foreground "#999" :background "#555" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-overdue-future-face
   nil :background "#333" :height 0.8)
  (set-face-attribute
   'org-habit-ready-face
   nil :foreground "#999" :background "#ddd" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-ready-future-face
   nil :background "#ddd" :height 0.8)
  (set-face-attribute
   'org-habit-clear-face
   nil :foreground "#999" :background "#333" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-clear-future-face
   nil :background "#333" :height 0.8))

;; ============================================================================
;;; Misc. other package faces
;; ============================================================================
(set-face-attribute
 'show-paren-match
 nil :foreground "#fff" :background "#333" :weight 'bold)
(with-eval-after-load 'keycast
  (set-face-attribute
   'keycast-key
   nil :foreground "#000" :background "#333" :box t :height 0.8))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#333" :background "#000")
  (set-face-attribute
   'corfu-current
   nil :foreground "#000" :background "#333"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#bbb"))
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face
   nil :foreground "#999" :height 1.0))
(with-eval-after-load 'counsel
  (set-face-attribute
   'ivy-current-match ; counsel use this ivy face
   nil :foreground "#ddd" :background "#333"))
(with-eval-after-load 'indent-guide
  (set-face-attribute
   'indent-guide-face
   nil :foreground "#333"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#bbb"))
(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute
   'rainbow-delimiters-base-error-face
   nil :foreground "#fff" :background "#555" :weight 'bold :underline t)
  (set-face-attribute
   'rainbow-delimiters-depth-1-face
   nil :foreground "#ddd")
  (set-face-attribute
   'rainbow-delimiters-depth-2-face
   nil :foreground "#555")
  (set-face-attribute
   'rainbow-delimiters-depth-3-face
   nil :foreground "#999"))
(with-eval-after-load 'tab-bar
  (set-face-attribute
   'tab-bar
   nil :foreground "#333" :background "#000" :weight 'bold :box nil :height 0.8 :inherit 'default)
  (set-face-attribute
   'tab-bar-tab
   nil :foreground "#000" :background "#333" :box t)
  (set-face-attribute
   'tab-bar-tab-inactive
   nil :foreground "#333" :background "#000" :box nil))
(with-eval-after-load 'tab-line
  (set-face-attribute
   'tab-line
   nil :foreground "#333" :background "#000" :overline t :box nil)
  (set-face-attribute
   'tab-line-tab ;; active tab in another frame
   nil :box nil :inherit 'tab-line)
  (set-face-attribute
   'tab-line-tab-current
   nil :foreground "#000" :background "#333" :box nil)
  (set-face-attribute
   'tab-line-tab-modified
   nil :foreground "#000" :background "#333" :box nil)
  (set-face-attribute
   'tab-line-tab-inactive
   nil :foreground "#333" :background "#000" :box nil)
  (set-face-attribute
   'tab-line-highlight ;; mouseover
   nil :foreground "#fff" :background "#000" :box nil))
;; End of my-faces.el
