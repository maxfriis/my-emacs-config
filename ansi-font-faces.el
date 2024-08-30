;; -*- lexical-binding: t; -*-
;; #+title: ansi-colors.el

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 `sizes': small (4/5=0.8), normal (1.0) and large (5/4=1.25).
;; 8 `colors': #??? combining f, a and 5, and some brownish colors.
;; |------+-----------------+------+-----------------|
;; | #000 | background      | #fff | default text    |
;; |------+-----------------+------+-----------------|
;; | #f00 | shadow/hl-line  | #f0f | error/todo      |
;; | #0f0 | success/done    | #ff0 | warning/heading |
;; | #00f | link/timestamp  | #0ff | comment/tag     |
;; |------+-----------------+------+-----------------|
;; Unspecified faces are handled by the vanilla theme.
;; ----------------------------------------------------------------------------
(set-face-attribute
 'default
 nil :foreground "#fff" :background "#000" :font "Ubuntu Mono" :height 200)
(set-face-attribute
 'fixed-pitch
 nil :font "Ubuntu Mono")
(set-face-attribute
 'variable-pitch
 nil :font "Ubuntu" :height 200)
(set-face-attribute
 'error
 nil :foreground "#f0f" :underline t)
(set-face-attribute
 'warning
 nil :foreground "#ff0")
(set-face-attribute
 'success
 nil :foreground "#0f0")
(set-face-attribute
 'shadow
 nil :foreground "#f00")
(set-face-attribute
 'match
 nil :background "#f00")
;; ----------------------------------------------------------------------------
;; font-lock faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'font-lock-builtin-face
 nil :foreground "#0f0")
(set-face-attribute
 'font-lock-comment-face
 nil :foreground "#0ff")
(set-face-attribute
 'font-lock-constant-face
 nil :foreground "#ff0")
(set-face-attribute
 'font-lock-function-name-face
 nil :foreground "#0f0")
(set-face-attribute
 'font-lock-keyword-face
 nil :foreground "#00f")
(set-face-attribute
 'font-lock-string-face
 nil :foreground "#0ff")
(set-face-attribute
 'font-lock-warning-face
 nil :underline nil)
;; ----------------------------------------------------------------------------
;; Decorations
;; ----------------------------------------------------------------------------
(set-face-attribute
 'highlight
 nil :background "#f00")
(set-face-attribute
 'vertical-border
 nil :foreground "#f00")
(set-face-attribute
 'fringe
 nil :foreground "#f00" :background "#000")
(set-face-attribute
 'line-number
 nil :foreground "#f00" :background "#000" :height 0.8)
(set-face-attribute
 'line-number-current-line
 nil :foreground "#000" :background "#f00")
(set-face-attribute
 'mode-line
 nil :foreground "#000" :background "#f00" :box nil :height 0.8)
(set-face-attribute
 'mode-line-inactive
 nil :foreground "#f00" :background "#000" :box nil :overline t)
(set-face-attribute
 'tab-bar
 nil :foreground "#f00" :background "#000" :weight 'bold :box nil :inherit 'default :height 0.8)
(set-face-attribute
 'tab-bar-tab
 nil :foreground "#000" :background "#f00" :box t)
(set-face-attribute
 'tab-bar-tab-inactive
 nil :foreground "#f00" :background "#000" :box nil)
(with-eval-after-load 'tab-line
  (set-face-attribute
   'tab-line
   nil :foreground "#f00" :background "#000" :overline t :box nil)
  (set-face-attribute
   'tab-line-tab ;; active tab in another frame
   nil :inherit 'tab-line :box nil)
  (set-face-attribute
   'tab-line-tab-current
   nil :foreground "#000" :background "#f00" :box nil)
  (set-face-attribute
   'tab-line-tab-modified
   nil :foreground "#000" :background "#f00" :box nil)
  (set-face-attribute
   'tab-line-tab-inactive
   nil :foreground "#f00" :background "#000" :box nil)
  (set-face-attribute
   'tab-line-highlight ;; mouseover
   nil :foreground "#fff" :background "#000" :box nil))
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
   nil :foreground "#ff0" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-1
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-2
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-3
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-4
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-5
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-6
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-7
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-8
   nil :foreground "#ff0" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-todo
   nil :foreground "#f0f" :height 0.8)
  (set-face-attribute
   'org-done
   nil :foreground "#0f0" :height 0.8)
  (set-face-attribute
   'org-headline-done
   nil :foreground "#0ff")
  (set-face-attribute
   'org-ellipsis
   nil :foreground "#0ff" :weight 'normal :underline nil :height 0.8)
  (set-face-attribute
   'org-document-info-keyword
   nil :foreground "#0ff" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-special-keyword
   nil :foreground "#0ff" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-checkbox
   nil :foreground "#0ff" :background "#000" :box nil :height 0.8)
  (set-face-attribute
   'org-tag
   nil :foreground "#0ff" :weight 'normal)
  (set-face-attribute
   'org-formula
   nil :foreground "#0ff" :height 0.8)
  (set-face-attribute
   'org-code
   nil :foreground "#0ff" :height 0.8)
  (set-face-attribute
   'org-verbatim
   nil :foreground "#0ff" :height 0.8)
  (set-face-attribute
   'org-table
   nil :foreground "#0ff" :height 0.8)
  (set-face-attribute
   'org-block
   nil :foreground "#fff")
  (set-face-attribute
   'org-block-begin-line
   nil :foreground "#00f" :height 0.8)
  (set-face-attribute
   'org-block-end-line
   nil :foreground "#00f")
  (set-face-attribute
   'org-drawer
   nil :foreground "#00f" :height 0.8)
  (set-face-attribute
   'org-footnote
   nil :foreground "#00f" :underline nil :height 0.8)
  (set-face-attribute
   'org-date
   nil :foreground "#00f" :underline nil :height 0.8)
  (set-face-attribute
   'org-link
   nil :foreground "#00f")
  (set-face-attribute
   'org-meta-line
   nil :height 1.25))
;; ----------------------------------------------------------------------------
;; Agenda
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-agenda
  (set-face-attribute
   'header-line
   nil :foreground "#ff0" :background "#000" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-agenda-structure
   nil :foreground "#ff0" :background "#000" :box nil :weight 'bold :height 1.0)
  (set-face-attribute
   'org-column
   nil :background "#000")
  (set-face-attribute
   'org-warning
   nil :foreground "#f0f")
  (set-face-attribute
   'org-agenda-done
   nil :foreground "#0ff" :slant 'normal)
  (set-face-attribute
   'org-time-grid
   nil :foreground "#0ff")
  (set-face-attribute
   'calendar-weekday-header
   nil :foreground "#0ff")
  (set-face-attribute
   'org-agenda-calendar-event
   nil :foreground "#fff")
  (set-face-attribute
   'org-agenda-clocking
   nil :foreground "#0ff" :background "#f00")
  (set-face-attribute
   'org-agenda-date
   nil :foreground "#00f" :background "#000" :box nil :weight 'normal)
  (set-face-attribute
   'org-agenda-date-weekend
   nil :foreground "#00f" :background "#000" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today
   nil :foreground "#ff0" :background "#000" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-upcoming-distant-deadline
   nil :foreground "#0f0")
  (set-face-attribute
   'org-upcoming-deadline
   nil :foreground "#0f0")
  (set-face-attribute
   'org-imminent-deadline
   nil :foreground "#ff0" :weight 'normal)
  (set-face-attribute
   'org-scheduled
   nil :foreground "#0f0")
  (set-face-attribute
   'org-scheduled-today
   nil :foreground "#0f0")
  (set-face-attribute
   'org-scheduled-previously
   nil :foreground "#ff0"))
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-superstar
  (set-face-attribute
   'org-superstar-leading
   nil :foreground "#f00" :height 0.8) ; the dots marking the deapt
  (set-face-attribute
   'org-superstar-item
   nil :foreground "#ff0" :height 0.8)) ; the bullet face
;; ----------------------------------------------------------------------------
;; Habit
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-habit
  (set-face-attribute
   'org-habit-alert-face
   nil :foreground "#f0f" :background "#ff0" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-alert-future-face
   nil :background "#ff0" :height 0.8)
  (set-face-attribute
   'org-habit-overdue-face
   nil :foreground "#ff0" :background "#f0f" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-overdue-future-face
   nil :background "#f00" :height 0.8)
  (set-face-attribute
   'org-habit-ready-face
   nil :foreground "#ff0" :background "#0f0" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-ready-future-face
   nil :background "#0f0" :height 0.8)
  (set-face-attribute
   'org-habit-clear-face
   nil :foreground "#ff0" :background "#f00" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-clear-future-face
   nil :background "#f00" :height 0.8))

;; ============================================================================
;;; Misc. other package faces
;; ============================================================================
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#0ff"))
(set-face-attribute ; matching parenthesis get cursor colors
 'show-paren-match
 nil :foreground "#fff" :background "#000" :weight 'bold)
(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute
   'rainbow-delimiters-base-error-face
   nil :foreground "#fff" :background "#f0f" :weight 'bold :underline t)
  (set-face-attribute
   'rainbow-delimiters-depth-1-face
   nil :foreground "#00f")
  (set-face-attribute
   'rainbow-delimiters-depth-2-face
   nil :foreground "#ff0")
  (set-face-attribute
   'rainbow-delimiters-depth-3-face
   nil :foreground "#f0f"))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#f00" :background "#000")
  (set-face-attribute
   'corfu-current
   nil :foreground "#000" :background "#f00"))
(with-eval-after-load 'keycast
  (set-face-attribute
   'keycast-key
   nil :foreground "#000" :background "#f00" :box t :height 0.8))
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face
   nil :foreground "#ff0" :height 1.0))
(with-eval-after-load 'counsel
  (set-face-attribute
   'ivy-current-match ; counsel use this face
   nil :foreground "#0f0" :background "#f00"))
(with-eval-after-load 'indent-guide
  (set-face-attribute
   'indent-guide-face
   nil :foreground "#f00"))
