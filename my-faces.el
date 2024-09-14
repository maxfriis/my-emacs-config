;; -*- lexical-binding: t; -*-
;; #+title: my-faces.el

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 `sizes': small (4/5=0.8), normal (1.0) and large (5/4=1.25).
;; 8 `colors': #??? combining f, a and 5, and some grayishs colors.
;; |------+-----------------+------+-----------------+------+-----------------|
;; | #221 | background      | #bba | default text    | #432 | shadow/hl-line  |
;; |------+-----------------+------+-----------------+------+-----------------|
;; | #f5a | error/todo      | #af5 | success/done    | #5af | link/timestamp  |
;; | #fa5 | warning/heading | #5fa | comment/tag     | #a5f | not used        |
;; |------+-----------------+------+-----------------+------+-----------------|
;; Dark, warm, simple, systematic and aesthetically pleasing.
;; ----------------------------------------------------------------------------
;; When I configure `evil' I add 8 cursor colors depending on the evil state.
;; Unspecified faces are handled by the dark part of the vanilla theme.
;; ----------------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------------
(set-face-attribute
 'default
 nil :font "Ubuntu Mono" :foreground "#bba" :background "#221" :height 180)
(set-face-attribute
 'fixed-pitch
 nil :font "Ubuntu Mono")
(set-face-attribute
 'variable-pitch
 nil :font "Verdana")
;; ----------------------------------------------------------------------------
(set-face-attribute
 'error
 nil :foreground "#f5a" :underline t)
(set-face-attribute
 'warning
 nil :foreground "#fa5")
(set-face-attribute
 'success
 nil :foreground "#af5")
(set-face-attribute
 'shadow
 nil :foreground "#432")
(set-face-attribute
 'match
 nil :background "#432")
;; ----------------------------------------------------------------------------
;; font-lock faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'font-lock-builtin-face
 nil :foreground "#af5")
(set-face-attribute
 'font-lock-comment-face
 nil :foreground "#5fa")
(set-face-attribute
 'font-lock-constant-face
 nil :foreground "#fa5")
(set-face-attribute
 'font-lock-function-name-face
 nil :foreground "#af5")
(set-face-attribute
 'font-lock-keyword-face
 nil :foreground "#5af")
(set-face-attribute
 'font-lock-string-face
 nil :foreground "#5fa")
(set-face-attribute
 'font-lock-warning-face
 nil :underline nil)
;; ----------------------------------------------------------------------------
;; Decorations
;; ----------------------------------------------------------------------------
(set-face-attribute
 'highlight
 nil :background "#432")
(set-face-attribute
 'vertical-border
 nil :foreground "#432")
(set-face-attribute
 'fringe
 nil :foreground "#432" :background "#221")
(set-face-attribute
 'line-number
 nil :foreground "#432" :background "#221" :height 0.8)
(set-face-attribute
 'line-number-current-line
 nil :foreground "#221" :background "#432")
(set-face-attribute
 'mode-line
 nil :foreground "#221" :background "#432" :box nil :height 0.8)
(set-face-attribute
 'mode-line-inactive
 nil :foreground "#432" :background "#221" :box nil :overline t)
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
   nil :foreground "#fa5" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-1
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-2
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-3
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-4
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-5
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-6
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-7
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-level-8
   nil :foreground "#fa5" :weight 'bold :height 1.0)
  (set-face-attribute
   'org-todo
   nil :foreground "#f5a" :height 0.8)
  (set-face-attribute
   'org-done
   nil :foreground "#af5" :height 0.8)
  (set-face-attribute
   'org-headline-done
   nil :foreground "#5fa")
  (set-face-attribute
   'org-ellipsis
   nil :foreground "#5fa" :weight 'normal :underline nil :height 0.8)
  (set-face-attribute
   'org-document-info-keyword
   nil :foreground "#5fa" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-special-keyword
   nil :foreground "#5fa" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-checkbox
   nil :foreground "#5fa" :background "#221" :box nil :height 0.8)
  (set-face-attribute
   'org-tag
   nil :foreground "#5fa" :weight 'normal)
  (set-face-attribute
   'org-formula
   nil :foreground "#5fa" :height 0.8)
  (set-face-attribute
   'org-code
   nil :foreground "#5fa" :height 0.8)
  (set-face-attribute
   'org-verbatim
   nil :foreground "#5fa" :height 0.8)
  (set-face-attribute
   'org-table
   nil :foreground "#5fa" :height 0.8)
  (set-face-attribute
   'org-block
   nil :foreground "#bba")
  (set-face-attribute
   'org-block-begin-line
   nil :foreground "#5af" :height 0.8)
  (set-face-attribute
   'org-block-end-line
   nil :foreground "#5af")
  (set-face-attribute
   'org-drawer
   nil :foreground "#5af" :height 0.8)
  (set-face-attribute
   'org-footnote
   nil :foreground "#5af" :underline nil :height 0.8)
  (set-face-attribute
   'org-date
   nil :foreground "#5af" :underline nil :height 0.8)
  (set-face-attribute
   'org-link
   nil :foreground "#5af")
  (set-face-attribute
   'org-meta-line
   nil :height 1.25))
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-superstar
  (set-face-attribute
   'org-superstar-leading
   nil :foreground "#432" :height 0.8) ; the dots marking the deapt
  (set-face-attribute
   'org-superstar-item
   nil :foreground "#fa5" :height 0.8)) ; the bullet face
;; ----------------------------------------------------------------------------
;; Agenda
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-agenda
  (set-face-attribute
   'header-line
   nil :foreground "#fa5" :background "#221" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-agenda-structure
   nil :foreground "#fa5" :background "#221" :box nil :weight 'bold :height 1.0)
  (set-face-attribute
   'org-column
   nil :background "#221")
  (set-face-attribute
   'org-warning
   nil :foreground "#f5a")
  (set-face-attribute
   'org-agenda-done
   nil :foreground "#5fa" :slant 'normal)
  (set-face-attribute
   'org-time-grid
   nil :foreground "#5fa")
  (set-face-attribute
   'calendar-weekday-header
   nil :foreground "#5fa")
  (set-face-attribute
   'org-agenda-calendar-event
   nil :foreground "#bba")
  (set-face-attribute
   'org-agenda-clocking
   nil :foreground "#5fa" :background "#432")
  (set-face-attribute
   'org-agenda-date
   nil :foreground "#5af" :background "#221" :box nil :weight 'normal)
  (set-face-attribute
   'org-agenda-date-weekend
   nil :foreground "#5af" :background "#221" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today
   nil :foreground "#fa5" :background "#221" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-upcoming-distant-deadline
   nil :foreground "#af5")
  (set-face-attribute
   'org-upcoming-deadline
   nil :foreground "#af5")
  (set-face-attribute
   'org-imminent-deadline
   nil :foreground "#fa5" :weight 'normal)
  (set-face-attribute
   'org-scheduled
   nil :foreground "#af5")
  (set-face-attribute
   'org-scheduled-today
   nil :foreground "#af5")
  (set-face-attribute
   'org-scheduled-previously
   nil :foreground "#fa5"))
;; ----------------------------------------------------------------------------
;; Habit
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-habit
  (set-face-attribute
   'org-habit-alert-face
   nil :foreground "#f5a" :background "#fa5" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-alert-future-face
   nil :background "#fa5" :height 0.8)
  (set-face-attribute
   'org-habit-overdue-face
   nil :foreground "#fa5" :background "#f5a" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-overdue-future-face
   nil :background "#432" :height 0.8)
  (set-face-attribute
   'org-habit-ready-face
   nil :foreground "#fa5" :background "#af5" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-ready-future-face
   nil :background "#af5" :height 0.8)
  (set-face-attribute
   'org-habit-clear-face
   nil :foreground "#fa5" :background "#432" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-clear-future-face
   nil :background "#432" :height 0.8))

;; ============================================================================
;;; Misc. other package faces
;; ============================================================================
(set-face-attribute
 'show-paren-match
 nil :foreground "#bba" :background "#000" :weight 'bold)
(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute
   'rainbow-delimiters-base-error-face
   nil :foreground "#bba" :background "#f5a" :weight 'bold :underline t)
  (set-face-attribute
   'rainbow-delimiters-depth-1-face
   nil :foreground "#5af")
  (set-face-attribute
   'rainbow-delimiters-depth-2-face
   nil :foreground "#fa5")
  (set-face-attribute
   'rainbow-delimiters-depth-3-face
   nil :foreground "#f5a"))
(with-eval-after-load 'tab-bar
  (set-face-attribute
   'tab-bar
   nil :foreground "#432" :background "#221" :weight 'bold :box nil :inherit 'default :height 0.8)
  (set-face-attribute
   'tab-bar-tab
   nil :foreground "#221" :background "#432" :box t)
  (set-face-attribute
   'tab-bar-tab-inactive
   nil :foreground "#432" :background "#221" :box nil))
(with-eval-after-load 'tab-line
  (set-face-attribute
   'tab-line
   nil :foreground "#432" :background "#221" :overline t :box nil)
  (set-face-attribute
   'tab-line-tab ;; active tab in another frame
   nil :inherit 'tab-line :box nil)
  (set-face-attribute
   'tab-line-tab-current
   nil :foreground "#221" :background "#432" :box nil)
  (set-face-attribute
   'tab-line-tab-modified
   nil :foreground "#221" :background "#432" :box nil)
  (set-face-attribute
   'tab-line-tab-inactive
   nil :foreground "#432" :background "#221" :box nil)
  (set-face-attribute
   'tab-line-highlight ;; mouseover
   nil :foreground "#bba" :background "#221" :box nil))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#432" :background "#221")
  (set-face-attribute
   'corfu-current
   nil :foreground "#221" :background "#432"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#5fa"))
(with-eval-after-load 'keycast
  (set-face-attribute
   'keycast-key
   nil :foreground "#221" :background "#432" :box t :height 0.8))
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face
   nil :foreground "#fa5" :height 1.0))
(with-eval-after-load 'counsel
  (set-face-attribute
   'ivy-current-match ; counsel use this ivy face
   nil :foreground "#af5" :background "#432"))
(with-eval-after-load 'indent-guide
  (set-face-attribute
   'indent-guide-face
   nil :foreground "#432"))
