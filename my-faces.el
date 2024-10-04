;; -*- lexical-binding: t; -*-
;; #+title: my-faces.el

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 `sizes': small (4/5=0.8), normal (1.0) and large (5/4=1.25).
;; 8 `colors': #??? combining f(15), 8 and 3 (n^2-1) and some grayishs colors.
;; |------+-----------------+------+-----------------|
;; | #220 | background      | #ddb | default text    |
;; |------+-----------------+------+-----------------|
;; | #f38 | error/todo      | #f83 | warning/heading |
;; | #8f3 | success/done    | #3f8 | comment/tag     |
;; | #38f | link/timestamp  | #432 | shadow/hl-line  |
;; |------+-----------------+------+-----------------|
;; Dark, warm, simple, systematic and aesthetically pleasing.
;; ----------------------------------------------------------------------------
;; I don't use #83f due to bad color contrast.
;; When I configure `evil' I add 7 cursor colors indicating the evil state.
;; Unspecified faces are handled by the dark part of the vanilla theme.
;; ----------------------------------------------------------------------------
;; Fonts
;; ----------------------------------------------------------------------------
(set-face-attribute
 'default
 nil :font "Ubuntu Mono" :foreground "#ddb" :background "#220" :height 180)
(set-face-attribute
 'fixed-pitch
 nil :font "Ubuntu Mono")
(set-face-attribute
 'variable-pitch
 nil :font "Verdana")
;; ----------------------------------------------------------------------------
;; Special faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'error
 nil :foreground "#f38" :underline t)
(set-face-attribute
 'warning
 nil :foreground "#f83")
(set-face-attribute
 'success
 nil :foreground "#8f3")
;; ----------------------------------------------------------------------------
;; Decorations
;; ----------------------------------------------------------------------------
(set-face-attribute
 'region
 nil :background "#432")
(set-face-attribute
 'shadow
 nil :foreground "#432")
(set-face-attribute
 'match
 nil :background "#432")
(set-face-attribute
 'highlight
 nil :background "#432")
(set-face-attribute
 'vertical-border
 nil :foreground "#432")
(set-face-attribute
 'fringe
 nil :foreground "#432" :background "#220")
(set-face-attribute
 'line-number
 nil :foreground "#432" :background "#220" :height 0.8)
(set-face-attribute
 'line-number-current-line
 nil :foreground "#220" :background "#432" :weight 'bold)
(set-face-attribute
 'mode-line
 nil :foreground "#220" :background "#432" :box nil :height 0.8)
(set-face-attribute
 'mode-line-inactive
 nil :foreground "#432" :background "#220" :box nil :overline t)
;; ----------------------------------------------------------------------------
;; font-lock faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'font-lock-builtin-face
 nil :foreground "#8f3")
(set-face-attribute
 'font-lock-comment-face
 nil :foreground "#3f8")
(set-face-attribute
 'font-lock-constant-face
 nil :foreground "#f83")
(set-face-attribute
 'font-lock-function-name-face
 nil :foreground "#8f3")
(set-face-attribute
 'font-lock-keyword-face
 nil :foreground "#38f")
(set-face-attribute
 'font-lock-string-face
 nil :foreground "#3f8")
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
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-1
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-2
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-3
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-4
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-5
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-6
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-7
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-8
   nil :foreground "#f83" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-todo
   nil :foreground "#f38" :height 0.8)
  (set-face-attribute
   'org-done
   nil :foreground "#8f3" :height 0.8)
  (set-face-attribute
   'org-headline-done
   nil :foreground "#3f8")
  (set-face-attribute
   'org-ellipsis
   nil :foreground "#3f8" :weight 'normal :underline nil :height 0.8)
  (set-face-attribute
   'org-document-info-keyword
   nil :foreground "#3f8" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-special-keyword
   nil :foreground "#3f8" :weight 'normal :height 0.8)
  (set-face-attribute
   'org-checkbox
   nil :foreground "#3f8" :background "#220" :box nil :height 0.8)
  (set-face-attribute
   'org-tag
   nil :foreground "#3f8" :weight 'normal)
  (set-face-attribute
   'org-formula
   nil :foreground "#3f8" :height 0.8)
  (set-face-attribute
   'org-code
   nil :foreground "#3f8" :height 0.8)
  (set-face-attribute
   'org-verbatim
   nil :foreground "#3f8" :height 0.8)
  (set-face-attribute
   'org-table
   nil :foreground "#3f8" :height 0.8)
  (set-face-attribute
   'org-block
   nil :foreground "#ddb")
  (set-face-attribute
   'org-block-begin-line
   nil :foreground "#38f" :height 0.8)
  (set-face-attribute
   'org-block-end-line
   nil :foreground "#38f")
  (set-face-attribute
   'org-drawer
   nil :foreground "#38f" :height 0.8)
  (set-face-attribute
   'org-footnote
   nil :foreground "#38f" :underline nil :height 0.8)
  (set-face-attribute
   'org-date
   nil :foreground "#38f" :underline nil :height 0.8)
  (set-face-attribute
   'org-link
   nil :foreground "#38f"))
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-superstar
  (set-face-attribute
   'org-superstar-leading
   nil :foreground "#432" :height 0.8) ; the dots marking the deapt
  (set-face-attribute
   'org-superstar-header-bullet
   nil :foreground "#f83" :height 0.8) ; the header bullet face
  (set-face-attribute
   'org-superstar-item
   nil :foreground "#f83" :height 0.8)) ; the item bullet face
;; ----------------------------------------------------------------------------
;; Agenda
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-agenda
  (set-face-attribute
   'header-line
   nil :foreground "#f83" :background "#220" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-agenda-structure
   nil :foreground "#f83" :background "#220" :box nil :weight 'bold :height 1.25)
  (set-face-attribute
   'org-column
   nil :background "#220")
  (set-face-attribute
   'org-warning
   nil :foreground "#f38")
  (set-face-attribute
   'org-agenda-done
   nil :foreground "#3f8" :slant 'normal)
  (set-face-attribute
   'org-time-grid
   nil :foreground "#3f8")
  (set-face-attribute
   'calendar-weekday-header
   nil :foreground "#3f8")
  (set-face-attribute
   'org-agenda-calendar-event
   nil :foreground "#ddb")
  (set-face-attribute
   'org-agenda-clocking
   nil :foreground "#3f8" :background "#432")
  (set-face-attribute
   'org-agenda-date
   nil :foreground "#38f" :background "#220" :box nil :weight 'normal :height 0.8)
  (set-face-attribute
   'org-agenda-date-weekend
   nil :foreground "#38f" :background "#220" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today
   nil :foreground "#f83" :background "#220" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-upcoming-distant-deadline
   nil :foreground "#8f3")
  (set-face-attribute
   'org-upcoming-deadline
   nil :foreground "#8f3")
  (set-face-attribute
   'org-imminent-deadline
   nil :foreground "#f83" :weight 'normal)
  (set-face-attribute
   'org-scheduled
   nil :foreground "#8f3")
  (set-face-attribute
   'org-scheduled-today
   nil :foreground "#8f3")
  (set-face-attribute
   'org-scheduled-previously
   nil :foreground "#f83"))
;; ----------------------------------------------------------------------------
;; Habit
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org-habit
  (set-face-attribute
   'org-habit-alert-face
   nil :foreground "#f38" :background "#f83" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-alert-future-face
   nil :background "#f83" :height 0.8)
  (set-face-attribute
   'org-habit-overdue-face
   nil :foreground "#f83" :background "#f38" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-overdue-future-face
   nil :background "#432" :height 0.8)
  (set-face-attribute
   'org-habit-ready-face
   nil :foreground "#f83" :background "#8f3" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-ready-future-face
   nil :background "#8f3" :height 0.8)
  (set-face-attribute
   'org-habit-clear-face
   nil :foreground "#f83" :background "#432" :weight 'bold :height 0.8)
  (set-face-attribute
   'org-habit-clear-future-face
   nil :background "#432" :height 0.8))

;; ============================================================================
;;; Misc. other package faces
;; ============================================================================
(set-face-attribute
 'show-paren-match
 nil :foreground "#ddb" :background "#220" :weight 'bold)
(with-eval-after-load 'tab-bar
  (set-face-attribute
   'tab-bar
   nil :foreground "#432" :background "#220" :weight 'bold :box nil :height 0.8 :inherit 'default)
  (set-face-attribute
   'tab-bar-tab
   nil :foreground "#220" :background "#432" :box t)
  (set-face-attribute
   'tab-bar-tab-inactive
   nil :foreground "#432" :background "#220" :box nil))
(with-eval-after-load 'tab-line
  (set-face-attribute
   'tab-line
   nil :foreground "#432" :background "#220" :overline t :box nil)
  (set-face-attribute
   'tab-line-tab ;; active tab in another frame
   nil :box nil :inherit 'tab-line)
  (set-face-attribute
   'tab-line-tab-current
   nil :foreground "#220" :background "#432" :box nil)
  (set-face-attribute
   'tab-line-tab-modified
   nil :foreground "#220" :background "#432" :box nil)
  (set-face-attribute
   'tab-line-tab-inactive
   nil :foreground "#432" :background "#220" :box nil)
  (set-face-attribute
   'tab-line-highlight ;; mouseover
   nil :foreground "#ddb" :background "#220" :box nil))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#432" :background "#220")
  (set-face-attribute
   'corfu-current
   nil :foreground "#220" :background "#432"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#3f8"))
(with-eval-after-load 'keycast
  (set-face-attribute
   'keycast-key
   nil :foreground "#220" :background "#432" :box t :height 0.8))
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face
   nil :foreground "#f83" :height 1.0))
(with-eval-after-load 'counsel
  (set-face-attribute
   'ivy-current-match ; counsel use this ivy face
   nil :foreground "#8f3" :background "#432"))
(with-eval-after-load 'indent-guide
  (set-face-attribute
   'indent-guide-face
   nil :foreground "#432"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#3f8"))
(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute
   'rainbow-delimiters-base-error-face
   nil :foreground "#ddb" :background "#f38" :weight 'bold :underline t)
  (set-face-attribute
   'rainbow-delimiters-depth-1-face
   nil :foreground "#38f")
  (set-face-attribute
   'rainbow-delimiters-depth-2-face
   nil :foreground "#8f3")
  (set-face-attribute
   'rainbow-delimiters-depth-3-face
   nil :foreground "#f38"))
;; End of my-faces.el
