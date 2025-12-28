;;; my-faces.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 sizes:  small (4/5=.8), normal (1.0) and large (5/4=1.25).
;; 8 colors: #??? combining f(15), 7 and 3 plus some dusty brownish colors.
;; |------+-----------------+------+-----------------|
;; | #210 | background      | #dcb | default text    |
;; |------+-----------------+------+-----------------|
;; | #f37 | error/todo      | #f73 | warning/heading |
;; | #7f3 | success/done    | #3f7 | comment/tag     |
;; | #37f | link/timestamp  | #432 | shadow/hl-line  |
;; |------+-----------------+------+-----------------|
;; Dark, warm, systematic, simple and aesthetically pleasing.  No "#73f".
;; ----------------------------------------------------------------------------
;; I add 7 cursor colors indicating the `evil-state' when I configure evil.
;; Unspecified faces are handled by the dark part of the vanilla theme.
;; ----------------------------------------------------------------------------
;;;; Special faces.
(set-face-attribute
 'default
 nil :foreground "#dcb" :background "#210")
(set-face-attribute
 'error
 nil :foreground "#f37" :underline t)
(set-face-attribute
 'warning
 nil :foreground "#f73")
(set-face-attribute
 'success
 nil :foreground "#7f3")
;; ----------------------------------------------------------------------------
;;;; font-lock.
(set-face-attribute
 'font-lock-comment-face
 nil :foreground "#3f7")
(set-face-attribute
 'font-lock-string-face
 nil :foreground "#3f7")
(set-face-attribute
 'font-lock-builtin-face
 nil :foreground "#37f")
(set-face-attribute
 'font-lock-type-face
 nil :foreground "#37f")
(set-face-attribute
 'font-lock-constant-face
 nil :foreground "#37f")
(set-face-attribute
 'font-lock-function-name-face
 nil :foreground "#7f3")
(set-face-attribute
 'font-lock-variable-name-face
 nil :foreground "#7f3")
(set-face-attribute
 'font-lock-keyword-face
 nil :foreground "#f73")
(set-face-attribute
 'font-lock-number-face
 nil :foreground "#f37")
(set-face-attribute
 'font-lock-warning-face
 nil :underline nil)
;; ----------------------------------------------------------------------------
;;;; Decorations.
(set-face-attribute
 'vertical-border
 nil :foreground "#432")
(set-face-attribute
 'scroll-bar
 nil :foreground "#432")
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
 'lazy-highlight
 nil :background "#432")
(set-face-attribute
 'isearch
 nil :foreground "#210" :background "#dcb")
(set-face-attribute
 'fringe
 nil :foreground "#432" :background "#210")
(set-face-attribute
 'line-number
 nil :foreground "#432" :background "#210" :height .8)
(set-face-attribute
 'line-number-current-line
 nil :foreground "#210" :background "#432" :weight 'bold)
(set-face-attribute
 'mode-line
 nil :foreground "#210" :background "#432" :box nil :height .8)
(set-face-attribute
 'mode-line-inactive
 nil :foreground "#432" :background "#210" :box nil :overline t)
(set-face-attribute
 'mode-line-highlight
 nil :box '(:line-width 1 :color "#3f7" :style released-button))

;; ============================================================================
;;; Org
;; ============================================================================
(with-eval-after-load 'org
  (set-face-attribute
   'org-document-title
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-1
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-2
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-3
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-4
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-5
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-6
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-7
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-level-8
   nil :foreground "#f73" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-todo
   nil :foreground "#f37" :height .8)
  (set-face-attribute
   'org-done
   nil :foreground "#7f3" :height .8)
  (set-face-attribute
   'org-headline-done
   nil :foreground "#3f7")
  (set-face-attribute
   'org-ellipsis
   nil :foreground "#3f7" :weight 'normal :underline nil :height .8)
  (set-face-attribute
   'org-document-info-keyword
   nil :foreground "#3f7" :weight 'normal :height .8)
  (set-face-attribute
   'org-special-keyword
   nil :foreground "#3f7" :weight 'normal :height .8)
  (set-face-attribute
   'org-checkbox
   nil :foreground "#3f7" :background "#210" :box nil :height .8)
  (set-face-attribute
   'org-tag
   nil :foreground "#3f7" :weight 'normal)
  (set-face-attribute
   'org-table
   nil :foreground "#3f7" :height .8)
  (set-face-attribute
   'org-formula
   nil :foreground "#3f7" :height .8)
  (set-face-attribute
   'org-code
   nil :foreground "#3f7" :height .8)
  (set-face-attribute
   'org-verbatim
   nil :foreground "#37f" :box '(:line-width 1 :color "#3f7" :style released-button) :height .8)
  (set-face-attribute
   'org-block
   nil :foreground "#dcb")
  (set-face-attribute
   'org-block-begin-line
   nil :foreground "#37f" :height .8)
  (set-face-attribute
   'org-block-end-line
   nil :foreground "#37f")
  (set-face-attribute
   'org-drawer
   nil :foreground "#37f" :height .8)
  (set-face-attribute
   'org-footnote
   nil :foreground "#37f" :underline nil :height .8)
  (set-face-attribute
   'org-date
   nil :foreground "#37f" :underline nil :height .8)
  (set-face-attribute
   'org-link
   nil :foreground "#37f"))
;; ----------------------------------------------------------------------------
;;;; Bullets.
(with-eval-after-load 'org-superstar
  (set-face-attribute
   'org-superstar-leading
   nil :foreground "#432" :height .8) ; the dots marking the deapt
  (set-face-attribute
   'org-superstar-header-bullet
   nil :foreground "#3f7" :height .8) ; the header bullet face
  (set-face-attribute
   'org-superstar-item
   nil :foreground "#f73" :height .8)) ; the item bullet face
;; ============================================================================
;;;; Agenda
;; ============================================================================
(with-eval-after-load 'org-agenda
  (set-face-attribute
   'header-line
   nil :foreground "#f73" :background "#210" :weight 'bold :height 1.25)
  (set-face-attribute
   'org-agenda-structure
   nil :foreground "#f73" :background "#210" :box nil :weight 'bold :height 1.25)
  (set-face-attribute
   'org-column
   nil :background "#210")
  (set-face-attribute
   'org-warning
   nil :foreground "#f37")
  (set-face-attribute
   'org-agenda-done
   nil :foreground "#3f7" :slant 'normal)
  (set-face-attribute
   'org-time-grid
   nil :foreground "#3f7")
  (set-face-attribute
   'calendar-weekday-header
   nil :foreground "#3f7")
  (set-face-attribute
   'org-agenda-calendar-event
   nil :foreground "#dcb")
  (set-face-attribute
   'org-agenda-clocking
   nil :foreground "#3f7" :background "#432")
  (set-face-attribute
   'org-agenda-date
   nil :foreground "#37f" :background "#210" :box nil :weight 'normal :height .8)
  (set-face-attribute
   'org-agenda-date-weekend
   nil :foreground "#37f" :background "#210" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today
   nil :foreground "#f73" :background "#210" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-upcoming-distant-deadline
   nil :foreground "#7f3")
  (set-face-attribute
   'org-upcoming-deadline
   nil :foreground "#7f3")
  (set-face-attribute
   'org-imminent-deadline
   nil :foreground "#f73" :weight 'normal)
  (set-face-attribute
   'org-scheduled
   nil :foreground "#7f3")
  (set-face-attribute
   'org-scheduled-today
   nil :foreground "#7f3")
  (set-face-attribute
   'org-scheduled-previously
   nil :foreground "#f73"))
;; ----------------------------------------------------------------------------
;;;; Habit.
(with-eval-after-load 'org-habit
  (set-face-attribute
   'org-habit-alert-face
   nil :foreground "#f37" :background "#f73" :weight 'bold)
  (set-face-attribute
   'org-habit-alert-future-face
   nil :background "#f73")
  (set-face-attribute
   'org-habit-overdue-face
   nil :foreground "#f73" :background "#f37" :weight 'bold)
  (set-face-attribute
   'org-habit-overdue-future-face
   nil :background "#432")
  (set-face-attribute
   'org-habit-ready-face
   nil :foreground "#f73" :background "#7f3" :weight 'bold)
  (set-face-attribute
   'org-habit-ready-future-face
   nil :background "#7f3")
  (set-face-attribute
   'org-habit-clear-face
   nil :foreground "#f73" :background "#432" :weight 'bold)
  (set-face-attribute
   'org-habit-clear-future-face
   nil :background "#432"))

;; ============================================================================
;;; Misc. other package faces
;; ============================================================================
(set-face-attribute
 'show-paren-match
 nil :foreground "#dcb" :background "#432" :weight 'bold)
(with-eval-after-load 'keycast
  (set-face-attribute
   'keycast-key
   nil :foreground "#210" :background "#432" :box t :height .8))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#3f7"))
(with-eval-after-load 'ace-window
  (set-face-attribute
   'aw-leading-char-face
   nil :foreground "#f73" :height 1.0))
(with-eval-after-load 'counsel
  (set-face-attribute
   'ivy-current-match ; counsel use this ivy face
   nil :foreground "#7f3" :background "#432"))
(with-eval-after-load 'indent-guide
  (set-face-attribute
   'indent-guide-face
   nil :foreground "#432"))
(with-eval-after-load 'dired
  (set-face-attribute
   'dired-ignored
   nil :foreground "#3f7"))
(with-eval-after-load 'marginalia
  (set-face-attribute
   'marginalia-documentation
   nil :foreground "#3f7"))
(with-eval-after-load 'corfu
  (set-face-attribute
   'corfu-default
   nil :foreground "#432" :background "#210")
  (set-face-attribute
   'corfu-current
   nil :foreground "#210" :background "#432"))
(with-eval-after-load 'flyspell
  (set-face-attribute
   'flyspell-duplicate
   nil :underline '(:style wave :color "#f73"))
  (set-face-attribute
   'flyspell-incorrect
   nil :underline '(:style wave :color "#f37")))
(with-eval-after-load 'rainbow-delimiters
  (set-face-attribute
   'rainbow-delimiters-base-error-face
   nil :foreground "#dcb" :background "#f37" :weight 'bold)
  (set-face-attribute
   'rainbow-delimiters-depth-1-face
   nil :foreground "#7f3")
  (set-face-attribute
   'rainbow-delimiters-depth-2-face
   nil :foreground "#37f")
  (set-face-attribute
   'rainbow-delimiters-depth-3-face
   nil :foreground "#f73"))
(with-eval-after-load 'tab-bar
  (set-face-attribute
   'tab-bar
   nil :foreground "#432" :background "#210" :weight 'bold :box nil :height .8 :inherit 'default)
  (set-face-attribute
   'tab-bar-tab
   nil :foreground "#210" :background "#432" :box t)
  (set-face-attribute
   'tab-bar-tab-inactive
   nil :foreground "#432" :background "#210" :box nil)
  (unless (version<= emacs-version "31.0")
    (set-face-attribute
     'tab-bar-tab-highlight
     nil :foreground "#210" :background "#432" :box '(:line-width 1 :color "#3f7" :style released-button))))
(with-eval-after-load 'tab-line
  (set-face-attribute
   'tab-line
   nil :foreground "#432" :background "#210" :overline t :box nil)
  (set-face-attribute
   'tab-line-tab ;; active tab in another frame
   nil :box nil :inherit 'tab-line)
  (set-face-attribute
   'tab-line-tab-current
   nil :foreground "#210" :background "#432" :box nil)
  (set-face-attribute
   'tab-line-tab-modified
   nil :foreground "#210" :background "#432" :box nil)
  (set-face-attribute
   'tab-line-tab-inactive
   nil :foreground "#432" :background "#210" :box nil)
  (set-face-attribute
   'tab-line-highlight ;; mouseover
   nil :foreground "#432" :background "#210" :box '(:line-width 1 :color "#3f7" :style released-button)))
;;; my-faces.el ends here
