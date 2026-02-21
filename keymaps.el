;;; keymaps.el --- My keymaps -*- lexical-binding: t; -*-

(defvar-keymap my-a-spc-map
  :doc "My <spc> prefix keys."
  "A"   #'tmr-tabulated-view
  "a"   #'tmr
  "C"   #'calc
  "c"   #'full-calc
  "i"   #'erc-tls
  "s"   #'eshell
  "u"   #'undo-tree-visualize
  "w"   #'world-clock)
(defvar-keymap my-b-spc-map
  :doc "My <spc> prefix keys."
  "b"   #'consult-buffer
  "d"   #'kill-current-buffer
  "i"   #'ibuffer
  "j"   #'next-buffer
  "k"   #'previous-buffer
  "m"   #'view-echo-area-messages
  "s"   #'scratch-buffer)
(defvar-keymap my-f-spc-map
  :doc "My <spc> prefix keys."
  "R"   #'consult-recent-file
  "S"   #'save-some-buffers
  "a"   #'my-find-agenda-file
  "b"   #'diff-buffer-with-file
  "d"   #'dired-jump
  "e"   #'ediff-files
  "f"   #'counsel-find-file
  "g"   #'consult-ripgrep
  "i"   #'my-find-init-file
  "n"   #'my-find-note-file
  "p"   #'find-file-at-point
  "r"   #'recentf-open-files
  "s"   #'basic-save-buffer
  "t"   #'my-find-my-faces-file
  "w"   #'write-file)
(defvar-keymap my-g-spc-map
  :doc "My <spc> prefix keys."
  "b"   #'magit-blame
  "c"   #'my-git-stage-all-and-commit
  "g"   #'magit
  "s"   #'magit-stage)
(defvar-keymap my-l-spc-map
  :doc "My <spc> prefix keys."
  "e"   #'eval-expression
  "l"   #'eval-last-sexp
  "p"   #'eval-print-last-sexp
  "r"   #'elisp-eval-region-or-buffer)
(defvar-keymap my-n-spc-map
  :doc "My <spc> prefix keys."
  "f"   #'narrow-to-defun
  "n"   #'recursive-narrow-or-widen-dwim
  "o"   #'org-narrow-to-subtree
  "r"   #'narrow-to-region
  "w"   #'widen)
(defvar-keymap my-o-spc-map
  :doc "My <spc> prefix keys."
  "."   #'org-time-stamp
  "A"   #'org-archive-subtree-default
  "E"   #'org-export-dispatch
  "F"   #'org-agenda-file-to-front
  "G"   #'org-goto
  "I"   #'org-clock-in
  "L"   #'org-store-link
  "N"   #'org-num-mode
  "O"   #'org-clock-out
  "P"   #'org-set-property
  "R"   #'org-refile
  "S"   #'org-sort
  "T"   #'orgtbl-mode
  "a"   #'org-agenda
  "b"   #'org-insert-structure-template
  "c"   #'org-capture
  "d"   #'org-deadline
  "e"   #'org-latex-export-to-pdf
  "g"   #'counsel-org-goto-all
  "l"   #'org-insert-link
  "n"   #'org-add-note
  "o"   #'org-open-at-point
  "p"   #'org-present
  "s"   #'org-schedule
  "t"   #'evil-org-org-insert-todo-heading-respect-content-below)
(defvar-keymap my-q-spc-map
  :doc "My <spc> prefix keys."
  "q"   #'my-save-all-kill-emacs-no-prompt
  "s"   #'save-buffers-kill-emacs)
(defvar-keymap my-r-spc-map
  :doc "My <spc> prefix keys."
  "b"   #'counsel-bookmark
  "d"   #'bookmark-delete
  "l"   #'consult-register-load
  "r"   #'counsel-mark-ring
  "s"   #'consult-register-store)
(defvar-keymap my-s-spc-map
  :doc "My <spc> prefix keys."
  "O"   #'occur
  "R"   #'query-replace-regexp
  "o"   #'consult-outline
  "r"   #'query-replace
  "s"   #'swiper
  "w"   #'eww)
(defvar-keymap my-tb-spc-map
  :doc "My <spc> prefix keys."
  "b"   #'tab-bar-mode
  "c"   #'tab-bar-close-tab
  "d"   #'tab-bar-duplicate-tab
  "h"   #'tab-bar-switch-to-prev-tab
  "l"   #'tab-bar-switch-to-next-tab
  "m"   #'tab-bar-move-tab
  "n"   #'tab-bar-new-tab
  "o"   #'tab-bar-close-other-tabs
  "p"   #'tab-bar-close-tab-by-name
  "r"   #'tab-undo
  "s"   #'tab-bar-select-tab-by-name)
(defvar-keymap my-ti-spc-map
  :doc "My <spc> prefix keys."
  "c"   #'display-fill-column-indicator-mode
  "i"   #'indent-guide-mode
  "n"   #'whitespace-newline-mode
  "p"   #'whitespace-page-delimiters-mode
  "s"   #'whitespace-mode)
(defvar-keymap my-tk-spc-map
  :doc "My <spc> prefix keys."
  "h"   #'keycast-header-line-mode
  "l"   #'keycast-log-mode
  "m"   #'keycast-mode-line-mode
  "t"   #'keycast-tab-bar-mode)
(defvar-keymap my-tl-spc-map
  :doc "My <spc> prefix keys."
  "l"   #'visual-line-mode
  "n"   #'display-line-numbers-mode
  "t"   #'toggle-truncate-lines)
(defvar-keymap my-t-spc-map
  :doc "My <spc> prefix keys."
  "a"   #'auto-save-visited-mode
  "c"   #'colorful-mode
  "d"   #'display-time-mode
  "e"   #'evil-emacs-cursor-model-mode
  "f"   #'mixed-pitch-mode
  "h"   #'global-hl-line-mode
  "m"   #'mode-line-invisible-mode
  "o"   #'olivetti-mode
  "p"   #'prettify-symbols-mode
  "s"   #'flyspell-mode
  "t"   #'my-toggle-themes
  "w"   #'writegood-mode
  "b"   my-tb-spc-map
  "i"   my-ti-spc-map
  "k"   my-tk-spc-map
  "l"   my-tl-spc-map)
(defvar-keymap my-z-spc-map
  :doc "My <spc> prefix keys."
  "+"   #'text-scale-increase
  "-"   #'text-scale-decrease
  "g"   #'global-text-scale-adjust
  "l"   #'text-scale-adjust)
(defvar-keymap my-x-spc-map
  :doc "My <spc> prefix keys."
  "D"   #'evil-downcase
  "S"   #'sort-lines
  "U"   #'evil-upcase
  "W"   #'flyspell-word
  "c"   #'transpose-chars
  "l"   #'transpose-lines
  "p"   #'transpose-paragraphs
  "s"   #'transpose-sentences
  "u"   #'insert-char
  "w"   #'transpose-words
  "x"   #'just-one-space)
(defvar-keymap my-root-spc-map
  :doc "The root of my <spc> prefix keys."
  "SPC" #'counsel-M-x
  "TAB" #'tab-bar-select-tab-by-name
  "0"   #'delete-window
  "1"   #'my-toggle-window-maximize
  "2"   #'split-window-below
  "3"   #'split-window-right
  "4"   #'my-4-windows
  "5"   #'my-ace-swap-window
  "6"   #'rotate-frame-anticlockwise
  "7"   #'transpose-frame
  "8"   #'my-split-dired-tab
  "9"   #'tab-bar-close-tab
  "e"   #'embark-act
  "j"   #'evil-avy-goto-char-timer
  "u"   #'universal-argument
  "a"   my-a-spc-map
  "b"   my-b-spc-map
  "f"   my-f-spc-map
  "g"   my-g-spc-map
  "h"   help-map
  "l"   my-l-spc-map
  "n"   my-n-spc-map
  "o"   my-o-spc-map
  "q"   my-q-spc-map
  "r"   my-r-spc-map
  "s"   my-s-spc-map
  "t"   my-t-spc-map
  "w"   evil-window-map
  "x"   my-x-spc-map
  "z"   my-z-spc-map)

;; ----------------------------------------------------------------------------
;; `which-key' leader key replacements.
(unless (version< emacs-version "30.1")
  (which-key-add-keymap-based-replacements my-a-spc-map
    "A"   '("View alarms"  . tmr-tabulated-view)
    "a"   '("Alarm"        . tmr)
    "C"   '("Calc"         . calc)
    "c"   '("Full calc"    . full-calc)
    "i"   '("irc chat"     . erc-tls)
    "s"   '("Eshell"       . eshell)
    "u"   '("Undo tree"    . undo-tree-visualize)
    "w"   '("World clock"  . world-clock))
  (which-key-add-keymap-based-replacements my-b-spc-map
    "b"   '("Mini menu"    . consult-buffer)
    "d"   '("Delete"       . kill-current-buffer)
    "i"   '("IBuffer"      . ibuffer)
    "j"   '("Next"         . next-buffer)
    "k"   '("Previous"     . previous-buffer)
    "m"   '("Messages"     . view-echo-area-messages)
    "s"   '("Scratch"      . scratch-buffer))
  (which-key-add-keymap-based-replacements my-f-spc-map
    "R"   '("Mini recent"  . consult-recent-file)
    "S"   '("Save all"     . save-some-buffers)
    "a"   '("Agenda"       . my-find-agenda-file)
    "b"   '("Buffer diff"  . diff-buffer-with-file)
    "d"   '("Dired"        . dired-jump)
    "e"   '("Ediff"        . ediff-files)
    "f"   '("Find"         . counsel-find-file)
    "g"   '("Grep"         . consult-ripgrep)
    "i"   '("Init"         . my-find-init-file)
    "n"   '("Notes"        . my-find-note-file)
    "p"   '("At point"     . find-file-at-point)
    "r"   '("Recent"       . recentf-open-files)
    "s"   '("Save"         . basic-save-buffer)
    "t"   '("My themes"    . my-find-my-faces-file)
    "w"   '("Write"        . write-file))
  (which-key-add-keymap-based-replacements my-g-spc-map
    "b"   '("Blame"        . magit-blame)
    "c"   '("Commit"       . my-git-stage-all-and-commit)
    "g"   '("Magit"        . magit)
    "s"   '("Stage"        . magit-stage))
  (which-key-add-keymap-based-replacements my-l-spc-map
    "e"   '("Expression"   . eval-expression)
    "l"   '("Last sexp"    . eval-last-sexp)
    "p"   '("Print result" . eval-print-last-sexp)
    "r"   '("Region"       . elisp-eval-region-or-buffer))
  (which-key-add-keymap-based-replacements my-n-spc-map
    "f"   '("Function"     . narrow-to-defun)
    "n"   '("Dwim"         . recursive-narrow-or-widen-dwim)
    "o"   '("Org tree"     . org-narrow-to-subtree)
    "r"   '("Region"       . narrow-to-region)
    "w"   '("Widen"        . widen))
  (which-key-add-keymap-based-replacements my-o-spc-map
    "."   '("Timestamp"    . org-time-stamp)
    "A"   '("Archive"      . org-archive-subtree-default)
    "E"   '("Export"       . org-export-dispatch)
    "F"   '("Agenda file"  . org-agenda-file-to-front)
    "G"   '("Goto"         . org-goto)
    "I"   '("Clock in"     . org-clock-in)
    "L"   '("Store link"   . org-store-link)
    "N"   '("Number heads" . org-num-mode)
    "O"   '("Clock out"    . org-clock-out)
    "P"   '("Property"     . org-set-property)
    "R"   '("Refile"       . org-refile)
    "S"   '("Sort"         . org-sort)
    "T"   '("Tables"       . orgtbl-mode)
    "a"   '("Agenda"       . org-agenda)
    "b"   '("Block"        . org-insert-structure-template)
    "c"   '("Capture"      . org-capture)
    "d"   '("Deadline"     . org-deadline)
    "e"   '("Latex pdf"    . org-latex-export-to-pdf)
    "g"   '("Goto head"    . counsel-org-goto-all)
    "l"   '("Ins. link"    . org-insert-link)
    "n"   '("Add note"     . org-add-note)
    "o"   '("Open link"    . org-open-at-point)
    "p"   '("Present"      . org-present)
    "s"   '("Schedule"     . org-schedule)
    "t"   '("New todo"     . evil-org-org-insert-todo-heading-respect-content-below))
  (which-key-add-keymap-based-replacements my-q-spc-map
    "q"   '("Save&kill"    . my-save-all-kill-emacs-no-prompt)
    "s"   '("Prompt&kill"  . save-buffers-kill-emacs))
  (which-key-add-keymap-based-replacements my-r-spc-map
    "b"   '("Bookmark"     . counsel-bookmark)
    "d"   '("Del.bookmark" . bookmark-delete)
    "l"   '("Load"         . consult-register-load)
    "r"   '("Mark ring"    . counsel-mark-ring)
    "s"   '("Store"        . consult-register-store))
  (which-key-add-keymap-based-replacements my-s-spc-map
    "O"   '("Occur"        . occur)
    "R"   '("Rep. regex"   . query-replace-regexp)
    "o"   '("Outline"      . consult-outline)
    "r"   '("Replace"      . query-replace)
    "s"   '("Swiper"       . swiper)
    "w"   '("Web (eww)"    . eww))
  (which-key-add-keymap-based-replacements my-tb-spc-map
    "b"   '("Toggle bar"   . tab-bar-mode)
    "c"   '("Close"        . tab-bar-close-tab)
    "d"   '("Duplicate"    . tab-bar-duplicate-tab)
    "h"   '("Previous"     . tab-bar-switch-to-prev-tab)
    "l"   '("Next"         . tab-bar-switch-to-next-tab)
    "m"   '("Move"         . tab-bar-move-tab)
    "n"   '("New"          . tab-bar-new-tab)
    "o"   '("Close others" . tab-bar-close-other-tabs)
    "p"   '("Close prompt" . tab-bar-close-tab-by-name)
    "r"   '("Restore"      . tab-undo)
    "s"   '("Select"       . tab-bar-select-tab-by-name))
  (which-key-add-keymap-based-replacements my-ti-spc-map
    "c"   '("Column 79"    . display-fill-column-indicator-mode)
    "i"   '("Indentation"  . indent-guide-mode)
    "n"   '("Newline"      . whitespace-newline-mode)
    "p"   '("Page"         . whitespace-page-delimiters-mode)
    "s"   '("Spaces"       . whitespace-mode))
  (which-key-add-keymap-based-replacements my-tk-spc-map
    "h"   '("Header"       . keycast-header-line-mode)
    "l"   '("Log frame"    . keycast-log-mode)
    "m"   '("Mode line"    . keycast-mode-line-mode)
    "t"   '("Tab bar"      . keycast-tab-bar-mode))
  (which-key-add-keymap-based-replacements my-tl-spc-map
    "l"   '("Visual"       . visual-line-mode)
    "n"   '("Numbers"      . display-line-numbers-mode)
    "t"   '("Truncate"     . toggle-truncate-lines))
  (which-key-add-keymap-based-replacements my-t-spc-map
    "a"   '("Auto save"    . auto-save-visited-mode)
    "c"   '("colorful"     . colorful-mode)
    "d"   '("Date/time"    . display-time-mode)
    "e"   '("Emacs cursor" . evil-emacs-cursor-model-mode)
    "f"   '("Font pitch"   . mixed-pitch-mode)
    "h"   '("Hl line"      . global-hl-line-mode)
    "m"   '("Mode line"    . mode-line-invisible-mode)
    "o"   '("Olivetti"     . olivetti-mode)
    "p"   '("Prettify"     . prettify-symbols-mode)
    "s"   '("Spellcheck"   . flyspell-mode)
    "t"   '("Theme"        . my-toggle-themes)
    "w"   '("Write good"   . writegood-mode)
    "b"   `("Tab bar"      . ,my-tb-spc-map)
    "i"   `("Indicate"     . ,my-ti-spc-map)
    "k"   `("Key cast"     . ,my-tk-spc-map)
    "l"   `("Line"         . ,my-tl-spc-map))
  (which-key-add-keymap-based-replacements my-x-spc-map
    "D"   '("Downcase"     . evil-downcase)
    "S"   '("Sort lines"   . sort-lines)
    "U"   '("Upcase"       . evil-upcase)
    "W"   '("Spell word"   . flyspell-word)
    "c"   '("Swap chars"   . transpose-chars)
    "l"   '("Swap lines"   . transpose-lines)
    "p"   '("Swap para."   . transpose-paragraphs)
    "s"   '("Swap sent."   . transpose-sentences)
    "u"   '("Unicode"      . insert-char)
    "w"   '("Swap words"   . transpose-words)
    "x"   '("One space"    . just-one-space))
  (which-key-add-keymap-based-replacements my-z-spc-map
    "+"   '("Increase"     . text-scale-increase)
    "-"   '("Decrease"     . text-scale-decrease)
    "g"   '("Global"       . global-text-scale-adjust)
    "l"   '("Local"        . text-scale-adjust))
  (which-key-add-keymap-based-replacements my-root-spc-map
    "SPC" '("M-x"          . counsel-M-x)
    "TAB" '("Select tab"   . tab-bar-select-tab-by-name)
    "0"   '("Close win"    . delete-window)
    "1"   '("Maximize"     . my-toggle-window-maximize)
    "2"   '("Below"        . split-window-below)
    "3"   '("Right"        . split-window-right)
    "4"   '("Four"         . my-4-windows)
    "5"   '("Swap"         . my-ace-swap-window)
    "6"   '("Rotate"       . rotate-frame-anticlockwise)
    "7"   '("Transpose"    . transpose-frame)
    "8"   '("Dired tab"    . my-split-dired-tab)
    "9"   '("Close tab"    . tab-bar-close-tab)
    "e"   '("Embark"       . embark-act)
    "j"   '("Avy jump"     . evil-avy-goto-char-timer)
    "u"   '("Uni.arg."     . universal-argument)
    "a"   `("App"          . ,my-a-spc-map)
    "b"   `("Buffer"       . ,my-b-spc-map)
    "f"   `("File"         . ,my-f-spc-map)
    "g"   `("Git"          . ,my-g-spc-map)
    "h"   `("Help"         . ,help-map)
    "l"   `("Lisp"         . ,my-l-spc-map)
    "n"   `("Narrow"       . ,my-n-spc-map)
    "o"   `("Org"          . ,my-o-spc-map)
    "q"   `("Quit"         . ,my-q-spc-map)
    "r"   `("Register"     . ,my-r-spc-map)
    "s"   `("Search"       . ,my-s-spc-map)
    "t"   `("Toggle"       . ,my-t-spc-map)
    "w"   `("Window"       . ,evil-window-map)
    "x"   `("Text"         . ,my-x-spc-map)
    "z"   `("Zoom"         . ,my-z-spc-map)))

(provide 'keymaps)
;;; keymaps.el ends here
