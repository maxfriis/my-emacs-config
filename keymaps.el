;;; keymaps.el --- My keymaps -*- lexical-binding: t; -*-

(defvar-keymap my-a-spc-map
  :doc "My <spc> prefix keys."
  "C"   #'full-calc
  "c"   #'calc
  "s"   #'eshell
  "u"   #'undo-tree-visualize)
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
  "a"   #'my/find-agenda-file
  "d"   #'dired-jump
  "e"   #'ediff-files
  "f"   #'counsel-find-file
  "i"   #'my/find-init-file
  "n"   #'my/find-note-file
  "p"   #'find-file-at-point
  "r"   #'recentf-open-files
  "s"   #'basic-save-buffer
  "t"   #'my/find-my-faces-file
  "w"   #'write-file)
(defvar-keymap my-g-spc-map
  :doc "My <spc> prefix keys."
  "b"   #'magit-blame
  "c"   #'my/git-stage-all-and-commit
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
  "E"   #'org-latex-export-to-pdf
  "F"   #'org-agenda-file-to-front
  "G"   #'org-goto
  "I"   #'org-clock-in
  "L"   #'org-store-link
  "O"   #'org-clock-out
  "P"   #'org-present
  "R"   #'org-refile
  "S"   #'org-sort
  "T"   #'orgtbl-mode
  "a"   #'org-agenda
  "b"   #'org-insert-structure-template
  "c"   #'org-capture
  "d"   #'org-deadline
  "e"   #'org-export-dispatch
  "g"   #'counsel-org-goto-all
  "l"   #'org-insert-link
  "n"   #'org-add-note
  "o"   #'org-open-at-point
  "p"   #'org-set-property
  "s"   #'org-schedule
  "t"   #'evil-org-org-insert-todo-heading-respect-content-below)
(defvar-keymap my-q-spc-map
  :doc "My <spc> prefix keys."
  "q"   #'my/save-all-kill-emacs-no-prompt
  "s"   #'save-buffers-kill-emacs)
(defvar-keymap my-r-spc-map
  :doc "My <spc> prefix keys."
  "l"   #'consult-register-load
  "r"   #'counsel-mark-ring
  "s"   #'consult-register-store
  "v"   #'exchange-point-and-mark)
(defvar-keymap my-s-spc-map
  :doc "My <spc> prefix keys."
  "O"   #'occur
  "R"   #'query-replace-regexp
  "o"   #'consult-outline
  "r"   #'query-replace
  "s"   #'swiper
  "w"   #'eww)
(defvar-keymap my-tc-spc-map
  :doc "My <spc> prefix keys."
  "h"   #'keycast-header-line-mode
  "l"   #'keycast-log-mode
  "m"   #'keycast-mode-line-mode
  "t"   #'keycast-tab-bar-mode)
(defvar-keymap my-ti-spc-map
  :doc "My <spc> prefix keys."
  "c"   #'display-fill-column-indicator-mode
  "i"   #'indent-guide-mode
  "n"   #'whitespace-newline-mode
  "p"   #'whitespace-page-delimiters-mode
  "s"   #'whitespace-mode)
(defvar-keymap my-tl-spc-map
  :doc "My <spc> prefix keys."
  "b"   #'toggle-truncate-lines
  "n"   #'display-line-numbers-mode
  "v"   #'visual-line-mode)
(defvar-keymap my-t-spc-map
  :doc "My <spc> prefix keys."
  "c"   my-tc-spc-map
  "d"   #'display-time-mode
  "e"   #'evil-emacs-cursor-model-mode
  "f"   #'mixed-pitch-mode
  "h"   #'global-hl-line-mode
  "i"   my-ti-spc-map
  "l"   my-tl-spc-map
  "m"   #'mode-line-invisible-mode
  "o"   #'outline-minor-mode
  "p"   #'prettify-symbols-mode
  "r"   #'rainbow-mode
  "s"   #'flyspell-mode
  "t"   #'my/toggle-faces
  "w"   #'writegood-mode
  "¨"   #'tab-bar-mode)
(defvar-keymap my-xz-spc-map
  :doc "My <spc> prefix keys."
  "g"   #'global-text-scale-adjust
  "l"   #'text-scale-adjust)
(defvar-keymap my-x-spc-map
  :doc "My <spc> prefix keys."
  "D"   #'evil-downcase
  "U"   #'evil-upcase
  "c"   #'transpose-chars
  "l"   #'transpose-lines
  "p"   #'transpose-paragraphs
  "s"   #'transpose-sentences
  "u"   #'insert-char
  "w"   #'transpose-words
  "x"   #'just-one-space
  "z"   my-xz-spc-map)
(defvar-keymap my-root-spc-map
  :doc "The root of my <spc> prefix keys."
  "SPC" #'counsel-M-x
  "TAB" #'mode-line-other-buffer
  "0"   #'delete-window
  "1"   #'my/toggle-window-maximize
  "2"   #'split-window-below
  "3"   #'split-window-right
  "4"   #'my/4-windows
  "5"   #'my/ace-swap-window
  "6"   #'ace-window
  "7"   #'transpose-frame
  "8"   #'rotate-frame-anticlockwise
  "9"   #'other-window-prefix
  "a"   my-a-spc-map
  "b"   my-b-spc-map
  "f"   my-f-spc-map
  "g"   my-g-spc-map
  "h"   help-map
  "j"   #'evil-avy-goto-char-timer
  "l"   my-l-spc-map
  "n"   my-n-spc-map
  "o"   my-o-spc-map
  "q"   my-q-spc-map
  "r"   my-r-spc-map
  "s"   my-s-spc-map
  "t"   my-t-spc-map
  "u"   #'universal-argument
  "w"   evil-window-map
  "x"   my-x-spc-map
  "¨"   #'tab-bar-close-tab-by-name)

;; ----------------------------------------------------------------------------
;; `which-key' leader key replacements.
(which-key-add-keymap-based-replacements my-a-spc-map
  "C"   '("Full calc"    . full-calc)
  "c"   '("Calc"         . calc)
  "s"   '("Eshell"       . eshell)
  "u"   '("Undo tree"    . undo-tree-visualize))
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
  "a"   '("Agenda"       . my/find-agenda-file)
  "d"   '("Dired"        . dired-jump)
  "e"   '("Ediff"        . ediff-files)
  "f"   '("Find"         . counsel-find-file)
  "i"   '("Init"         . my/find-init-file)
  "n"   '("Notes"        . my/find-note-file)
  "p"   '("At point"     . find-file-at-point)
  "r"   '("Recent"       . recentf-open-files)
  "s"   '("Save"         . basic-save-buffer)
  "t"   '("My themes"    . my/find-my-faces-file)
  "w"   '("Write"        . write-file))
(which-key-add-keymap-based-replacements my-g-spc-map
  "b"   '("Blame"        . magit-blame)
  "c"   '("Commit"       . my/git-stage-all-and-commit)
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
  "E"   '("Latex pdf"    . org-latex-export-to-pdf)
  "F"   '("Agenda file"  . org-agenda-file-to-front)
  "G"   '("Goto"         . org-goto)
  "I"   '("Clock in"     . org-clock-in)
  "L"   '("Store link"   . org-store-link)
  "O"   '("Clock out"    . org-clock-out)
  "P"   '("Present"      . org-present)
  "R"   '("Refile"       . org-refile)
  "S"   '("Sort"         . org-sort)
  "T"   '("Tables"       . orgtbl-mode)
  "a"   '("Agenda"       . org-agenda)
  "b"   '("Block"        . org-insert-structure-template)
  "c"   '("Capture"      . org-capture)
  "d"   '("Deadline"     . org-deadline)
  "e"   '("Export"       . org-export-dispatch)
  "g"   '("Goto head"    . counsel-org-goto-all)
  "l"   '("Ins. link"    . org-insert-link)
  "n"   '("Add note"     . org-add-note)
  "o"   '("Open link"    . org-open-at-point)
  "p"   '("Property"     . org-set-property)
  "s"   '("Schedule"     . org-schedule)
  "t"   '("New todo"     . evil-org-org-insert-todo-heading-respect-content-below))
(which-key-add-keymap-based-replacements my-q-spc-map
  "q"   '("Save&kill"    . my/save-all-kill-emacs-no-prompt)
  "s"   '("Prompt&kill"  . save-buffers-kill-emacs))
(which-key-add-keymap-based-replacements my-r-spc-map
  "l"   '("Load"         . consult-register-load)
  "r"   '("Mini marks"   . counsel-mark-ring)
  "s"   '("Store"        . consult-register-store)
  "v"   '("Visual mark"  . exchange-point-and-mark))
(which-key-add-keymap-based-replacements my-s-spc-map
  "O"   '("Occur"        . occur)
  "R"   '("Rep. regex"   . query-replace-regexp)
  "o"   '("Outline"      . consult-outline)
  "r"   '("Replace"      . query-replace)
  "s"   '("Swiper"       . swiper)
  "w"   '("Web (eww)"    . eww))
(which-key-add-keymap-based-replacements my-tc-spc-map
  "h"   '("Header"       . keycast-header-line-mode)
  "l"   '("Log frame"    . keycast-log-mode)
  "m"   '("Mode line"    . keycast-mode-line-mode)
  "t"   '("Tab bar"      . keycast-tab-bar-mode))
(which-key-add-keymap-based-replacements my-ti-spc-map
  "c"   '("Column 79"    . display-fill-column-indicator-mode)
  "i"   '("Indentation"  . indent-guide-mode)
  "n"   '("Newline"      . whitespace-newline-mode)
  "p"   '("Page"         . whitespace-page-delimiters-mode)
  "s"   '("Spaces"       . whitespace-mode))
(which-key-add-keymap-based-replacements my-tl-spc-map
  "b"   '("Breaks"       . toggle-truncate-lines)
  "n"   '("Numbers"      . display-line-numbers-mode)
  "v"   '("Visual"       . visual-line-mode))
(which-key-add-keymap-based-replacements my-t-spc-map
  "c"   `("Cast keys"    . ,my-tc-spc-map)
  "d"   '("Date/time"    . display-time-mode)
  "e"   '("Evil cursor"  . evil-emacs-cursor-model-mode)
  "f"   '("Font pitch"   . mixed-pitch-mode)
  "h"   '("Hl line"      . global-hl-line-mode)
  "i"   `("Indicate"     . ,my-ti-spc-map)
  "l"   `("Line"         . ,my-tl-spc-map)
  "m"   '("Mode line"    . mode-line-invisible-mode)
  "o"   '("Outline"      . outline-minor-mode)
  "p"   '("Prettify"     . prettify-symbols-mode)
  "r"   '("Rainbow"      . rainbow-mode)
  "s"   '("Spell"        . flyspell-mode)
  "t"   '("Theme"        . my/toggle-faces)
  "w"   '("Write good"   . writegood-mode)
  "¨"   '("Tab bar mode" . tab-bar-mode))
(which-key-add-keymap-based-replacements my-xz-spc-map
  "g"   '("Global"       . global-text-scale-adjust)
  "l"   '("Local"        . text-scale-adjust))
(which-key-add-keymap-based-replacements my-x-spc-map
  "D"   '("Downcase"     . evil-downcase)
  "U"   '("Upcase"       . evil-upcase)
  "c"   '("Swap chars"   . transpose-chars)
  "l"   '("Swap lines"   . transpose-lines)
  "p"   '("S.paragraphs" . transpose-paragraphs)
  "s"   '("S.sentences"  . transpose-sentences)
  "u"   '("Unicode"      . insert-char)
  "w"   '("Swap words"   . transpose-words)
  "x"   '("One space"    . just-one-space)
  "z"   `("Zoom"         . ,my-xz-spc-map))
(which-key-add-keymap-based-replacements my-root-spc-map
  "SPC" '("M-x"          . counsel-M-x)
  "TAB" '("Toggle buf."  . mode-line-other-buffer)
  "0"   '("Del.window"   . delete-window)
  "1"   '("Maximize"     . my/toggle-window-maximize)
  "2"   '("Below"        . split-window-below)
  "3"   '("Right"        . split-window-right)
  "4"   '("Four"         . my/4-windows)
  "5"   '("Swap"         . my/ace-swap-window)
  "6"   '("Select"       . ace-window)
  "7"   '("Transpose"    . transpose-frame)
  "8"   '("Rotate"       . rotate-frame-anticlockwise)
  "9"   '("Prefix"       . other-window-prefix)
  "a"   `("App"          . ,my-a-spc-map)
  "b"   `("Buffers"      . ,my-b-spc-map)
  "f"   `("Files"        . ,my-f-spc-map)
  "g"   `("Git"          . ,my-g-spc-map)
  "h"   `("Help"         . ,help-map)
  "j"   '("Jump"         . evil-avy-goto-char-timer)
  "l"   `("Lisp"         . ,my-l-spc-map)
  "n"   `("Narrow"       . ,my-n-spc-map)
  "o"   `("Org"          . ,my-o-spc-map)
  "q"   `("Quit"         . ,my-q-spc-map)
  "r"   `("Register"     . ,my-r-spc-map)
  "s"   `("Search"       . ,my-s-spc-map)
  "t"   `("Toggle"       . ,my-t-spc-map)
  "u"   '("Uni.Arg."     . universal-argument)
  "w"   `("Window"       . ,evil-window-map)
  "x"   `("Text"         . ,my-x-spc-map)
  "¨"   '("Close tab"    . tab-bar-close-tab-by-name))

;;; keymaps.el ends here
