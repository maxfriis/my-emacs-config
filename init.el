;; -*- lexical-binding: t; -*-
;; #+title: Emacs config init.el

;; ============================================================================
;;; Vanilla faces
;; ============================================================================
;; 3 sizes: small (4/5=0.8), normal (1.0) and large (5/4=1.25).
;; 8 colors: #??? combining f, 9 and 0, and some grayish colors.
;; |------+----------------+------+----------------+------+-----------------|
;; | #bba | default text   | #09f | link/timestamp | #0f9 | comment/tags    |
;; | #332 | shadow/hl-line | #9f0 | success/done   | #f90 | warning/heading |
;; | #221 | background     | #f09 | error/todo     | #90f | not used        |
;; |------+----------------+------+----------------+------+-----------------|
;; Dark, warm, simple, systematic, predictable and aesthetically pleasing.
;; ----------------------------------------------------------------------------
;; When I configure evil I add 8 colors reserved for the cursor and depending
;; on the evil state. Unspecified faces are handled by the vanilla theme.
;; ----------------------------------------------------------------------------
(set-face-attribute
 'fixed-pitch
 nil              :font "Ubuntu Mono")
(set-face-attribute
 'variable-pitch
 nil :height 140  :font "Ubuntu")
(set-face-attribute
 'error
 nil              :foreground "#f09")
(set-face-attribute
 'warning
 nil              :foreground "#f90")
(set-face-attribute
 'success
 nil              :foreground "#9f0")
(set-face-attribute
 'shadow
 nil              :foreground "#332")
(set-face-attribute
 'match
 nil                                 :background "#332")
;; ----------------------------------------------------------------------------
;; font-lock faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'font-lock-builtin-face
 nil              :foreground "#9f0")
(set-face-attribute
 'font-lock-comment-face
 nil              :foreground "#0f9")
(set-face-attribute
 'font-lock-constant-face
 nil              :foreground "#f90")
(set-face-attribute
 'font-lock-function-name-face
 nil              :foreground "#9f0")
(set-face-attribute
 'font-lock-keyword-face
 nil              :foreground "#09f")
(set-face-attribute
 'font-lock-string-face
 nil              :foreground "#0f9")
;; font-lock-bracket-face
;; font-lock-comment-delimiter-face
;; font-lock-delimiter-face
;; font-lock-doc-face
;; font-lock-doc-markup-face
;; font-lock-escape-face
;; font-lock-function-call-face
;; font-lock-misc-punctuation-face
;; font-lock-negation-char-face
;; font-lock-number-face
;; font-lock-operator-face
;; font-lock-preprocessor-face
;; font-lock-property-name-face
;; font-lock-property-use-face
;; font-lock-punctuation-face
;; font-lock-type-face
;; font-lock-variable-use-face
;; font-lock-warning-face
;; ----------------------------------------------------------------------------
;; Decorations
;; ----------------------------------------------------------------------------
(set-face-attribute
 'highlight
 nil                                 :background "#332")
(set-face-attribute
 'vertical-border
 nil              :foreground "#332")
(set-face-attribute
 'fringe
 nil              :foreground "#332" :background "#221")
(set-face-attribute
 'line-number
 nil :height 0.8  :foreground "#332" :background "#221")
(set-face-attribute
 'line-number-current-line
 nil              :foreground "#221" :background "#332")
(set-face-attribute
 'mode-line
 nil :height 0.8  :foreground "#221" :background "#332" :box nil)
(set-face-attribute
 'mode-line-inactive
 nil              :foreground "#332" :background "#221" :box nil :overline t)
;; ----------------------------------------------------------------------------
;; Tab bar
;; ----------------------------------------------------------------------------
(set-face-attribute
 'tab-bar
 nil :height 0.8  :foreground "#332" :background "#221" :weight 'bold :inherit 'default)
(set-face-attribute
 'tab-bar-tab
 nil              :foreground "#221" :background "#332" :box nil)
(set-face-attribute
 'tab-bar-tab-inactive
 nil              :foreground "#332" :background "#221" :box nil)
;; ----------------------------------------------------------------------------
;; Color keywords
;; ----------------------------------------------------------------------------
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("add-hook"           . font-lock-keyword-face)
   ("add-to-list"        . font-lock-keyword-face)
   ("set-face-attribute" . font-lock-keyword-face)))

;; ============================================================================
;;; Modeline
;; ============================================================================
(setq
 mode-line-buffer-identification-keymap nil)
(setq-default ; modeline need setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-modified ; "%*"
   " "
   mode-line-buffer-identification
   " "
   (:eval
    (propertize
     (concat ; short major mode
      "⌞"
      (string-replace
       "-" " "
       (replace-regexp-in-string
        "^org-" ""
        (replace-regexp-in-string
         "^emacs-" ""
         (replace-regexp-in-string
          "-buffer$" ""
          (replace-regexp-in-string
           "-mode$" ""
           (downcase (symbol-name major-mode)))))))
      "⌝")
     'help-echo (concat
                 (symbol-name major-mode) ; echo the full mode name when hovering
                 ", mouse-1: Toggle last 2 buffers")
     'local-map (make-mode-line-mouse-map
                 'mouse-1 #'mode-line-other-buffer)
     'mouse-face 'mode-line-highlight))
   (:eval
    (when (mode-line-window-selected-p) ; only include in the active window
      (list
       (if (eq vc-mode nil) ; version control
           ""
         ;; else
         (replace-regexp-in-string
          "^ Git" " "
          vc-mode))
       (when (buffer-narrowed-p) ; narrowed
         (list
          " "
          (propertize
           "[n]"
           'help-echo "Narrowed, mouse-1: widen"
           'local-map (make-mode-line-mouse-map
                       'mouse-1 #'mode-line-widen)
           'mouse-face 'mode-line-highlight)))
       " "
       mode-line-misc-info
       (propertize ; gap for alignment
        " "
        'display '((space :align-to (- (+ right right-fringe right-margin) 9)))
        'face    'mode-line-inactive)
       " "
       mode-line-percent-position
       mode-line-position-column-format)))))

;; ============================================================================
;;; Vanilla variables
;; ============================================================================
(setq-default ; need to be default
 ;; dired-omit-files-p t
 indent-tabs-mode nil)
(setq
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      "%b")))
 visible-bell t
 warning-minimum-level :error
 initial-scratch-message nil
 large-file-warning-threshold nil
 use-dialog-box nil
 use-short-answers t
 delete-by-moving-to-trash t
 trash-directory "~/.local/share/Trash/files"
 custom-file "/dev/null"
 dired-kill-when-opening-new-dired-buffer t
 dired-listing-switches "-agho --group-directories-first"
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-omit-verbose nil
 dired-omit-files "\\`\\.\\|\\`#"
 global-auto-revert-non-file-buffers t ; update dired buffer
 recentf-exclude
 '("\\`~/org/agenda/.*\\.org\\'" "\\`~/\\.emacs\\.d/.*")
 tab-width 4
 display-line-numbers-type 'relative
 display-time-format "[%Y-%m-%d %a %H:%M]"
 display-buffer-alist
 '(("\\*Help\\*"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (body-function . select-window)))
 shell-default-shell 'eshell ; I use a terminal outside emacs when needed
 eshell-ls-initial-args
 '("-agho")
 ;; ----------------------------------------------------------------------------
 ;; Incremental search
 ;; ----------------------------------------------------------------------------
 isearch-lazy-count t
 lazy-count-prefix-format "%s/%s "
 lazy-count-suffix-format nil
 ;; ----------------------------------------------------------------------------
 ;; Tab bar
 ;; ----------------------------------------------------------------------------
 tab-bar-show 1
 tab-bar-close-button-show      nil
 tab-bar-new-button             nil
 tab-bar-close-last-tab-choice 'tab-bar-mode
 tab-bar-close-tab-select      'recent
 tab-bar-new-tab-to            'right
 tab-bar-new-tab-choice        "*scratch*"
 tab-bar-separator             " "
 tab-bar-menu-bar-button       "☰"
 tab-bar-format
 '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator)
 ;; ----------------------------------------------------------------------------
 ;; Org variables used in functions
 ;; ----------------------------------------------------------------------------
 org-directory "~/org/"
 org-agenda-directory (concat org-directory "agenda/"))
;; Org directories and an empty inbox.org unless they exist. Good for fresh install
(unless (file-exists-p org-directory)
  (make-directory org-directory))
(unless (file-exists-p org-agenda-directory)
  (make-directory org-agenda-directory))
(unless (file-exists-p (concat org-directory "inbox.org"))
  (make-empty-file (concat org-directory "inbox.org")))
;; ----------------------------------------------------------------------------
;; Vanilla global modes
;; ----------------------------------------------------------------------------
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)

;; ============================================================================
;;; My custom functions
;; ============================================================================
;; Open init files
;; ----------------------------------------------------------------------------
(defun my/open-init-files ()
  "Open config file init.el"
  (interactive)
  (find-file (locate-user-emacs-file "early-init.el")) ; to easily switch
  (find-file (locate-user-emacs-file "init.el")))
;; ----------------------------------------------------------------------------
;; Open notes file
;; ----------------------------------------------------------------------------
(defun my/open-note-file ()
  "Open my notes file note.org"
  (interactive)
  (find-file (concat org-agenda-directory "note.org")))
;; ----------------------------------------------------------------------------
;; Open agenda file
;; ----------------------------------------------------------------------------
(defun my/open-agenda-file ()
  "Open my agenda file agenda.org"
  (interactive)
  (find-file (concat org-agenda-directory "plan.org")) ; to easily switch
  (find-file (concat org-agenda-directory "agenda.org")))
;; ----------------------------------------------------------------------------
;; Custom agenda
;; ----------------------------------------------------------------------------
(defun my/org-agenda-custom ()
  "Custom agenda with NEXT, agenda and TODO/HOLD"
  (interactive)
  (org-agenda nil "c")
  (unless (eq ?\s (char-after)) ; when the char at point is not a space
    (org-agenda-goto-today)))
;; ----------------------------------------------------------------------------
;; Capture idea
;; ----------------------------------------------------------------------------
(defun my/org-capture-idea ()
  "Idea capture to inbox.org"
  (interactive)
  (org-capture nil "i")
  (org-save-all-org-buffers))
;; ----------------------------------------------------------------------------
;; Ace window swap
;; ----------------------------------------------------------------------------
(defun my/ace-swap-window ()
  "Swap two windows (prompt if 3+) focus the current window"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))
;; ----------------------------------------------------------------------------
;; 3-window setup
;; ----------------------------------------------------------------------------
(defun my/3-windows ()
  "3 windows, two on the right and the left focused"
  (interactive)
  (delete-other-windows)
  (split-window-right) ; reading window
  (other-window 1)
  (split-window-below) ; notes window
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window -2)) ; back to initial window

;; ============================================================================
;;; Packages
;; ============================================================================
(setq
 load-prefer-newer t ; use .el if newer than .elc
 package-archives
 '(("elpa"  . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/"))
 package-selected-packages
 '(evil evil-collection evil-nerd-commenter evil-surround evil-numbers evil-org
        vertico marginalia orderless consult embark embark-consult lsp-mode
        company company-box winum golden-ratio ace-window transpose-frame
        nerd-icons nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion
        rainbow-delimiters rainbow-mode indent-guide visual-fill-column
        mixed-pitch centered-cursor-mode recursive-narrow avy keycast magit
        undo-tree counsel helpful flycheck writegood-mode which-key general
        org-superstar org-appear org-present auto-package-update))
(require 'package)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)
;; ----------------------------------------------------------------------------
;; Auto update
;; ----------------------------------------------------------------------------
(setq
 auto-package-update-interval 7
 auto-package-update-hide-results t
 auto-package-update-prompt-before-update t)
(require 'auto-package-update)
(auto-package-update-maybe)

;; ============================================================================
;;; Evil
;; ============================================================================
;; Cursors are special. I combine f and 0 to color all types.
;; I tax the eyes a bit to follow the cursor/point and the evil states.
;; |------+----------+------+---------|
;; | #f00 | operator | #0ff | Emacs   |
;; | #0f0 | normal   | #f0f | replace |
;; | #00f | motion   | #ff0 | insert  |
;; | #000 | region   | #fff | visual  |
;; |------+----------+------+---------|
;; Operator state is red to alert. Normal state has the opposite color.
;; Insert (last trafic light color) and replace state use bar vs hbar.
;; Emacs and motion states have the remaining rgb colors.
;; "Input" states have the "lighter" colors (with 2 f's) and bars in common.
;; The visual state is hollow and filled with the region color.
;; ----------------------------------------------------------------------------
(setq
 evil-operator-state-cursor '(box        "#f00")
 evil-normal-state-cursor   '(box        "#0f0")
 evil-motion-state-cursor   '(box        "#00f")
 evil-emacs-state-cursor    '((bar  . 4) "#0ff")
 evil-replace-state-cursor  '((hbar . 4) "#f0f")
 evil-insert-state-cursor   '((bar  . 4) "#ff0")
 evil-visual-state-cursor   '(hollow     "#fff"))
(set-face-attribute
 'region
 nil :background "#000")
(setq
 evil-want-keybinding nil ; evil-collection need this
 evil-want-integration t
 evil-want-C-u-scroll t ; "C-u" still works as vanilla in insert and Emacs state
 evil-ex-substitute-highlight-all nil
 evil-ex-search-persistent-highlight nil
 evil-shift-round t
 evil-undo-system 'undo-tree
 vim-style-remap-Y-to-y$ t)
(require 'evil)
(evil-mode 1)
(require 'evil-collection)
(evil-collection-init)
(require 'evil-nerd-commenter)
(require 'evil-numbers)
(require 'evil-surround)
(global-evil-surround-mode 1)
(with-eval-after-load 'org
  (require 'evil-org)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (add-hook
   'org-mode-hook
   (lambda ()
     evil-org-mode))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
;; ----------------------------------------------------------------------------
;; Undo and save stuff
;; ----------------------------------------------------------------------------
(setq
 undo-tree-visualizer-diff t
 undo-tree-visualizer-timestamps t
 ;; undo-tree-history-directory-alist
 ;; '(("." . "~/.emacs.d/undo"))
 ;; (locate-user-emacs-file) ; will this work with directories?
 undo-tree-auto-save-history t)
(require 'undo-tree)
(global-undo-tree-mode 1)
(setq
 save-place-forget-unreadable-files nil)
(require 'saveplace)
(save-place-mode 1)
(setq
 savehist-save-minibuffer-history 1)
(require 'savehist)
(savehist-mode 1)

;; ============================================================================
;;; Completion
;; ============================================================================
;; Minibuffer
;; ----------------------------------------------------------------------------
(setq
 ;; vertico-cycle t
 vertico-resize nil)
(require 'vertico)
(vertico-mode 1)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy) ; tidy typing directories
(with-eval-after-load 'vertico
  (require 'marginalia)
  (marginalia-mode 1))
(setq
 completion-styles
 '(orderless)) ; other options: (basic substring flex)
(require 'orderless) ; Fuzzy completions
(require 'consult)   ; Combine functionality (e.g. buffers + recentf)
(require 'embark)    ;
;; ----------------------------------------------------------------------------
;; Buffer
;; ----------------------------------------------------------------------------
(require 'lsp-mode)
(setq
 company-minimum-prefix-length 3
 ;; company-show-quick-access t
 company-idle-delay 0)
(require 'company)
(with-eval-after-load 'company
  (require 'company-box))
(add-hook 'company-mode-hook #'company-box-mode)
(company-tng-mode 1)
(global-company-mode 1)

;; ============================================================================
;;; Window manipulation
;; ============================================================================
(setq
 winum-format " %s")
(require 'winum)
(winum-mode 1)
(require 'golden-ratio)
(golden-ratio-mode 1)
(require 'ace-window)
(require 'transpose-frame)

;; ============================================================================
;;; Thumbnails, indentation and colors
;; ============================================================================
(when (display-graphic-p)
  (require 'nerd-icons) ; all-the-icons' alignment sucks. Nerd works
  (require 'nerd-icons-dired)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (require 'nerd-icons-ibuffer)
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  (with-eval-after-load 'marginalia
    (require 'nerd-icons-completion)
    (nerd-icons-completion-mode 1)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))
(require 'indent-guide)
(add-hook 'prog-mode-hook #'indent-guide-mode)
(setq
 visual-fill-column-center-text t)
(setq-default ; for some reason this needs to be default to have effect
 fill-column 90)
(require 'visual-fill-column)
;; ----------------------------------------------------------------------------
;; Colors
;; ----------------------------------------------------------------------------
(setq
 rainbow-delimiters-max-face-count 3)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;; ============================================================================
;;; Help
;; ============================================================================
(require 'counsel)
(require 'helpful)
(bind-key [remap describe-function] 'counsel-describe-function)
(bind-key [remap describe-variable] 'counsel-describe-variable)
(bind-key [remap describe-command]  'helpful-command)
(bind-key [remap describe-key]      'helpful-key)
;; ----------------------------------------------------------------------------
;; Key casting
;; ----------------------------------------------------------------------------
(require 'keycast)
(keycast-tab-bar-mode 1) ; Cast in the tab bar
;; ----------------------------------------------------------------------------
;; Spell checking and writing tips
;; ----------------------------------------------------------------------------
(require 'flycheck)
(add-hook 'org-mode-hook #'flyspell-mode)
(require 'writegood-mode)

;; ============================================================================
;;; Misc packages
;; ============================================================================
(require 'avy)
;; ----------------------------------------------------------------------------
;; Toggle narrow
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org
  (require 'recursive-narrow))
;; ----------------------------------------------------------------------------
;; Toggle pitch
;; ----------------------------------------------------------------------------
(setq
 mixed-pitch-set-height t)
(require 'mixed-pitch)
(dolist (face '(org-special-keyword org-date org-tag org-priority org-todo org-table))
  (add-to-list 'mixed-pitch-fixed-pitch-faces face))
;; ----------------------------------------------------------------------------
;; Scroll lock
;; ----------------------------------------------------------------------------
(require 'centered-cursor-mode)
(global-centered-cursor-mode 1)

;; ============================================================================
;;; Magit
;; ============================================================================
(require 'magit)
(with-eval-after-load 'evil
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert))

;; ============================================================================
;;; Org
;; ============================================================================
;; Variables setting up my enviroment
;; ----------------------------------------------------------------------------
(setq
 org-default-notes-file (concat org-directory "inbox.org")
 org-ellipsis " …"
 ;; ----------------------------------------------------------------------------
 ;; Todo states: I use :category: and refile rather than more keywords
 ;; ----------------------------------------------------------------------------
 org-todo-keywords
 '((type     "NEXT(n!/!)" "TODO(t!/!)" "|")
   (type "|" "HOLD(h@/!)" "DONE(d!/!)"))
 org-list-allow-alphabetical t
 org-list-demote-modify-bullet
 '(("+" . "*")
   ("*" . "-")
   ("-" . "+"))
 org-default-priority ?C
 org-priority-faces ; This affects rendering in agenda
 '((?A . (:height 0.8 :slant nil))
   (?B . (:height 0.8 :slant nil))
   (?C . (:height 0.8 :slant nil)))
 org-tags-column -75 ; minus aligns right
 org-tag-alist
 '(("bg"       . ?b)
   ("comp"     . ?c)
   ("fam"      . ?f)
   ("home"     . ?h)
   ("idea"     . ?i)
   ("money"    . ?m)
   ("phone"    . ?p)
   ("work"     . ?w)
   ;; Special keyword tags
   ("CRYPT"    . ?C)
   ("ORDERED"  . ?O)
   ("NOEXPORT" . ?X))
 org-cycle-hide-block-startup t
 org-confirm-babel-evaluate nil
 ;; ----------------------------------------------------------------------------
 ;; Logbook
 ;; ----------------------------------------------------------------------------
 org-log-into-drawer 'logbook
 org-log-done        'time
 org-log-refile      'time
 org-log-reschedule t
 org-log-note-headings
 '((state       . "State %6s from %-9S %t") ; Align timestamps (with capture)
   (note        . "Note                        %t")
   (refile      . "Refiled                     %t")
   (done        . "Closing note                %t")
   (clock-out   . "Timer stopped               %t")
   (reschedule  . "Rescheduled                 %t from %S")
   (delschedule . "Unscheduled                 %t, was %S")
   (redeadline  . "New deadline                %t from %S")
   (deldeadline . "Deadline removed            %t, was %S"))
 ;; ----------------------------------------------------------------------------
 ;; Capture
 ;; ----------------------------------------------------------------------------
 org-capture-templates
 `(("i" "Idea" entry ; backtick to concat primarely for code readability
    (file ,(concat org-directory "inbox.org"))
    ,(concat
      "* NEXT %^{Idea}\n"
      ":LOGBOOK:\n"
      "- State \"NEXT\" from \"Capture\" %U\n"
      ":END:\n"
      "%i")
    :immediate-finish t :prepend t)
   ("m" "Meeting" entry
    (file+olp ,(concat org-agenda-directory "agenda.org") "Meet")
    ,(concat
      "* TODO %^{Meet who?} %^G\n"
      "SCHEDULED: %^{When?}t\n"
      ":LOGBOOK:\n"
      "- State \"TODO\" from \"Capture\" %U\n"
      ":END:\n"
      "%?%i"))
   ("n" "Note" entry
    (file+datetree ,(concat org-agenda-directory "note.org"))
    ,(concat
      "* %U\n"
      "%?%i")
    :tree-type month)
   ("p" "Project" entry
    (file ,(concat org-agenda-directory "plan.org"))
    ,(concat
      "* TODO %^{Project heading} [/]\n"
      ":PROPERTIES:\n"
      ":ARCHIVE: " org-directory "archive.org::* Log\n"
      ":CATEGORY: %^{Category}\n"
      ":END:\n"
      ":LOGBOOK:\n"
      "- State \"TODO\" from \"Capture\" %U\n"
      ":END:\n")
    :immediate-finish t :prepend t)
   ("t" "Task" entry
    (file+olp ,(concat org-agenda-directory "agenda.org") "Task")
    ,(concat
      "* TODO %^{Do what?} %^G\n"
      ":LOGBOOK:\n"
      "- State \"TODO\" from \"Capture\" %U\n"
      ":END:\n"
      "%?%i"))
   ("x" "Log clipboard" entry
    (file+olp+datetree ,(concat org-directory "archive.org") "Clipboard")
    ,(concat
      "* %^{Log what?|Clipboard}\n"
      ":LOGBOOK:\n"
      "- %U\n"
      ":END:\n"
      "%x")
    :immediate-finish t))
 ;; ----------------------------------------------------------------------------
 ;; Agenda
 ;; ----------------------------------------------------------------------------
 ;; System to organize tasks and supress out of date information
 ;; It's not primarily a calendar. It's all about task management
 org-agenda-window-setup 'current-window
 org-archive-location (concat org-directory "archive.org::* Archive")
 org-refile-targets
 `((,(concat org-agenda-directory "agenda.org") :maxlevel . 1)
   (,(concat org-agenda-directory "plan.org")   :maxlevel . 1))
 org-agenda-files (list (concat org-directory "inbox.org")
                        org-agenda-directory) ; all org files in the agenda dir
 org-agenda-format-date " [%F %a] "
 org-agenda-block-separator ?⎺ ; also separate appended agenda ("a")
 org-agenda-span 'month
 org-agenda-custom-commands
 '(("c" "Custom agenda setup"
    ((todo "NEXT"
           ;; all NEXT (including timestamped and priority)
           ((org-agenda-overriding-header "")))
     (todo "TODO|HOLD|DONE"
           ;; prioritized TODO, HOLD or even DONE.
           ((org-agenda-overriding-header "")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'notregexp org-priority-regexp))))
     (agenda "" ((org-agenda-span 'week))) ; might repeat items from above
     (todo "TODO"
           ;; not every TODO has or even should have a timestamp
           ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'regexp org-priority-regexp ; or
               'timestamp))))
     (todo "HOLD"
           ;; HOLD for third party action pending (include timestamped)
           ((org-agenda-overriding-header "") ; share heading with the item above
            (org-agenda-block-separator nil)  ; don't separate
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'regexp org-priority-regexp)))))))
 org-agenda-prefix-format
 '((agenda   . "  %-6c%-12t%?-6s")
   (timeline . "  %-6c%-12t%?-6s")
   (todo     . "  %-6c%-12e")
   (tags     . "  %-6c%-12e")
   (search   . "  %-6c%-12e"))
 org-agenda-scheduled-leaders
 '("☐" "☐%3dx")
 org-agenda-deadline-leaders
 '("☒" "☒%3dd" "☒%3dx")
 org-agenda-bulk-mark-char ":"
 org-agenda-breadcrumbs-separator ":"
 org-agenda-time-grid nil
 ;; org-agenda-start-with-log-mode t
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 calendar-week-start-day 1
 org-columns-default-format "%30Item %Clocksum(Used) %Effort(Plan) %Category(Cat.) %Tags %Priority(#) %Todo(ToDo)"
 org-global-properties
 '(("effort_all" . "0:05 0:10 0:15 0:20 0:30 0:45 1:00 1:30 2:00"))
 org-clock-in-switch-to-state "NEXT"
 org-clock-out-when-done t
 ;; ----------------------------------------------------------------------------
 ;; Diary
 ;; ----------------------------------------------------------------------------
 org-agenda-include-diary t
 diary-mark-entries t
 holiday-bahai-holidays    nil
 holiday-hebrew-holidays   nil
 holiday-islamic-holidays  nil
 holiday-oriental-holidays nil
 holiday-general-holidays  nil
 holiday-other-holidays ; Custom diary formated
 '((holiday-float  5  0  2 "Mors dag")
   (holiday-float 12  0 -5 "1. søndag i advent")
   (holiday-fixed  3  8    "Kvindernes kamp dag")
   (holiday-fixed  3 14    "π dag")
   (holiday-fixed  5  1    "Arbejdernes kamp dag")
   (holiday-fixed  6  5    "Grundlovs og fars dag")
   (holiday-fixed  6 23    "Sanct Hans")
   (holiday-fixed 12 24    "Juleaften"))
 ;; ----------------------------------------------------------------------------
 ;; Export
 ;; ----------------------------------------------------------------------------
 org-html-postamble      nil
 org-latex-title-command nil
 org-export-with-smart-quotes t
 org-export-backends
 '(ascii beamer html latex md odt org)
 org-file-apps
 '(("\\.docx\\'"    . default)
   ("\\.mm\\'"      . default)
   ("\\.x?html?\\'" . default)
   ("\\.pdf\\'"     . "evince %s") ; open pdf outside Emacs
   (directory       . emacs)
   (auto-mode       . emacs)))
(require 'org)
;; ----------------------------------------------------------------------------
;; A pictogram is often better than a word
;; ----------------------------------------------------------------------------
(add-hook
 'org-mode-hook
 (lambda ()
   (setq
    prettify-symbols-alist ; utf8's that work with most fonts even in the terminal
    '(("[-]"            . ?⊟) ; Not a "ballot" icon
      ("[ ]"            . ?☐)
      ("[X]"            . ?☒)
      ("CLOSED:"        . ?☑) ; The checkmark displays different in some fonts
      ("SCHEDULED:"     . ?☐) ; That's OK because the "CLOSED:" inactive timestamp
      ("DEADLINE:"      . ?☒) ; is different from the active timestamps
      (":PROPERTIES:"   . ?⚙) ; Settings
      (":LOGBOOK:"      . ?☰) ; Meta data
      ("CLOCK:"         . ?–) ; Items in logbook have a dash bullet
      (":END:"          . ?✐)
      ("#+begin_export" . ?✎)
      ("#+end_export"   . ?✐)
      ("#+begin_src"    . ?✎)
      ("#+end_src"      . ?✐))
    prettify-symbols-unprettify-at-point t)
   (prettify-symbols-mode 1)))
;; ----------------------------------------------------------------------------
;; Insert state on org-capture and on org-add-note/org-refile
;; ----------------------------------------------------------------------------
(with-eval-after-load 'evil
  (add-hook 'org-capture-mode-hook     #'evil-insert-state)
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state))
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(setq
 org-superstar-cycle-headline-bullets nil
 org-superstar-headline-bullets-list
 '(?① ?② ?③ ?④ ?ⓧ))
(require 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)
;; ----------------------------------------------------------------------------
;; Habits
;; ----------------------------------------------------------------------------
(add-to-list 'org-modules 'org-habit t)
(setq
 org-habit-preceding-days 28
 org-habit-graph-column   60
 org-appear-autolinks t)
(require 'org-appear)
(add-hook 'org-mode-hook #'org-appear-mode)
;; ----------------------------------------------------------------------------
;; Present
;; ----------------------------------------------------------------------------
(with-eval-after-load 'evil-collection
  (setq-local
   org-blank-before-new-entry
   '((heading . t)
     (plain-list-item . nil)))
  (require 'org-present)
  (evil-collection-org-present-setup))

;; ============================================================================
;;; General
;; ============================================================================
(setq
 which-key-idle-delay 0)
(require 'which-key)
(which-key-mode 1)
(require 'general)
(general-evil-setup t)
(general-auto-unbind-keys t)
(general-create-definer my/set-leader-keys
  :keymaps '(motion visual normal insert emacs)
  :prefix "SPC"
  :global-prefix "C-SPC") ; Visual mode sets the mark
(my/set-leader-keys
  "" nil
  "SPC" '(counsel-M-x                            :which-key "M-x")
  ;; this will toggle back and forth between 2 buffers.
  "TAB" '(mode-line-other-buffer                 :which-key "Toggle buf")
  "0"   '(delete-window                          :which-key "Del win")
  "1"   '(delete-other-windows                   :which-key "Max win")
  "2"   '(split-window-below                     :which-key "Win below")
  "3"   '(split-window-right                     :which-key "Win right")
  "4"   '(my/3-windows                           :which-key "Three win")
  "5"   '(my/ace-swap-window                     :which-key "Swap win")
  "6"   '(rotate-frame-clockwise                 :which-key "Rot. frame")
  "7"   '(transpose-frame                        :which-key "Transpose")
  "8"   '(dired-other-window                     :which-key "Dired win")
  "9"   '(other-window-prefix                    :which-key "Win prefix")
  "¨"   '(previous-buffer                        :which-key "Prev buf")
  "´"   '(other-window                           :which-key "Prev win")
  "½"   '(tab-bar-close-tab-by-name              :which-key "Close tab")
  "a"   '(:ignore t                              :which-key "Apps")
  "aC"  '(full-calc                              :which-key "Full calc")
  "ac"  '(calc                                   :which-key "Calc")
  "as"  '(eshell                                 :which-key "Eshell")
  "au"  '(undo-tree-visualize                    :which-key "Undo tree")
  "b"   '(:ignore t                              :which-key "Buffer")
  "bb"  '(consult-buffer                         :which-key "Buffers")
  "bd"  '(kill-this-buffer                       :which-key "Delete")
  "bi"  '(ibuffer                                :which-key "IBuffer")
  "bj"  '(next-buffer                            :which-key "Next")
  "bk"  '(previous-buffer                        :which-key "Previous")
  "bo"  '(counsel-switch-buffer-other-window     :which-key "Other win")
  "bs"  '(scratch-buffer                         :which-key "Scratch")
  "c"   '(:ignore t                              :which-key "c")
  "d"   '(:ignore t                              :which-key "d")
  "e"   '(eval-last-sexp                         :which-key "Eval sexp")
  "f"   '(:ignore t                              :which-key "Files")
  "fS"  '(save-some-buffers                      :which-key "Save all")
  "fa"  '(my/open-agenda-file                    :which-key "Agenda file")
  "fd"  '(dired-jump                             :which-key "Dired")
  "ff"  '(counsel-find-file                      :which-key "Find")
  "fi"  '(my/open-init-files                     :which-key "Init")
  "fP"  '(find-file-at-point                     :which-key "At point")
  "fn"  '(my/open-note-file                      :which-key "Note file")
  "fr"  '(recentf-open-files                     :which-key "Recent")
  "fs"  '(basic-save-buffer                      :which-key "Save")
  "fw"  '(write-file                             :which-key "Save as")
  "g"   '(:ignore t                              :which-key "Git")
  "gb"  '(magit-blame                            :which-key "Blame")
  "gg"  '(magit                                  :which-key "Magit")
  "gs"  '(magit-status                           :which-key "Status")
  "h"   '(:ignore t                              :which-key "Help")
  "hC"  '(helpful-command                        :which-key "Command")
  "hF"  '(counsel-describe-face                  :which-key "Face")
  "hM"  '(view-echo-area-messages                :which-key "Messages")
  "hb"  '(counsel-descbinds                      :which-key "Bindings")
  "hc"  '(describe-char                          :which-key "Char")
  "hf"  '(counsel-describe-function              :which-key "Function")
  "hk"  '(helpful-key                            :which-key "Key")
  "hm"  '(describe-mode                          :which-key "Mode")
  "hv"  '(counsel-describe-variable              :which-key "Variable")
  "i"   '(:ignore t                              :which-key "Insert")
  "j"   '(avy-goto-char-timer                    :which-key "Jump")
  "k"   '(:ignore t                              :which-key "k")
  "l"   '(:ignore t                              :which-key "Lisp")
  "m"   '(:ignore t                              :which-key "Mode")
  "n"   '(:ignore t                              :which-key "Narrow")
  "nf"  '(narrow-to-defun                        :which-key "Function")
  "nn"  '(recursive-narrow-or-widen-dwim         :which-key "Dwim")
  "no"  '(org-narrow-to-subtree                  :which-key "Org tree")
  "nr"  '(narrow-to-region                       :which-key "Region")
  "nw"  '(widen                                  :which-key "Widen")
  "o"   '(:ignore t                              :which-key "Org")
  "o."  '(org-time-stamp                         :which-key "Timestamp")
  "oA"  '(org-archive-subtree-default            :which-key "Archive")
  "oE"  '(org-latex-export-to-pdf                :which-key "Latex pdf")
  "oG"  '(org-goto                               :which-key "Goto")
  "oI"  '(org-clock-in                           :which-key "Clock in")
  "oL"  '(org-store-link                         :which-key "Store link")
  "oO"  '(org-clock-out                          :which-key "Clock out")
  "oR"  '(org-refile                             :which-key "Refile")
  "oS"  '(org-sort                               :which-key "Sort")
  "oP"  '(org-present                            :which-key "Present")
  "oa"  '(org-agenda                             :which-key "Agenda")
  "ob"  '(org-insert-structure-template          :which-key "Block")
  "oc"  '(org-capture                            :which-key "Capture")
  "od"  '(org-deadline                           :which-key "Deadline")
  "oe"  '(org-export-dispatch                    :which-key "Export")
  "og"  '(counsel-org-goto-all                   :which-key "Goto head")
  "ol"  '(org-insert-link                        :which-key "Ins. link")
  "on"  '(org-add-note                           :which-key "Add note")
  "oo"  '(org-open-at-point                      :which-key "Open link")
  "op"  '(org-set-property                       :which-key "Property")
  "os"  '(org-schedule                           :which-key "Schedule")
  "ot"  '(evil-org-org-insert-todo-heading-respect-content-below :which-key "New ToDo")
  "p"   '(:ignore t                              :which-key "p")
  "q"   '(:ignore t                              :which-key "Quit")
  "qq"  '(save-buffers-kill-emacs                :which-key "Quit Emacs")
  "r"   '(:ignore t                              :which-key "r")
  "s"   '(:ignore t                              :which-key "Search")
  "sO"  '(consult-outline                        :which-key "Outline")
  "si"  '(consult-isearch-forward                :which-key "isearch")
  "so"  '(occur                                  :which-key "Occur")
  "sr"  '(query-replace                          :which-key "Replace")
  "ss"  '(swiper                                 :which-key "Swiper")
  "t"   '(:ignore t                              :which-key "Toggle")
  "tc"  '(visual-fill-column-mode                :which-key "Center col")
  "tf"  '(mixed-pitch-mode                       :which-key "Font pitch")
  "tg"  '(golden-ratio-mode                      :which-key "Gold ratio")
  "th"  '(hl-line-mode                           :which-key "Hl line")
  "tk"  '(keycast-tab-bar-mode                   :which-key "Keycast")
  "tp"  '(prettify-symbols-mode                  :which-key "Prettify")
  "tr"  '(rainbow-mode                           :which-key "Rainbow")
  "ts"  '(flyspell-mode                          :which-key "Spell")
  "tt"  '(font-lock-mode                         :which-key "Textcolor")
  "tw"  '(writegood-mode                         :which-key "Write good")
  "u"   '(universal-argument                     :which-key "Uni arg")
  "v"   '(exchange-point-and-mark                :which-key "Visual swap")
  "w"   '(:ignore t                              :which-key "Window")
  "wb"  '(evil-window-split                      :which-key "Split below")
  "wd"  '(evil-window-delete                     :which-key "Delete")
  "wh"  '(evil-window-left                       :which-key "Left")
  "wj"  '(evil-window-down                       :which-key "Down")
  "wk"  '(evil-window-up                         :which-key "Up")
  "wl"  '(evil-window-right                      :which-key "Right")
  "wr"  '(evil-window-vsplit                     :which-key "Split right")
  "wR"  '(rotate-frame-clockwise                 :which-key "Rot. frame")
  "ws"  '(ace-select-window                      :which-key "Select")
  "wS"  '(my/ace-swap-window                     :which-key "Swap")
  "wt"  '(transpose-frame                        :which-key "Transpose")
  "ww"  '(delete-other-windows                   :which-key "Max")
  "x"   '(:ignore t                              :which-key "Text")
  "xU"  '(evil-upcase                            :which-key "Upcase")
  "xc"  '(insert-char                            :which-key "Unicode")
  "xs"  '(just-one-space                         :which-key "One space")
  "xt"  '(orgtbl-mode                            :which-key "Org tables")
  "xu"  '(evil-downcase                          :which-key "Downcase")
  "y"   '(:ignore t                              :which-key "y")
  "z"   '(text-scale-adjust                      :which-key "Zoom")
  ;; Danish
  "æ"   '(:ignore t                              :which-key "æ")
  "ø"   '(:ignore t                              :which-key "ø")
  "å"   '(:ignore t                              :which-key "å"))

;; ============================================================================
;;; Keybindings
;; ============================================================================
(bind-key* "¤" 'evil-normal-state) ; for phone keyboard with no esc
(bind-keys
 ;; ----------------------------------------------------------------------------
 ;; Global keys!!! (minor mode maps will overwrite)
 ;; ----------------------------------------------------------------------------
 ("<escape>"      . keyboard-escape-quit)
 ("<Scroll_Lock>" . centered-cursor-mode)
 ("<next>"        . evil-scroll-down)
 ("<prior>"       . evil-scroll-up)
 ("C-<tab>"       . tab-next)
 ("C-S-<tab>"     . tab-previous)
 ;; ----------------------------------------------------------------------------
 ;; Operator state keys!!!
 ;; ----------------------------------------------------------------------------
 :map
 evil-operator-state-map
 ;; Bad things happen if I hit "å" (next to "ø") in operator state
 ("å"             . keyboard-escape-quit)
 ;; ----------------------------------------------------------------------------
 ;; Motion state keys!!! (normal, visual and motion)
 ;; ----------------------------------------------------------------------------
 :map
 evil-motion-state-map
 ("<down>"        . evil-next-visual-line)     ; up/down keys navigate wrapped lines while
 ("<up>"          . evil-previous-visual-line) ; j/k respect the line atom approach in vim
 ("´"             . evil-window-next)
 ("¨"             . evil-next-buffer)
 ("½"             . tab-new)
 ("C-½"           . tab-bar-mode)
 ("M-p"           . consult-yank-pop)
 ("Æ"             . evil-backward-paragraph)
 ("æ"             . evil-forward-paragraph)
 ("Ø"             . evil-first-non-blank)
 ("ø"             . evil-end-of-line)
 ("Å"             . my/org-capture-idea)
 ("å"             . my/org-agenda-custom)
 ("gc"            . evilnc-comment-operator) ; package
 ("g1"            . winum-select-window-1)   ; package
 ("g2"            . winum-select-window-2)   ; package
 ("g3"            . winum-select-window-3)   ; package
 ;; ----------------------------------------------------------------------------
 ;; Visual state keys!!!
 ;; ----------------------------------------------------------------------------
 :map
 evil-visual-state-map
 ;; "/" is not easily available on my keyboard so I use "cl" for substitute
 ("s"             . evil-surround-region)    ; package
 ("S"             . evil-Surround-region)    ; package
 ;; ----------------------------------------------------------------------------
 ;; Normal state keys!!!
 ;; ----------------------------------------------------------------------------
 :map
 evil-normal-state-map
 ;; I find myself primarely using isearch and swiper for searching
 ("s"             . evil-ex-search-forward)
 ("S"             . evil-ex-search-backward)
 ("g+"            . evil-numbers/inc-at-pt)  ; package
 ("g-"            . evil-numbers/dec-at-pt)  ; package
 ;; ----------------------------------------------------------------------------
 ;; Insert state keys!!!
 ;; ----------------------------------------------------------------------------
 ;; I come from Emacs so I like to access some navigation in insert state.
 :map
 evil-insert-state-map
 ("C-g"           . evil-normal-state)
 ("C-B"           . evil-backward-WORD-begin)
 ("C-b"           . evil-backward-word-begin)
 ("C-d"           . backward-kill-word) ; Usually "C-w" but I use that for forward word
 ("C-e"           . forward-word)
 ("C-p"           . yank)
 ("M-p"           . consult-yank-pop)
 ("C-u"           . universal-argument)
 ("C-W"           . evil-forward-WORD-begin)
 ("C-w"           . evil-forward-word-begin)
 ("C-v"           . set-mark-command) ; "C-SPC" is used by general
 ("C-Æ"           . evil-backward-paragraph)
 ("C-æ"           . evil-forward-paragraph)
 ("C-Ø"           . evil-first-non-blank)
 ("C-ø"           . move-end-of-line)
 ("C-0"           . evil-beginning-of-line)
 :map
 org-present-mode-keymap
 ("<left>"        . org-present-prev)
 ("<right>"       . org-present-next)
 ("<up>"          . org-present-beginning)
 ("<down>"        . org-present-end)
 :map
 ccm-map
 ("<prior>"       . evil-scroll-up)
 ("<next>"        . evil-scroll-down)
 :map
 org-mode-map
 ("C-Æ"           . org-previous-visible-heading)
 ("C-æ"           . org-next-visible-heading)
 ;; :map
 ;; view-mode-map
 ;; ("SPC"           . View-quit)
 :map
 minibuffer-local-map
 ("C-."           . embark-act)
 ("M-."           . embark-dwim)
 ("C-æ"           . marginalia-cycle))
(evil-define-key
  'emacs ibuffer-mode-map
  (kbd "´")         'evil-window-next
  (kbd "¨")         'evil-next-buffer
  (kbd "Å")         'my/org-capture-idea
  (kbd "å")         'my/org-agenda-custom)
(evil-define-key
  'normal dired-mode-map
  (kbd "SPC")       'quit-window ; C-SPC for the general package
  (kbd "a")         'dired-omit-mode
  (kbd "h")         'dired-up-directory
  (kbd "l")         'dired-find-file)
(evil-define-key
  'normal org-mode-map
  (kbd "t")         'org-todo
  (kbd "T")         'org-todo-yesterday)
(evil-define-key
  'motion org-agenda-mode-map
  (kbd "SPC")       'org-agenda-quit ; C-SPC for the general package
  (kbd "´")         'evil-window-next
  (kbd "¨")         'evil-next-buffer
  (kbd "½")         'tab-new
  (kbd "C-½")       'tab-bar-mode
  (kbd "C-<tab>")   'tab-next
  (kbd "C-S-<tab>") 'tab-previous
  (kbd "a")         'org-agenda-append-agenda
  (kbd "A")         'org-agenda-archive-default-with-confirmation
  (kbd "R")         'org-agenda-refile
  (kbd "T")         'org-agenda-todo-yesterday
  (kbd "sd")        'org-agenda-deadline
  (kbd "ss")        'org-agenda-schedule
  (kbd "gf")        'org-agenda-follow-mode
  (kbd "gy")        'org-agenda-year-view
  (kbd "S-<left>")  'org-agenda-earlier
  (kbd "S-<right>") 'org-agenda-later
  (kbd "n")         'org-agenda-add-note
  (kbd "Æ")         'org-agenda-backward-block
  (kbd "æ")         'org-agenda-forward-block
  (kbd "å")         'org-agenda-columns)

;; ============================================================================
;;; Package faces
;; ============================================================================
;; Org
;; ----------------------------------------------------------------------------
(set-face-attribute
 'org-document-title
 nil :height 1.25 :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-1
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-2
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-3
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-4
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-5
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-6
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-7
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-level-8
 nil :height 1.0  :foreground "#f90" :weight 'bold)
(set-face-attribute
 'org-todo
 nil :height 0.8  :foreground "#f09")
(set-face-attribute
 'org-done
 nil :height 0.8  :foreground "#9f0")
(set-face-attribute
 'org-headline-done
 nil              :foreground "#0f9")
(set-face-attribute
 'org-ellipsis
 nil :height 0.8  :foreground "#0f9" :weight 'normal :underline nil)
(set-face-attribute
 'org-document-info-keyword
 nil :height 0.8  :foreground "#0f9" :weight 'normal)
(set-face-attribute
 'org-special-keyword
 nil :height 0.8  :foreground "#0f9" :weight 'normal)
(set-face-attribute
 'org-checkbox
 nil :height 0.8  :foreground "#0f9" :background "#221" :box nil)
(set-face-attribute
 'org-tag
 nil              :foreground "#0f9" :weight 'normal)
(set-face-attribute
 'org-formula
 nil :height 0.8  :foreground "#0f9")
(set-face-attribute
 'org-code
 nil :height 0.8  :foreground "#0f9")
(set-face-attribute
 'org-verbatim
 nil :height 0.8  :foreground "#0f9")
(set-face-attribute
 'org-table
 nil :height 0.8  :foreground "#0f9")
(set-face-attribute
 'org-block
 nil              :foreground "#bba")
(set-face-attribute
 'org-block-begin-line
 nil :height 0.8  :foreground "#09f")
(set-face-attribute
 'org-block-end-line
 nil              :foreground "#09f")
(set-face-attribute
 'org-drawer
 nil :height 0.8  :foreground "#09f")
(set-face-attribute
 'org-footnote
 nil :height 0.8  :foreground "#09f" :underline nil)
(set-face-attribute
 'org-date
 nil :height 0.8  :foreground "#09f" :underline nil)
(set-face-attribute
 'org-link
 nil              :foreground "#09f")
(set-face-attribute
 'org-meta-line
 nil :height 0.8)
;; ----------------------------------------------------------------------------
;; Agenda
;; ----------------------------------------------------------------------------
(set-face-attribute
 'header-line
 nil :height 1.25 :foreground "#f90" :background "#221" :weight 'bold)
(set-face-attribute
 'org-agenda-structure
 nil :height 1.0  :foreground "#f90" :background "#221" :box nil :weight 'bold)
(set-face-attribute
 'org-column
 nil                                 :background "#221")
(set-face-attribute
 'org-warning
 nil              :foreground "#f09")
(set-face-attribute
 'org-agenda-done
 nil              :foreground "#0f9" :slant 'normal)
(set-face-attribute
 'org-time-grid
 nil              :foreground "#0f9")
(set-face-attribute
 'calendar-weekday-header
 nil              :foreground "#0f9")
(set-face-attribute
 'org-agenda-calendar-event
 nil              :foreground "#bba")
(set-face-attribute
 'org-agenda-date
 nil              :foreground "#09f" :background "#221" :box nil :weight 'normal)
(set-face-attribute
 'org-agenda-date-weekend
 nil              :foreground "#09f" :background "#221" :box nil :weight 'normal :underline nil)
(set-face-attribute
 'org-agenda-date-today
 nil              :foreground "#f90" :background "#221" :box nil :weight 'normal :slant 'normal :inverse-video nil)
(set-face-attribute
 'org-upcoming-distant-deadline
 nil              :foreground "#9f0")
(set-face-attribute
 'org-upcoming-deadline
 nil              :foreground "#9f0")
(set-face-attribute
 'org-imminent-deadline
 nil              :foreground "#f90" :weight 'normal)
(set-face-attribute
 'org-scheduled
 nil              :foreground "#9f0")
(set-face-attribute
 'org-scheduled-today
 nil              :foreground "#9f0")
(set-face-attribute
 'org-scheduled-previously
 nil              :foreground "#f90")
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(set-face-attribute
 'org-superstar-leading
 nil :height 0.8  :foreground "#332")  ; the dots marking the deapt
(set-face-attribute
 'org-superstar-item
 nil :height 0.8  :foreground "#f90") ; the bullet face
;; ----------------------------------------------------------------------------
;; Habit
;; ----------------------------------------------------------------------------
(add-hook ; needed for faces to be recognized
 'org-appear-mode-hook
 (lambda ()
   (set-face-attribute
    'org-habit-alert-face
    nil :height 0.8  :foreground "#f09" :background "#f90" :weight 'bold)
   (set-face-attribute
    'org-habit-alert-future-face
    nil :height 0.8                     :background "#f90")
   (set-face-attribute
    'org-habit-overdue-face
    nil :height 0.8  :foreground "#f90" :background "#f09" :weight 'bold)
   (set-face-attribute
    'org-habit-overdue-future-face
    nil :height 0.8                     :background "#332")
   (set-face-attribute
    'org-habit-ready-face
    nil :height 0.8  :foreground "#f90" :background "#9f0" :weight 'bold)
   (set-face-attribute
    'org-habit-ready-future-face
    nil :height 0.8                     :background "#9f0")
   (set-face-attribute
    'org-habit-clear-face
    nil :height 0.8  :foreground "#f90" :background "#332" :weight 'bold)
   (set-face-attribute
    'org-habit-clear-future-face
    nil :height 0.8                     :background "#332")))
;; ----------------------------------------------------------------------------
;; Misc. other faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'rainbow-delimiters-base-error-face
 nil              :foreground "#f09" :background "#332" :weight 'bold)
(set-face-attribute
 'show-paren-match
 nil              :foreground "#f90" :background "#332" :weight 'bold)
(set-face-attribute
 'rainbow-delimiters-depth-1-face
 nil              :foreground "#9f0")
(set-face-attribute
 'rainbow-delimiters-depth-2-face
 nil              :foreground "#09f")
(set-face-attribute
 'rainbow-delimiters-depth-3-face
 nil              :foreground "#0f9")
(set-face-attribute
 'dired-ignored
 nil              :foreground "#0f9")
(set-face-attribute
 'keycast-key
 nil :height 0.8  :foreground "#221" :background "#332" :box nil)
(set-face-attribute
 'indent-guide-face
 nil :foreground "#332")
(set-face-attribute
 'aw-leading-char-face
 nil :height 1.0  :foreground "#f90")
(set-face-attribute
 'ivy-current-match ; counsel use this face
 nil              :foreground "#9f0" :background "#332")

;; ============================================================================
;;; Additional hooks
;; ============================================================================
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ; toggle with "("
(add-hook 'dired-mode-hook #'dired-omit-mode)         ; toggle with "a"
(add-hook 'text-mode-hook  #'visual-line-mode)
(add-hook 'which-key-init-buffer-hook  #'org-save-all-org-buffers)
(add-hook
 'before-save-hook
 (lambda ()
   (whitespace-cleanup)
   (when (string-equal major-mode "org-mode")
     (org-update-statistics-cookies t))))
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs ready in %s with %d garbage collections."
    (format
     "%.1f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))
;; ----------------------------------------------------------------------------
;; Add my book to the agenda to use todos as bookmarks
;; ----------------------------------------------------------------------------
(my/org-agenda-custom)
(when (file-exists-p (concat org-directory "bgbog/bg.org"))
  (setq
   org-agenda-files
   (cons (concat org-directory "bgbog/bg.org") org-agenda-files)))
;; (byte-recompile-directory package-user-dir nil 'force)
