;; -*- lexical-binding: t; -*-
;; #+title: Emacs config init.el

;; ============================================================================
;;; Font faces (my themes)
;; ============================================================================
;; I like everything compiled
(when (file-newer-than-file-p
       (locate-user-emacs-file "my-ansi-faces.el")
       (locate-user-emacs-file "my-ansi-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "my-ansi-faces.el")))
(when (file-newer-than-file-p
       (locate-user-emacs-file "my-faces.el")
       (locate-user-emacs-file "my-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "my-faces.el")))
(if (eq (user-uid) 0) ; Different colors as super user
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)
  ;; else
  (load (locate-user-emacs-file "my-faces.elc") nil t))

;; ============================================================================
;;; Modeline
;; ============================================================================
(setq
 mode-line-buffer-identification-keymap nil)
(setq-default ; mode-line-format is buffer local so setq-default
 mode-line-format
 '(mode-line-front-space
   (:eval
    (if (eq (user-uid) 0)
        (list "#")
      ;; else
      (list " ")))
   mode-line-modified
   " "
   mode-line-buffer-identification
   " "
   (:eval
    (list
     ;; short major mode name
     (propertize
      (concat
       "["
       (string-replace
        "-" " "
        (replace-regexp-in-string
         "\\`emacs-" ""
         (replace-regexp-in-string
          "\\`org-" ""
          (replace-regexp-in-string
           "-buffer\\'" ""
           (replace-regexp-in-string
            "-mode\\'" ""
            (downcase (symbol-name major-mode)))))))
       "]")
      'help-echo (symbol-name major-mode)
      'local-map (make-mode-line-mouse-map
                  'mouse-1 #'transpose-frame)
      'mouse-face 'mode-line-highlight)
     (when (mode-line-window-selected-p)
       (list
        (unless (eq vc-mode nil) ; version control
          (replace-regexp-in-string
           "\\` Git" " "
           vc-mode))
        (when (buffer-narrowed-p)
          (list
           " "
           (propertize
            "[n]"
            'help-echo "Narrowed, mouse-1: widen"
            'local-map (make-mode-line-mouse-map
                        'mouse-1 #'mode-line-widen)
            'mouse-face 'mode-line-highlight)))
        mode-line-misc-info))
     " "))
   (:eval
    (list
     ;; gap for alignment
     (if (mode-line-window-selected-p)
         (propertize
          " "
          'display '((space :align-to (- (+ right right-fringe right-margin) 7)))
          'face    'mode-line-inactive)
       ;; else
       (propertize
        " "
        'display '((space :align-to (- (+ right right-fringe right-margin) 3)))))
     ;; position information
     (if (mode-line-window-selected-p)
         (if global-display-line-numbers-mode
             '(-7 "%3c %p")
           ;; else
           "%4l,%2c")
       ;; else
       '(-3 "%p"))))))

;; ============================================================================
;;; Other vanilla stuff
;; ============================================================================
;; Variables
;; ----------------------------------------------------------------------------
(setq-default ; buffer-local variables
 indent-tabs-mode nil
 display-line-numbers-width 3)
(setq
 tab-width 4
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      ;; else
      "%b")))
 warning-minimum-level :error
 visible-bell t
 use-dialog-box nil
 use-short-answers t
 initial-scratch-message nil
 large-file-warning-threshold nil
 custom-file "/dev/null" ; I don't deal with custom.el
 trash-directory "~/.local/share/Trash/files"
 delete-by-moving-to-trash t
 recentf-exclude
 '("\\`~/org/agenda/.*\\.org\\'"
   "\\`~/org/inbox\\.org\\'"
   "\\`~/\\.emacs\\.d/.*\\.el\\'"
   "\\`~/\\.emacs\\.d/bookmarks\\'"
   "\\`~/\\.emacs\\.d/diary\\'")
 shell-default-shell 'eshell ; I use a terminal outside emacs when needed
 eshell-ls-initial-args
 '("-agho")
 ;; ----------------------------------------------------------------------------
 ;; Dired
 ;; ----------------------------------------------------------------------------
 global-auto-revert-non-file-buffers t ; update dired buffer
 auto-revert-verbose nil
 dired-kill-when-opening-new-dired-buffer t
 dired-listing-switches "-agho --group-directories-first"
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-omit-verbose nil
 dired-omit-files "\\`[#\\.].*\\'" ; "a" to toggle
 ;; ----------------------------------------------------------------------------
 ;; Incremental search (I use Emacs tools for search)
 ;; ----------------------------------------------------------------------------
 search-whitespace-regexp ".*?"
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
 tab-bar-menu-bar-button       "☰"
 tab-bar-separator             " "
 tab-bar-format
 '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator)
 ;; ----------------------------------------------------------------------------
 ;; Display
 ;; ----------------------------------------------------------------------------
 display-time-format "[%Y-%m-%d %a %H:%M]"
 display-line-numbers-type 'relative
 ;; Popups not associated with a file pop below the current window.
 display-buffer-alist
 '(("\\`\s?\\*.*\\*\s?\\'"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (body-function . select-window))))
;; ----------------------------------------------------------------------------
;; Global minor modes
;; ----------------------------------------------------------------------------
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
;; ----------------------------------------------------------------------------
;; Hooks
;; ----------------------------------------------------------------------------
(add-hook 'dired-mode-hook  #'dired-hide-details-mode) ; toggle with "("
(add-hook 'dired-mode-hook  #'dired-omit-mode)         ; toggle with "a"
(add-hook 'text-mode-hook   #'visual-line-mode)
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs ready in %s with %d garbage collections."
    (format
     "%.1f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))

;; ============================================================================
;;; Custom functions
;; ============================================================================
;; Open init files
;; ----------------------------------------------------------------------------
(defun my/open-init-file ()
  "Open config file init.el"
  (interactive)
  (find-file (locate-user-emacs-file "early-init.el"))
  (find-file (locate-user-emacs-file "init.el")))
;; ----------------------------------------------------------------------------
;; Open agenda and plan file
;; ----------------------------------------------------------------------------
(defun my/open-agenda-file ()
  "Open my agenda file agenda.org"
  (interactive)
  (find-file (concat org-agenda-directory "plan.org"))
  (find-file (concat org-agenda-directory "agenda.org")))
;; ----------------------------------------------------------------------------
;; Open note and date file
;; ----------------------------------------------------------------------------
(defun my/open-note-file ()
  "Open my notes file note.org"
  (interactive)
  (find-file (concat org-agenda-directory "date.org"))
  (find-file (concat org-agenda-directory "note.org")))
;; ----------------------------------------------------------------------------
;; Custom agenda
;; ----------------------------------------------------------------------------
(defun my/org-agenda-custom ()
  "Custom agenda with NEXT, agenda and TODO/HOLD"
  (interactive)
  (org-agenda nil "c")
  (delete-other-windows)
  ;; If the point start on the agenda heading move to today's date
  (unless (eq (char-after) ?\s)
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
  (split-window-right)
  (other-window 1)
  (mode-line-other-buffer) ; switch to the most recent buffer
  (split-window-below)
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window -2)) ; back to initial window
;; ----------------------------------------------------------------------------
;; Magit stage and commit
;; ----------------------------------------------------------------------------
(defun my/magit-stage-all-and-commit (message)
  (interactive "sCommit Message: ")
  (save-some-buffers t)
  (shell-command
   (format "git commit -a -m \"%s\" &" message)))
;; ----------------------------------------------------------------------------
;; Save and quit
;; ----------------------------------------------------------------------------
(defun my/save-all-kill-emacs-no-prompt ()
  "Save all and quit without prompting" ; Not risky with backup-each-save.
  (interactive)
  (save-some-buffers t)
  (when (file-newer-than-file-p
         (locate-user-emacs-file "early-init.el")
         (locate-user-emacs-file "early-init.elc"))
    (byte-compile-file (locate-user-emacs-file "early-init.el")))
  (when (file-newer-than-file-p
         (locate-user-emacs-file "init.el")
         (locate-user-emacs-file "init.elc"))
    (byte-compile-file (locate-user-emacs-file "init.el")))
  (kill-emacs))
;; ----------------------------------------------------------------------------
;; My faces (theme)
;; ----------------------------------------------------------------------------
(defun my/toggle-faces ()
  "My default faces"
  (interactive)
  (if (equal (face-background 'default) "#000")
      (load (locate-user-emacs-file "my-faces.elc") nil t)
    ;; else
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)))

;; ============================================================================
;;; Package.el
;; ============================================================================
(setq
 load-prefer-newer t ; use .el if newer than .elc
 package-archives
 '(("elpa"  . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/"))
 package-selected-packages
 '(evil evil-collection evil-nerd-commenter evil-surround evil-numbers evil-org
        vertico marginalia orderless consult embark embark-consult lsp-mode
        avy org-superstar org-appear org-present cape corfu nerd-icons-corfu
        nerd-icons nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion
        rainbow-delimiters rainbow-mode golden-ratio ace-window transpose-frame
        recursive-narrow centered-cursor-mode visual-fill-column mixed-pitch
        undo-tree keycast indent-guide counsel helpful flycheck writegood-mode
        magit which-key general super-save backup-each-save auto-package-update))
(unless (package-installed-p 'package)
  (require 'package))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)
;; ----------------------------------------------------------------------------
;; Update, undo, save and backup
;; ----------------------------------------------------------------------------
(setq
 auto-package-update-interval 7
 auto-package-update-hide-results t
 auto-package-update-prompt-before-update t)
(require 'auto-package-update)
(auto-package-update-maybe)
(setq
 undo-tree-visualizer-diff t
 undo-tree-visualizer-timestamps t
 undo-tree-auto-save-history t)
(require 'undo-tree)
(global-undo-tree-mode 1)
(require 'saveplace)
(save-place-mode 1)
(require 'savehist)
(savehist-mode 1)
(require 'super-save)
(super-save-mode 1)
(setq
 backup-each-save-mirror-location "~/.backup-emacs-saved")
(require 'backup-each-save)
(add-hook 'after-save-hook #'backup-each-save)

;; ============================================================================
;;; Evil.el
;; ============================================================================
;; Cursors are special. I combine f and 0 to color all types.
;; I tax the eyes a bit to follow the cursor/point and the evil states.
;; |------+----------+------+---------|
;; | #000 | region   | #fff | visual  |
;; |------+----------+------+---------|
;; | #f00 | operator | #0ff | emacs   |
;; | #0f0 | normal   | #f0f | replace |
;; | #00f | motion   | #ff0 | insert  |
;; |------+----------+------+---------|
;; Operator state is red to alert. Normal state has the opposite green color.
;; Insert state has the remaining yellow trafic light color and use a bar.
;; Replace state has a magenta color and use a horizontal hbar cursor.
;; Emacs and motion states have the remaining cyan and blue extreme rgb colors.
;; "Input" states have the colors with 2 f's and bars in common.
;; The visual state has a white hollow cursor and regions are black.
;; ----------------------------------------------------------------------------
(set-face-attribute
 'region
 nil :background "#000")
(setq
 evil-operator-state-cursor '(box        "#f00")
 evil-normal-state-cursor   '(box        "#0f0")
 evil-motion-state-cursor   '(box        "#00f")
 evil-emacs-state-cursor    '((bar  . 4) "#0ff")
 evil-replace-state-cursor  '((hbar . 4) "#f0f")
 evil-insert-state-cursor   '((bar  . 4) "#ff0")
 evil-visual-state-cursor   '(hollow     "#fff")
 evil-ex-substitute-highlight-all nil
 evil-ex-search-persistent-highlight nil
 evil-shift-round t
 evil-undo-system 'undo-tree
 evil-want-integration t
 evil-want-keybinding nil ; evil-collection need this
 evil-want-C-u-scroll t   ; in the motion states
 vim-style-remap-Y-to-y$ t)
;; ----------------------------------------------------------------------------
;; Evil package
;; ----------------------------------------------------------------------------
;; I dislike using my <ctrl> key and prefer modal to layered keybindings.
(require 'evil)
(evil-mode 1)
;; ----------------------------------------------------------------------------
;; Cursor not `on-characters' in normal state (my sacrilege)
;; ----------------------------------------------------------------------------
;; Normal and insert state share the cursor `between-characters' model.
;; I try to minimize the use of layers. <shift> just like <ctrl> is a layer.
;; I swap "p"/"P" and "o"/"O" compared to Vim to avoid layered bindings.
;; I almost only use "p" to paste and "i", "c" or "o" to enter insert state.
;; I never use "a" or "s" and rarely use capitalized bindings as Vim `verbs'.
;; ----------------------------------------------------------------------------
;; This model is easier to internalize especially if you are not a power user.
;; It's more similar to other editing experiences.
;; More info: [[https://www.dr-qubit.org/Evil_cursor_model.html]]
;; Normal `evil' behavior will work if the next few lines are omitted.
(when (file-newer-than-file-p
       (locate-user-emacs-file "evil-cursor-model.el")
       (locate-user-emacs-file "evil-cursor-model.elc"))
  (byte-compile-file (locate-user-emacs-file "evil-cursor-model.el")))
(load (locate-user-emacs-file "evil-cursor-model.elc") nil t)
;; ----------------------------------------------------------------------------
;; Additional evil packages
;; ----------------------------------------------------------------------------
(require 'evil-collection)
(evil-collection-init)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-nerd-commenter)
(require 'evil-numbers)
(with-eval-after-load 'org
  (require 'evil-org)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (add-hook
   'org-mode-hook
   (lambda ()
     evil-org-mode))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; ============================================================================
;;; Org.el
;; ============================================================================
;; Create org agenda directories and files unless they exist
;; ----------------------------------------------------------------------------
(setq
 org-directory "~/org/")
(defvar
  org-agenda-directory (concat org-directory "agenda/")
  "Default org-agenda directory")
(unless (file-exists-p org-directory)
  (make-directory org-directory))
(unless (file-exists-p org-agenda-directory)
  (make-directory org-agenda-directory))
;; Need at least one agenda file. Can't just be a directory
(unless (file-exists-p (concat org-directory "inbox.org"))
  (make-empty-file     (concat org-directory "inbox.org")))
;; Create agenda.org with two refile targets
(unless (file-exists-p (concat org-agenda-directory "agenda.org"))
  (find-file           (concat org-agenda-directory "agenda.org"))
  (insert
   (concat
    "#+title: Main agenda file\n"
    "#+startup: content\n"
    "* Event [/]\n"
    ":PROPERTIES:\n"
    ":CATEGORY: Event\n"
    ":END:\n"
    "* Task [/]\n"
    ":PROPERTIES:\n"
    ":CATEGORY: Task\n"
    ":END:\n"))
  (save-buffer)
  (switch-to-buffer "*scratch*"))
;; Create additional empty capture files
(unless (file-exists-p (concat org-agenda-directory "plan.org"))
  (make-empty-file     (concat org-agenda-directory "plan.org")))
(unless (file-exists-p (concat org-agenda-directory "note.org"))
  (make-empty-file     (concat org-agenda-directory "note.org")))
(unless (file-exists-p (concat org-directory "archive.org"))
  (make-empty-file     (concat org-directory "archive.org")))
;; ----------------------------------------------------------------------------
;; Variables setting up the environment
;; ----------------------------------------------------------------------------
(setq
 org-default-notes-file (concat org-directory "inbox.org")
 org-ellipsis " … "
 ;; ----------------------------------------------------------------------------
 ;; 4 todo states: I use category and refile rather than more keywords
 ;; ----------------------------------------------------------------------------
 org-todo-keywords
 '((type     "NEXT(n!/!)" "TODO(t!/!)" "|")
   (type "|" "HOLD(h@/!)" "DONE(d!/!)"))
 org-priority-default ?C
 org-priority-faces ; This affects rendering in agenda
 '((?A . (:slant nil :height 0.8))
   (?B . (:slant nil :height 0.8))
   (?C . (:slant nil :height 0.8)))
 org-list-allow-alphabetical t
 org-list-demote-modify-bullet
 '(("+" . "*")
   ("*" . "-")
   ("-" . "+"))
 org-tags-column -75 ; minus aligns right
 org-tag-alist
 '(("bg"       . ?b)
   ("pc"       . ?c)
   ("emacs"    . ?e)
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
 org-startup-folded 'show2levels
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
 `(("c" "Category" entry ; backtick to "concat" primarily for code readability
    (file ,(concat org-agenda-directory "plan.org"))
    ,(concat
      "* TODO %^{Heading} [/]\n"
      ":PROPERTIES:\n"
      ":ARCHIVE: " org-directory "archive.org::* Log\n"
      ":CATEGORY: %^{Category}\n"
      ":END:\n"
      ":LOGBOOK:\n"
      "- State \"TODO\" from \"Capture\" %U\n"
      ":END:\n")
    :immediate-finish t :prepend t)
   ("i" "Idea" entry
    (file ,(concat org-directory "inbox.org"))
    ,(concat
      "* NEXT %^{Idea}\n"
      ":LOGBOOK:\n"
      "- State \"NEXT\" from \"Capture\" %U\n"
      ":END:\n"
      "%i")
    :immediate-finish t :prepend t)
   ("e" "Event" entry
    (file+olp ,(concat org-agenda-directory "agenda.org") "Event")
    ,(concat
      "* TODO %^{Meet who?} %^G\n"
      "SCHEDULED: %^{When?}t\n"
      ":LOGBOOK:\n"
      "- State \"TODO\" from \"Capture\" %U\n"
      ":END:\n"
      "%?%i"))
   ("t" "Task" entry
    (file+olp ,(concat org-agenda-directory "agenda.org") "Task")
    ,(concat
      "* TODO %^{Do what?} %^G\n"
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
   ("x" "Clipboard" entry
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
 ;; System to organize tasks and suppress out of date information.
 ;; It's not primarily a calendar. It's all about task management.
 org-agenda-window-setup 'current-window
 org-archive-location (concat org-directory "archive.org::* Archive")
 org-refile-targets
 `((,(concat org-agenda-directory "agenda.org") :maxlevel . 1)
   (,(concat org-agenda-directory "plan.org")   :maxlevel . 1))
 org-agenda-files (list (concat org-directory "inbox.org")
                        org-agenda-directory) ; all org files in the agenda dir
 org-agenda-format-date " [%F %a] "
 org-agenda-block-separator ?﹋ ; ﹌̅‾﹉⎺ also separate appended agenda ("a")
 org-agenda-span 'month
 org-agenda-custom-commands
 '(("c" "Custom agenda setup"
    ((todo "NEXT"
           ;; max 5 min NEXT unblocked tasks above agenda
           ((org-agenda-overriding-header "")))
     (agenda ""
             ((org-agenda-span 'week)))
     (todo "TODO"
           ;; TODOs with [/] cookies (subtasks).
           ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'notregexp "\\[[0-9]+/[0-9]+\\]"
               ;; or
               'scheduled)))) ; will be shown with a deadline.
     (todo "TODO"
           ;; Not every TODO has or should have a timestamp.
           ((org-agenda-overriding-header "") ; share heading with the item above
            (org-agenda-block-separator nil)  ; don't separate
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'regexp "\\[[0-9]+/[0-9]+\\]"
               ;; or
               'timestamp))))
     ;; Inactive states
     (todo "HOLD"
           ;; HOLD for third party action pending.
           ((org-agenda-overriding-header "")
            (org-agenda-block-separator nil)))
     (todo "DONE"
           ;; Prioritized DONE.
           ((org-agenda-overriding-header "")
            (org-agenda-block-separator nil)
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'notregexp org-priority-regexp))))))
   ("d" "DONE entries"
    ;; Useful for archiving done tasks
    ((todo "DONE"
           ((org-agenda-overriding-header "Done TODOs")))))
   ("u" "Untagged TODOs"
    ;; Useful for adding tags
    ((tags-todo "-{.*}"
                ((org-agenda-overriding-header "Untagged TODOs"))))))
 org-agenda-time-grid nil
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
 org-agenda-timerange-leaders
 '(":1" "%2d/%d") ; :# is a marker to discover when this is used
 org-agenda-entry-text-leaders ":2"
 org-agenda-bulk-mark-char ":3"
 org-agenda-breadcrumbs-separator ":4"
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
 org-time-stamp-rounding-minutes '(15 15)
 ;; ----------------------------------------------------------------------------
 ;; Diary
 ;; ----------------------------------------------------------------------------
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
   (holiday-fixed  4  1    "Aprilsnar")
   (holiday-fixed  5  1    "Arbejdernes kamp dag")
   (holiday-fixed  6  5    "Grundlovs og fars dag")
   (holiday-fixed  6 23    "Sanct Hans")
   (holiday-fixed 12 24    "Juleaften"))
 org-agenda-include-diary t
 diary-mark-entries t
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
(unless (package-installed-p 'org)
  (require 'org))
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
      ("SCHEDULED:"     . ?☐) ; That's OK because the inactive timestamp "CLOSED:"
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
;; Update "[/]"
;; ----------------------------------------------------------------------------
(add-hook
 'org-after-todo-state-change-hook
 (lambda () ; Overkill to update cookies with state change from inside agenda
   (mapcar
    (lambda (buffer)
      (with-current-buffer buffer (org-update-statistics-cookies t)))
    (org-buffer-list))))
;; ----------------------------------------------------------------------------
;; Insert state in org-capture and on org-add-note/org-refile
;; ----------------------------------------------------------------------------
(with-eval-after-load 'evil
  (add-hook 'org-capture-mode-hook     #'evil-insert-state)
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state))
;; ----------------------------------------------------------------------------
;; Bullets
;; ----------------------------------------------------------------------------
(setq
 org-superstar-headline-bullets-list
 '(?① ?② ?③ ?④ ?ⓧ)
 org-superstar-cycle-headline-bullets nil)
(require 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)
;; ----------------------------------------------------------------------------
;; Habits
;; ----------------------------------------------------------------------------
(add-to-list 'org-modules 'org-habit)
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
;;; Completion
;; ============================================================================
;; Minibuffer
;; ----------------------------------------------------------------------------
(setq
 vertico-resize nil)
(require 'vertico)
(vertico-mode 1)
;; tidy typing directories
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(with-eval-after-load 'vertico
  (require 'marginalia)
  (marginalia-mode 1))
(setq
 completion-styles
 '(orderless)) ; other options: (basic substring flex)
(require 'orderless) ; Fuzzy completions
(require 'consult)   ; Combine functionality (e.g. buffers + recentf)
(require 'embark)    ; not something I use yet
;; ----------------------------------------------------------------------------
;; Buffer
;; ----------------------------------------------------------------------------
(require 'lsp-mode)
(setq
 corfu-auto t
 corfu-auto-delay 0.1
 corfu-auto-prefix 3
 corfu-count 5
 corfu-quit-at-boundary 'separator)
(require 'corfu)
(global-corfu-mode 1)
(corfu-echo-mode 1)
(corfu-history-mode 1)

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
;; Writing tips
;; ----------------------------------------------------------------------------
(require 'flycheck)
(add-hook 'org-mode-hook #'flyspell-mode)
(require 'writegood-mode)

;; ============================================================================
;;; Thumbnails and colors
;; ============================================================================
(when (display-graphic-p)
  (require 'nerd-icons) ; all-the-icons' alignment sucks. Nerd works
  (require 'nerd-icons-dired)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (require 'nerd-icons-ibuffer)
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  (require 'nerd-icons-corfu)
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (with-eval-after-load 'marginalia
    (require 'nerd-icons-completion)
    (nerd-icons-completion-mode 1)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))
;; ----------------------------------------------------------------------------
;; Color color-codes and delimiters
;; ----------------------------------------------------------------------------
(setq
 rainbow-delimiters-max-face-count 3)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(require 'indent-guide)
(require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)

;; ============================================================================
;;; Misc. packages
;; ============================================================================
(setq
 avy-timeout-seconds 1) ; I'm slow
(require 'avy)
(require 'centered-cursor-mode)
(global-centered-cursor-mode 1)
(require 'keycast)
(keycast-tab-bar-mode 1)
(add-hook 'prog-mode-hook #'indent-guide-mode)
(setq-default
 fill-column 90)
(setq
 visual-fill-column-center-text t)
(require 'visual-fill-column)
;; ----------------------------------------------------------------------------
;; Font pitch
;; ----------------------------------------------------------------------------
(setq
 mixed-pitch-set-height t)
(require 'mixed-pitch)
(dolist
    (face
     '(org-special-keyword org-date org-tag org-priority org-todo org-table))
  (add-to-list 'mixed-pitch-fixed-pitch-faces face))
;; ----------------------------------------------------------------------------
;; Window manipulation
;; ----------------------------------------------------------------------------
(require 'ace-window)
(ace-window-display-mode 1)
(require 'transpose-frame)
(require 'golden-ratio)
(golden-ratio-mode 1)
;; ----------------------------------------------------------------------------
;; Narrow with dwim
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org
  (require 'recursive-narrow))
;; ----------------------------------------------------------------------------
;; Magit
;; ----------------------------------------------------------------------------
(require 'magit)
(with-eval-after-load 'evil
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert))

;; ============================================================================
;;; General.el
;; ============================================================================
(setq
 which-key-idle-delay 0)
(which-key-mode 1)
(require 'general)
(general-evil-setup t)
(general-auto-unbind-keys t)
(general-create-definer my/set-leader-keys
  :keymaps '(motion insert emacs)
  :prefix "SPC"
  :global-prefix "C-SPC") ; Visual state ("v") sets the mark
(my/set-leader-keys
  ""    nil
  "SPC" '(counsel-M-x                            :which-key "M-x")
  ;; toggle back and forth between 2 buffers:
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
  "´"   '(evil-window-prev                       :which-key "Prev win")
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
  "bm"  '(view-echo-area-messages                :which-key "Messages")
  "bs"  '(scratch-buffer                         :which-key "Scratch")
  "c"   '(:ignore t                              :which-key "c")
  "d"   '(:ignore t                              :which-key "d")
  "e"   '(:ignore t                              :which-key "Eval")
  "ee"  '(eval-expression                        :which-key "Expression")
  "es"  '(eval-last-sexp                         :which-key "Sexp @point")
  "f"   '(:ignore t                              :which-key "Files")
  "fS"  '(save-some-buffers                      :which-key "Save all")
  "fR"  '(consult-recent-file                    :which-key "Mini recent")
  "fa"  '(my/open-agenda-file                    :which-key "Agenda")
  "fd"  '(dired-jump                             :which-key "Dired")
  "ff"  '(counsel-find-file                      :which-key "Find")
  "fi"  '(my/open-init-file                      :which-key "Init")
  "fn"  '(my/open-note-file                      :which-key "Notes")
  "fp"  '(find-file-at-point                     :which-key "At point")
  "fr"  '(recentf-open-files                     :which-key "Recent")
  "fs"  '(basic-save-buffer                      :which-key "Save")
  "fw"  '(write-file                             :which-key "Save as")
  "g"   '(:ignore t                              :which-key "Git")
  "gb"  '(magit-blame                            :which-key "Blame")
  "gc"  '(my/magit-stage-all-and-commit          :which-key "Commit")
  "gg"  '(magit                                  :which-key "Magit")
  "gs"  '(magit-status                           :which-key "Status")
  "h"   '(:ignore t                              :which-key "Help")
  "hC"  '(helpful-command                        :which-key "Command")
  "hF"  '(counsel-describe-face                  :which-key "Face")
  "hK"  '(counsel-descbinds                      :which-key "Bindings")
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
  "oF"  '(org-agenda-file-to-front               :which-key "Agenda file")
  "oG"  '(org-goto                               :which-key "Goto")
  "oI"  '(org-clock-in                           :which-key "Clock in")
  "oL"  '(org-store-link                         :which-key "Store link")
  "oO"  '(org-clock-out                          :which-key "Clock out")
  "oP"  '(org-present                            :which-key "Present")
  "oR"  '(org-refile                             :which-key "Refile")
  "oS"  '(org-sort                               :which-key "Sort")
  "oT"  '(orgtbl-mode                            :which-key "Tables")
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
  "or"  '(:ignore t                              :which-key "Org roam")
  "p"   '(consult-yank-pop                       :which-key "Paste pop")
  "q"   '(:ignore t                              :which-key "Quit")
  "qq"  '(my/save-all-kill-emacs-no-prompt       :which-key "Save & kill")
  "qs"  '(save-buffers-kill-emacs                :which-key "Prompt&save")
  "r"   '(:ignore t                              :which-key "Registers")
  "rl"  '(consult-register-load                  :which-key "Load")
  "rr"  '(consult-register-store                 :which-key "Store")
  "s"   '(:ignore t                              :which-key "Search")
  "so"  '(consult-outline                        :which-key "Outline")
  "sO"  '(occur                                  :which-key "Occur")
  "sr"  '(query-replace                          :which-key "Replace")
  "sR"  '(query-replace-regexp                   :which-key "Rep. regex")
  "ss"  '(swiper                                 :which-key "Swiper") ; or consult-line
  "sw"  '(eww                                    :which-key "Web (eww)")
  "t"   '(:ignore t                              :which-key "Toggle")
  "tc"  '(visual-fill-column-mode                :which-key "Center col")
  "tf"  '(mixed-pitch-mode                       :which-key "Font pitch")
  "tg"  '(golden-ratio-mode                      :which-key "Gold ratio")
  "th"  '(hl-line-mode                           :which-key "Hl line")
  "tk"  '(keycast-tab-bar-mode                   :which-key "Keycast")
  "tl"  '(global-display-line-numbers-mode       :which-key "Line number")
  "tp"  '(prettify-symbols-mode                  :which-key "Prettify")
  "tr"  '(rainbow-mode                           :which-key "Rainbow")
  "ts"  '(flyspell-mode                          :which-key "Spell")
  "tt"  '(my/toggle-faces                        :which-key "Theme")
  "tw"  '(writegood-mode                         :which-key "Write good")
  "u"   '(universal-argument                     :which-key "Uni arg")
  "v"   '(exchange-point-and-mark                :which-key "Swap mark")
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
  "xC"  '(evil-upcase                            :which-key "Upcase")
  "xc"  '(evil-downcase                          :which-key "Downcase")
  "xs"  '(just-one-space                         :which-key "One space")
  "xu"  '(insert-char                            :which-key "Unicode")
  "xx"  '(transpose-chars                        :which-key "Swap chars")
  "y"   '(:ignore t                              :which-key "y")
  "z"   '(:ignore t                              :which-key "Zoom")
  "zl"  '(text-scale-adjust                      :which-key "Local")
  "zz"  '(global-text-scale-adjust               :which-key "Global")
  ;; Danish
  "æ"   '(:ignore t                              :which-key "æ")
  "ø"   '(:ignore t                              :which-key "ø")
  "å"   '(:ignore t                              :which-key "å"))

;; ============================================================================
;;; Keybindings
;; ============================================================================
(bind-key* "¤" 'evil-normal-state) ; I'm experimenting with a phone keyboard
(bind-keys
 ;; ----------------------------------------------------------------------------
 ;; Global keys!!! (minor mode maps will override)
 ;; ----------------------------------------------------------------------------
 ("<escape>"      . keyboard-escape-quit)
 ("<Scroll_Lock>" . centered-cursor-mode)
 ("<next>"        . evil-scroll-down)
 ("<prior>"       . evil-scroll-up)
 ("C-½"           . tab-line-mode)
 ("C-<tab>"       . tab-next)
 ("C-S-<tab>"     . tab-previous)
 ("C-a"           . org-cycle-agenda-files)
 ("M-p"           . consult-yank-pop) ; paste with kill-ring dialog
 ;; ----------------------------------------------------------------------------
 ;; Operator state keys!!!
 ;; ----------------------------------------------------------------------------
 :map evil-operator-state-map
 ;; Bad things happen if I hit "å" in operator state
 ("å"             . keyboard-escape-quit)
 ;; ----------------------------------------------------------------------------
 ;; Motion state keys!!! (normal, visual and motion)
 ;; ----------------------------------------------------------------------------
 :map evil-motion-state-map
 ("<down>"        . evil-next-visual-line)     ; up/down navigate wrapped lines
 ("<up>"          . evil-previous-visual-line) ; j/k respect the line atom in vim
 ("´"             . other-window)
 ("¨"             . next-buffer)
 ("½"             . tab-new)
 ("gc"            . evilnc-comment-operator)
 ;; danish keyboard
 ("Æ"             . evil-backward-paragraph)
 ("æ"             . evil-forward-paragraph)
 ("Ø"             . evil-first-non-blank)
 ("ø"             . evil-end-of-line)
 ("Å"             . my/org-capture-idea)
 ("å"             . my/org-agenda-custom)
 ("C-å"           . org-cycle-agenda-files)
 ;; ----------------------------------------------------------------------------
 ;; Normal state keys!!!
 ;; ----------------------------------------------------------------------------
 :map evil-normal-state-map
 ("g+"            . evil-numbers/inc-at-pt)
 ("g-"            . evil-numbers/dec-at-pt)
 ;; ----------------------------------------------------------------------------
 ;; Visual state keys!!!
 ;; ----------------------------------------------------------------------------
 :map evil-visual-state-map
 ("s"             . evil-surround-region)
 ("S"             . evil-Surround-region)
 ;; ----------------------------------------------------------------------------
 ;; Insert state keys!!!
 ;; ----------------------------------------------------------------------------
 ;; I come from Emacs so I like to access some navigation in insert state.
 :map evil-insert-state-map
 ("C-g"           . evil-normal-state)
 ("C-b"           . evil-backward-word-begin)
 ("C-B"           . evil-backward-WORD-begin)
 ("C-d"           . backward-kill-word) ; Usually "C-w" but I use that for forward word
 ("C-e"           . forward-word)
 ("C-p"           . yank)
 ("M-p"           . consult-yank-pop)
 ("C-u"           . universal-argument)
 ("C-w"           . evil-forward-word-begin)
 ("C-W"           . evil-forward-WORD-begin)
 ("C-v"           . set-mark-command) ; "C-SPC" is used by general.el
 ("C-æ"           . evil-forward-paragraph)
 ("C-Æ"           . evil-backward-paragraph)
 ("C-ø"           . move-end-of-line)
 ("C-Ø"           . evil-first-non-blank)
 ("C-0"           . evil-beginning-of-line)
 :map org-mode-map
 ("C-Æ"           . org-previous-visible-heading)
 ("C-æ"           . org-next-visible-heading)
 :map org-present-mode-keymap
 ("<left>"        . org-present-prev)
 ("<right>"       . org-present-next)
 ("<up>"          . org-present-beginning)
 ("<down>"        . org-present-end)
 :map corfu-map
 ("C-M-SPC"       . corfu-insert-separator) ; for orderless
 ("<tab>"         . corfu-next)
 ("S-<tab>"       . corfu-previous)
 ([tab]           . corfu-next)
 ([backtab]       . corfu-previous)
 :map ccm-map
 ("<prior>"       . nil)
 ("<next>"        . nil)
 :map minibuffer-local-map
 ("C-."           . embark-act)
 ("M-."           . embark-dwim)
 ("<next>"        . marginalia-cycle))
;; ----------------------------------------------------------------------------
(evil-define-key 'normal org-mode-map
  "t"         #'org-todo
  "T"         #'org-todo-yesterday)
(evil-define-key 'motion org-agenda-mode-map
  (kbd "SPC") nil ; make general.el take over "SPC"
  (kbd "S-<left>")  #'org-agenda-earlier
  (kbd "S-<right>") #'org-agenda-later
  "a"         #'org-agenda-append-agenda
  "A"         #'org-agenda-archive-default-with-confirmation
  "R"         #'org-agenda-refile
  "T"         #'org-agenda-todo-yesterday
  "gf"        #'org-agenda-follow-mode
  "gy"        #'org-agenda-year-view
  "sd"        #'org-agenda-deadline
  "ss"        #'org-agenda-schedule
  "n"         #'org-agenda-add-note
  "æ"         #'org-agenda-forward-block
  "Æ"         #'org-agenda-backward-block
  "å"         #'org-agenda-columns)
(evil-define-key 'emacs ibuffer-mode-map
  "´"         #'evil-window-next
  "¨"         #'evil-next-buffer
  "å"         #'my/org-agenda-custom
  "Å"         #'my/org-capture-idea)
(evil-define-key 'normal dired-mode-map
  (kbd "SPC") nil ; make general.el take over "SPC"
  "a"         #'dired-omit-mode
  "h"         #'dired-up-directory
  "l"         #'dired-find-file)
(evil-define-key 'normal help-mode-map
  (kbd "SPC") nil) ; make general.el take over "SPC"

;; ============================================================================
;;; Startup page
;; ============================================================================
(my/org-agenda-custom)
