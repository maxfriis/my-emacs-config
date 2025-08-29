;; -*- lexical-binding: t; -*-
;; #+title: Emacs config init.el

;; ============================================================================
;;; Fonts and faces (my themes)
;; ============================================================================
(cond
 ((find-font
   (font-spec :name "Ubuntu Mono"))
  (set-face-attribute
   'default
   nil :font "Ubuntu Mono-18"))
 ((find-font
   (font-spec :name "DejaVu Sans Mono"))
  (set-face-attribute
   'default
   nil :font "DejaVu Sans Mono"))) ; Fallback mono space font.
;; Though the Ubuntu font is nice it does not look good with Ubuntu Mono.
(when (find-font
       (font-spec :name "Verdana"))
  (set-face-attribute
   'variable-pitch
   nil :font "Verdana-20")) ; Best variable width font with Ubuntu Mono imo.
;; ----------------------------------------------------------------------------
;; I like the lisp files I load to be compiled.
;; ----------------------------------------------------------------------------
(when (file-newer-than-file-p
       (locate-user-emacs-file "my-ansi-faces.el")
       (locate-user-emacs-file "my-ansi-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "my-ansi-faces.el")))
(when (file-newer-than-file-p
       (locate-user-emacs-file "my-faces.el")
       (locate-user-emacs-file "my-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "my-faces.el")))
;; ----------------------------------------------------------------------------
;; I use files for faces rather than variables so I see the colors when I edit.
;; ----------------------------------------------------------------------------
(if (eq
     (user-uid) 0) ; Ansi colors as root.
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)
  ;; else
  (load (locate-user-emacs-file "my-faces.elc") nil t))

;; ============================================================================
;;; Mode line
;; ============================================================================
(setq-default ; mode-line-format is buffer local so setq-default.
 mode-line-format
 '((:eval ; Eval and list everything.
    (list
     "%e"
     " "
     ;; Indicators for writable and modified.
     mode-line-modified
     ;; Other custom indicator.
     (cond
      ((eq (user-uid) 0)
       (propertize
        "#"
        'help-echo "Root access"
        'face 'warning
        'mouse-face 'mode-line-highlight))
      ((buffer-narrowed-p)
       (propertize
        "n"
        'help-echo "Narrowed, mouse-1: Widen"
        'face 'warning
        'mouse-face 'mode-line-highlight
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'mode-line-widen)))
      (t " "))
     " "
     ;; Buffer name.
     mode-line-buffer-identification
     " "
     ;; Major mode name made shorter (message full name on mouse hover).
     (propertize
      (concat
       "["
       (string-replace
        "-" " "
        (replace-regexp-in-string ; Remove redundant information.
         "\\`emacs-" "e"
         (replace-regexp-in-string
          "\\`org-" ""
          (replace-regexp-in-string
           "-buffer\\'" ""
           (replace-regexp-in-string
            "-mode\\'" ""
            (downcase (symbol-name major-mode)))))))
       "]")
      'help-echo (concat (symbol-name major-mode) ", mouse-1: Cycle outline")
      'mouse-face 'mode-line-highlight
      'local-map (make-mode-line-mouse-map
                  'mouse-1 #'outline-cycle-buffer)) ; I use outline-minor-mode.
     ;; Version control in active window.
     (when (and
            vc-mode (mode-line-window-selected-p))
       (replace-regexp-in-string
        "\\` Git" " "
        vc-mode))
     " "
     ;; Show the variable `global-mode-string' which is usually nil.
     mode-line-misc-info
     ;; Horizontal gap for alignment.
     (propertize
      " "
      'display '((space
                  :align-to (- (+ right right-fringe right-margin) 8)))
      'face    'mode-line-inactive)
     ;; Cursor position information.
     (if (mode-line-window-selected-p)
         (if global-display-line-numbers-mode
             (list
              (propertize
               "%4c "
               'help-echo "Position, mouse-1: Toggle line numbers"
               'mouse-face 'mode-line-highlight
               'local-map (make-mode-line-mouse-map
                           'mouse-1 #'global-display-line-numbers-mode))
              '(-3 "%p")) ; Show "Bot" rather than "Bottom".
           ;; else
           (list
            (propertize
             "%4l,"
             'help-echo "Position, mouse-1: Toggle line numbers"
             'mouse-face 'mode-line-highlight
             'local-map (make-mode-line-mouse-map
                         'mouse-1 #'global-display-line-numbers-mode))
            "%c"))
       ;; else
       (list
        "     "
        '(-3 "%p")))))))

;; ============================================================================
;;; Other vanilla stuff
;; ============================================================================
;; Variables
;; ----------------------------------------------------------------------------
(setq-default ; buffer local variables.
 display-line-numbers-width 3 ; Minimum width.
 indent-tabs-mode nil)
(setq
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      ;; else
      "%b")))
 tab-width 4
 warning-minimum-level :error
 visible-bell t
 use-dialog-box nil
 use-short-answers t
 initial-scratch-message nil
 large-file-warning-threshold nil
 custom-file "/dev/null" ; I don't deal with custom.el.
 trash-directory "~/.local/share/Trash/files"
 delete-by-moving-to-trash t
 recentf-exclude
 '("\\`~/org/agenda/.*\\.org\\'"
   "\\`~/org/inbox\\.org\\'"
   "\\`~/\\.emacs\\.d/.*\\.el\\'"
   "\\`~/\\.emacs\\.d/bookmarks\\'"
   "\\`~/\\.emacs\\.d/diary\\'")
 shell-default-shell 'eshell ; I use a terminal outside Emacs when I need it.
 eshell-ls-initial-args
 '("-agho")
 ;; ----------------------------------------------------------------------------
 ;; Dired
 ;; ----------------------------------------------------------------------------
 dired-kill-when-opening-new-dired-buffer t
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "\\`[#\\.].*\\'" ; "a" to toggle.
 dired-omit-verbose nil
 auto-revert-verbose nil
 global-auto-revert-non-file-buffers t ; Update dired buffer.
 ;; ----------------------------------------------------------------------------
 ;; Incremental search (I use Emacs rather than vim tools for search)
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
 display-time-format "[%Y-%m-%d %a %H:%M]" ; The org timestamp format.
 display-line-numbers-type 'relative
 ;; Popups not associated with a file pop below the current window.
 display-buffer-alist
 '(("\\`\s?\\*.*\\*\s?\\'"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (body-function . select-window))))
;; ============================================================================
;;;; Global vanilla minor modes and hooks
;; ============================================================================
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
;; ----------------------------------------------------------------------------
;; Hooks
;; ----------------------------------------------------------------------------
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'dired-mode-hook  #'dired-omit-mode)         ; Toggle with "s".
(add-hook 'dired-mode-hook  #'dired-hide-details-mode) ; Toggle with "a".
(add-hook 'prog-mode-hook   #'outline-minor-mode)      ; Cycle with "S-tab".
(add-hook 'text-mode-hook   #'visual-line-mode)        ; Soft line breaks.
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
  "Open config file init.el."
  (interactive)
  (find-file (locate-user-emacs-file "early-init.el"))
  (find-file (locate-user-emacs-file "init.el")))
;; ----------------------------------------------------------------------------
;; Open agenda and plan file
;; ----------------------------------------------------------------------------
(defun my/open-agenda-file ()
  "Open my agenda file agenda.org."
  (interactive)
  (find-file (concat org-agenda-directory "plan.org"))
  (find-file (concat org-agenda-directory "agenda.org")))
;; ----------------------------------------------------------------------------
;; Open note and date file
;; ----------------------------------------------------------------------------
(defun my/open-note-file ()
  "Open my notes file note.org."
  (interactive)
  (find-file (concat org-agenda-directory "date.org"))
  (find-file (concat org-agenda-directory "note.org")))
;; ----------------------------------------------------------------------------
;; Custom agenda
;; ----------------------------------------------------------------------------
(defun my/org-agenda-custom ()
  "Custom agenda with NEXT, agenda and TODO/HOLD."
  (interactive)
  (org-agenda nil "c")
  (delete-other-windows)
  ;; If the point start on a heading move to today's date (no NEXT item).
  (unless (eq
           (char-after) ?\s)
    (org-agenda-goto-today)))
;; ----------------------------------------------------------------------------
;; Capture idea
;; ----------------------------------------------------------------------------
(defun my/org-capture-idea ()
  "Capture idea to inbox.org."
  (interactive)
  (org-capture nil "i")
  (org-save-all-org-buffers)
  (with-current-buffer "*Org Agenda*"
    (org-agenda-redo t) ; Might turn of hl-line for some reason?
    (beginning-of-buffer)))
;; ----------------------------------------------------------------------------
;; 3-window setup
;; ----------------------------------------------------------------------------
(defun my/3-windows ()
  "3 windows, two on the right and the left focused."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (mode-line-other-buffer) ; Switch to the most recent buffer.
  (split-window-below) ; (scratch-buffer) will situationally work differently.
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window -2)) ; Back to initial window.
;; ----------------------------------------------------------------------------
;; Ace window swap
;; ----------------------------------------------------------------------------
(defun my/ace-swap-window ()
  "Swap two windows (prompt if 3+). Keep focusing the current window."
  (interactive)
  (ace-swap-window)
  (aw-flip-window))
;; ----------------------------------------------------------------------------
;; Toggle my faces (theme)
;; ----------------------------------------------------------------------------
(defun my/toggle-faces ()
  "Toggle my 2 default faces."
  (interactive)
  (if (equal ; (eq...) will not work here. (equal...) is more relaxed.
       (face-background 'default) "#000")
      (load (locate-user-emacs-file "my-faces.elc") nil t)
    ;; else
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)))
;; ----------------------------------------------------------------------------
;; Magit stage and commit
;; ----------------------------------------------------------------------------
(defun my/magit-stage-all-and-commit (message)
  "Easy commit everything."
  (interactive "sCommit Message: ")
  (save-some-buffers t)
  (call-process-shell-command
   (format "git commit -a -m \"%s\" &" message) nil nil))
;; ----------------------------------------------------------------------------
;; Save and quit
;; ----------------------------------------------------------------------------
(defun my/save-all-kill-emacs-no-prompt ()
  ;; I use `super-save' and `backup-each-save'.
  "Save all and quit without prompt."
  (interactive)
  (save-some-buffers t)
  (when (file-newer-than-file-p
         (locate-user-emacs-file "early-init.el")
         (locate-user-emacs-file "early-init.elc"))
    (byte-compile-file (locate-user-emacs-file "early-init.el")))
  (when (file-newer-than-file-p
         (locate-user-emacs-file "init.el")
         (locate-user-emacs-file "init.elc"))
    (when (file-exists-p ; A fresh install has no compiled elc file to copy.
           (locate-user-emacs-file "init.elc"))
      (copy-file ; Backup to debug a broken config with a working Emacs.
       (locate-user-emacs-file "init.elc")
       (locate-user-emacs-file "init.elc~") t)) ; Overwrite.
    (byte-compile-file (locate-user-emacs-file "init.el")))
  (kill-emacs))

;; ============================================================================
;;; Package.el
;; ============================================================================
;; I use the old package.el system rather than a newer alternative.
;; The alternatives generally increase garbage collections to a point where
;; you need to know what you are doing to gain anything from using them.
;; I trust packages to defer stuff when it's warranted and useful and
;; I don't want to micromanage.
;; ----------------------------------------------------------------------------
(setq
 load-prefer-newer t ; Use .el if newer than .elc.
 package-archives
 '(("elpa"         . "https://elpa.gnu.org/packages/")
   ("melpa"        . "https://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
   ("org"          . "https://orgmode.org/elpa/"))
 package-archive-priorities
 '(("elpa"  . 2)  ; Minimize bleeding edge with old versions from elpa.
   ("melpa" . 1)) ; Other archives have priority 0.
 package-selected-packages
 '(evil evil-collection evil-nerd-commenter evil-surround evil-numbers evil-org
        org-superstar org-appear org-present magit cape corfu nerd-icons-corfu
        nerd-icons nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion
        avy vertico marginalia orderless consult embark embark-consult lsp-mode
        rainbow-delimiters rainbow-mode golden-ratio ace-window transpose-frame
        recursive-narrow centered-cursor-mode mixed-pitch indent-guide keycast
        undo-tree counsel helpful flycheck writegood-mode auto-package-update
        super-save backup-each-save general))
(unless (package-installed-p
         'package)
  (require 'package)) ; I keep this even though it is no longer needed.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)
;; ============================================================================
;;;; Misc. packages
;; ============================================================================
;; Update, undo, save and backup.
;; ----------------------------------------------------------------------------
(setq
 auto-package-update-interval 30
 ;; auto-package-update-prompt-before-update t
 auto-package-update-hide-results t)
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
;; ----------------------------------------------------------------------------
;; Magit
;; ----------------------------------------------------------------------------
(require 'magit)
(with-eval-after-load 'evil
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert))
;; ============================================================================
;;;; Windows
;; ============================================================================
(require 'golden-ratio)
(golden-ratio-mode 1)
(require 'transpose-frame)
(require 'ace-window)
(ace-window-display-mode 1)
(require 'centered-cursor-mode)
(global-centered-cursor-mode 1) ; I like this controversial mode.
;; ----------------------------------------------------------------------------
;; Narrow with dwim
;; ----------------------------------------------------------------------------
(with-eval-after-load 'org
  (require 'recursive-narrow))
(put 'narrow-to-region 'disabled nil) ; Disable a recursive-narrow warning.
;; ============================================================================
;;;; Completion
;; ============================================================================
;; Mini buffer
;; ----------------------------------------------------------------------------
(setq
 vertico-resize nil)
(require 'vertico)
(vertico-mode 1)
;; Tidy typing directories:
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(with-eval-after-load 'vertico
  (require 'marginalia)
  (marginalia-mode 1))
(setq
 completion-styles
 '(orderless)) ; Other options: (basic substring flex).
(require 'orderless) ; Fuzzy completions.
(require 'consult)   ; Combine functionality (e.g. buffers + recentf).
(require 'embark)    ; I don't use it yet.
;; ----------------------------------------------------------------------------
;; Buffer
;; ----------------------------------------------------------------------------
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
(setq
 avy-timeout-seconds 1) ; I'm slow and don't use this much.
(require 'avy)
;; ============================================================================
;;;; Help
;; ============================================================================
(require 'counsel)
(require 'helpful)
(bind-key [remap describe-function] 'counsel-describe-function)
(bind-key [remap describe-variable] 'counsel-describe-variable)
(bind-key [remap describe-command]  'helpful-command)
(bind-key [remap describe-key]      'helpful-key)
(require 'keycast)
(keycast-tab-bar-mode 1)
;; ----------------------------------------------------------------------------
;; Writing tips
;; ----------------------------------------------------------------------------
(require 'flycheck)
(add-hook 'org-mode-hook #'flyspell-mode)
(require 'writegood-mode)
;; ============================================================================
;;;; Thumbnails and colors
;; ============================================================================
(when (display-graphic-p)
  (require 'nerd-icons) ; I prefer nerd-icons to all-the-icons.
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
;; Color guiding
;; ----------------------------------------------------------------------------
(require 'lsp-mode)
(require 'indent-guide)
(add-hook 'prog-mode-hook #'indent-guide-mode)
(setq
 rainbow-delimiters-max-face-count 3)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
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

;; ============================================================================
;;; Evil.el
;; ============================================================================
;; Cursors are special. I combine f and 0 to color all types.
;; I tax the eyes a bit to follow the cursor/point indicating the evil states.
;; |------+----------+------+----------|
;; | #000 | not used | #fff | visual   |
;; |------+----------+------+----------|
;; | #f00 | operator | #ff0 | insert   |
;; | #0f0 | normal   | #0ff | emacs    |
;; | #00f | motion   | #f0f | replace  |
;; |------+----------+------+----------|
;; The `normal' state is green and the `operator' state is red to alert.
;; The `insert' state has the remaining yellow traffic light color and a `bar'.
;; The `replace' state is magenta and use a horizontal `hbar' cursor.
;; The `emacs' and `motion' states have the remaining cyan and blue rgb colors.
;; "Input" states have the colors with 2 f's and bars in common.
;; The `visual' state has a white hollow cursor.
;; ----------------------------------------------------------------------------
(setq
 evil-operator-state-cursor '(box        "#f00")
 evil-normal-state-cursor   '(box        "#0f0")
 evil-motion-state-cursor   '(box        "#00f")
 evil-insert-state-cursor   '((bar  . 4) "#ff0")
 evil-emacs-state-cursor    '((bar  . 4) "#0ff")
 evil-replace-state-cursor  '((hbar . 4) "#f0f")
 evil-visual-state-cursor   '(hollow     "#fff")
 evil-ex-search-persistent-highlight nil
 evil-ex-substitute-highlight-all nil
 evil-shift-round t
 evil-undo-system 'undo-tree
 evil-want-C-u-scroll t   ; In normal, visual and motion state.
 evil-want-keybinding nil ; evil-collection need this.
 evil-want-integration t
 vim-style-remap-Y-to-y$ t)
;; ============================================================================
;;;; Evil packages
;; ============================================================================
;; I dislike using my <ctrl> key and prefer modal to layered keybindings.
;; ----------------------------------------------------------------------------
(unless (package-installed-p
         'evil)
  (require 'evil))
(evil-mode 1)
(require 'evil-collection)
(evil-collection-init)
(require 'evil-surround)
(global-evil-surround-mode 1)
(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (push
    '(?` . ("`" . "'")) evil-surround-pairs-alist)))
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
;;;; Hooks to disable `hl-line' while marking a region or inputing
;; ============================================================================
(dolist
    (hook
     '(evil-visual-state-entry-hook
       evil-insert-state-entry-hook
       evil-replace-state-entry-hook
       evil-emacs-state-entry-hook))
  (add-hook
   hook
   (lambda ()
     (global-hl-line-mode 0))))
(dolist
    (hook
     '(evil-visual-state-exit-hook
       evil-insert-state-exit-hook
       evil-replace-state-exit-hook
       evil-emacs-state-exit-hook))
  (add-hook
   hook
   (lambda ()
     (global-hl-line-mode 1))))
;; ============================================================================
;;;; Evil_cursor_model (my controversial sacrilege)
;; ============================================================================
;; I use Emacs' cursor-between-characters model of cursor positioning in
;; `evil-mode' instead of Vim's normal state cursor-on-characters model.
;; ----------------------------------------------------------------------------
;; I use Vim's modal bindings but I rarely use capitalized bindings as `verbs'.
;; `<shift>' just like `<ctrl>' is a layer and I minimize the use of layers.
;; I swap "A"/"a", "O"/"o" and "P"/"p" to avoid layered capital letters.
;; I use "p" to paste and "a", "c", "i" or "o" to enter insert state.
;; ----------------------------------------------------------------------------
;; This cursor model is easier to internalize if you are not a power user.
;; I find it more consistent to use the same model in insert and normal state.
;; It is still modal but closer to other editing experiences.
;; More info: [[https://www.dr-qubit.org/Evil_cursor_model.html]]
;; Default evil/Vim behavior will work if the "(load..." line below is omitted.
;; ----------------------------------------------------------------------------
(when (file-newer-than-file-p
       (locate-user-emacs-file "evil-cursor-model.el")
       (locate-user-emacs-file "evil-cursor-model.elc"))
  (byte-compile-file (locate-user-emacs-file "evil-cursor-model.el")))
(load (locate-user-emacs-file "evil-cursor-model.elc") nil t)

;; ============================================================================
;;; Org.el
;; ============================================================================
;; Create org agenda directories and files unless they exist.
;; ----------------------------------------------------------------------------
(setq
 org-directory "~/org/")
(defvar
  org-agenda-directory (concat org-directory "agenda/")
  "Default org-agenda directory.")
(unless (file-exists-p
         org-directory)
  (make-directory org-directory))
(unless (file-exists-p
         org-agenda-directory)
  (make-directory org-agenda-directory))
;; Create empty capture files.
(unless (file-exists-p
         (concat org-directory "archive.org"))
  (make-empty-file (concat org-directory "archive.org")))
(unless (file-exists-p
         (concat org-directory "inbox.org"))
  (make-empty-file (concat org-directory "inbox.org")))
(unless (file-exists-p
         (concat org-agenda-directory "note.org"))
  (make-empty-file (concat org-agenda-directory "note.org")))
;; Create plan.org with a header.
(unless (file-exists-p
         (concat org-agenda-directory "plan.org"))
  (find-file (concat org-agenda-directory "plan.org"))
  (insert
   (concat
    "#+title: Projects\n"
    "#+startup: content hideblocks\n"
    "\n"))
  (save-buffer)
  (switch-to-buffer "*scratch*"))
;; Create agenda.org with two refile targets.
(unless (file-exists-p
         (concat org-agenda-directory "agenda.org"))
  (find-file (concat org-agenda-directory "agenda.org"))
  (insert
   (concat
    "#+title: Main agenda file\n"
    "#+startup: content hideblocks\n"
    "\n"
    "* [/] Event\n"
    ":PROPERTIES:\n"
    ":CATEGORY: Event\n"
    ":END:\n"
    "* [/] Task\n"
    ":PROPERTIES:\n"
    ":CATEGORY: Task\n"
    ":END:\n"))
  (save-buffer)
  (switch-to-buffer "*scratch*"))
;; ============================================================================
;;;; Variables setting up the org environment
;; ============================================================================
(setq
 org-default-notes-file (concat org-directory "inbox.org")
 org-ellipsis " … "
 ;; ----------------------------------------------------------------------------
 ;; 4 todo states: I use category and refile rather than more keywords.
 ;; ----------------------------------------------------------------------------
 org-todo-keywords
 '((type     "NEXT(n!/!)" "TODO(t!/!)" "|") ; Active states
   (type "|" "HOLD(h@/!)" "DONE(d!/!)"))  ; Inactive states
 org-priority-default ?C
 org-priority-faces ; This affects rendering in the agenda.
 '((?A . (:slant nil :height 0.8)) ; For some reason the default is slanted.
   (?B . (:slant nil :height 0.8)) ; I don't know how to fix it with faces.
   (?C . (:slant nil :height 0.8))) ; I have not investigated since this works.
 org-list-allow-alphabetical t
 org-list-demote-modify-bullet
 '(("+" . "*")
   ("*" . "-")
   ("-" . "+"))
 org-tags-column -75 ; Minus align tags right.
 org-tag-alist
 '(("pc"       . ?c) ; c for computer.
   ("emacs"    . ?e)
   ("family"   . ?f)
   ("game"     . ?g)
   ("home"     . ?h)
   ("idea"     . ?i)
   ("money"    . ?m)
   ("phone"    . ?p)
   ("work"     . ?w)
   ;; Special keyword tags.
   ("CRYPT"    . ?C)
   ("ORDERED"  . ?O)
   ("NOEXPORT" . ?X))
 ;; org-startup-folded 'show2levels
 org-cycle-hide-block-startup t
 org-confirm-babel-evaluate nil
 org-M-RET-may-split-line
 '((default . nil))
 org-insert-heading-respect-content t
 ;; ----------------------------------------------------------------------------
 ;; Logbook
 ;; ----------------------------------------------------------------------------
 org-log-into-drawer 'logbook
 org-log-done        'time
 org-log-refile      'time
 org-log-reschedule t
 org-log-note-headings
 '((state       . "State %6s from %-9S %t") ; Align timestamps (with capture).
   (note        . "Note                        %t")
   (refile      . "Refiled                     %t")
   (done        . "Closing note                %t")
   (clock-out   . "Timer stopped               %t")
   (reschedule  . "Rescheduled                 %t from %S")
   (delschedule . "Unscheduled                 %t, was %S")
   (redeadline  . "New deadline                %t from %S")
   (deldeadline . "Deadline removed            %t, was %S"))
 ;; ----------------------------------------------------------------------------
 ;; Export
 ;; ----------------------------------------------------------------------------
 org-html-postamble      nil
 org-latex-title-command nil
 org-export-with-smart-quotes t
 org-export-backends
 '(ascii latex beamer texinfo html odt md org)
 org-file-apps
 '(("\\.docx\\'"    . default)
   ("\\.mm\\'"      . default)
   ("\\.x?html?\\'" . default)
   ("\\.pdf\\'"     . "evince %s") ; External pdf viewer.
   (directory       . emacs)
   (auto-mode       . emacs)))
;; ============================================================================
;;;; Capture
;; ============================================================================
(setq
 org-capture-templates
 `(("c" "Category" entry ; Back tick to "concat" for code readability.
    (file ,(concat org-agenda-directory "plan.org"))
    ,(concat
      "* TODO [/] %^{Heading}\n"
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
    :immediate-finish t)))
;; ============================================================================
;;;; Agenda
;; ============================================================================
;; System to organize tasks and suppress out of date information.
;; It's not primarily a calendar. It's all about task management.
;; ----------------------------------------------------------------------------
(setq
 org-agenda-window-setup 'current-window
 org-archive-location (concat org-directory "archive.org::* Archive")
 org-refile-targets
 `((,(concat org-agenda-directory "agenda.org") :maxlevel . 1)
   (,(concat org-agenda-directory "plan.org")   :maxlevel . 1))
 org-agenda-files (list (concat org-directory "inbox.org")
                        org-agenda-directory) ; Org files in the agenda dir.
 org-agenda-format-date " [%F %a] "
 org-agenda-block-separator ?﹋ ; Other options: ﹌⎺̅‾﹉
 org-agenda-span 'month
 org-agenda-custom-commands
 '(("c" "Custom agenda setup"
    ((todo "NEXT"
           ;; Unblocked NEXT tasks often for scheduling and refiling.
           ((org-agenda-overriding-header "")))
     (agenda ""
             ;; Supress timestamped active state items that are not current.
             ((org-agenda-span 'week)))
     ;; TODO without a timestamp.
     (todo "TODO"
           ;; With [/] cookies. This is typically categories.
           ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'notregexp "\\[[0-9]+/[0-9]+\\]"
               ;; or
               'timestamp))))
     (todo "TODO"
           ;; Others. Not all TODOs have or should have a timestamp.
           ((org-agenda-overriding-header "") ; share heading (same block)
            (org-agenda-block-separator nil)  ; don't separate
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'regexp "\\[[0-9]+/[0-9]+\\]"
               ;; or
               'timestamp))))
     ;; Inactive states. Will not show in the agenda even if scheduled.
     (todo "HOLD"
           ;; HOLD for third party action pending.
           ((org-agenda-overriding-header "") ; same block
            (org-agenda-block-separator nil)))
     (todo "DONE"
           ;; Prioritized DONE.
           ((org-agenda-overriding-header "") ; same block
            (org-agenda-block-separator nil)
            (org-agenda-skip-function
             '(org-agenda-skip-entry-if
               'notregexp org-priority-regexp))))))
   ;; Custom options for maintainance.
   ("d" "DONE TODOs"
    ;; For archiving done tasks.
    ((todo "DONE"
           ((org-agenda-overriding-header "DONE TODOs")))))
   ("u" "Untagged TODOs"
    ;; For adding tags.
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
 '("⧄" "⧄%3dx")
 org-agenda-deadline-leaders
 '("⧅" "⧅%3dd" "⧅%3dx")
 org-agenda-timerange-leaders
 '("Range" "%2d/%d")
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 calendar-week-start-day 1
 org-columns-default-format "%30Item %Clocksum(Used) %Effort(Plan) %Category(Cat.) %Tags %Priority(#) %Todo(Todo)"
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
 holiday-other-holidays ; Custom diary formatted.
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
 diary-mark-entries t)
;; ============================================================================
;;;; Load org
;; ============================================================================
(unless (package-installed-p
         'org)
  (require 'org))
;; ----------------------------------------------------------------------------
;; A pictogram is often better than a word
;; ----------------------------------------------------------------------------
(add-hook
 'org-mode-hook
 (lambda ()
   (setq
    prettify-symbols-alist ; utf8's that work in the terminal.
    '(("[-]"            . ?⊟) ; utf8 options: ⧈⧇⊡⧆⊞⊟⧄⧅⊠⟏⟎ ▸▴▾◂ ✏✎✐ ☑☐☒
      ("[ ]"            . ?⊡)
      ("[X]"            . ?⊠)
      ("CLOSED:"        . ?⧇)
      ("SCHEDULED:"     . ?⧄)
      ("DEADLINE:"      . ?⧅)
      (":PROPERTIES:"   . ?⚙) ; Settings.
      (":LOGBOOK:"      . ?☰) ; Meta data.
      ("CLOCK:"         . ?–) ; Other items in the logbook have a dash bullet.
      (":END:"          . ?▴)
      ("#+begin_export" . ?▾)
      ("#+end_export"   . ?▴)
      ("#+begin_src"    . ?▾)
      ("#+end_src"      . ?▴))
    prettify-symbols-unprettify-at-point t)
   (prettify-symbols-mode 1)))
;; ----------------------------------------------------------------------------
;; Update "[/]" cookie
;; ----------------------------------------------------------------------------
(add-hook
 'org-after-todo-state-change-hook
 (lambda () ; Overkill to update all "[/]" cookies with every state change.
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
 '(?⓪ ?① ?② ?③ ?ⓧ)
 org-superstar-cycle-headline-bullets nil) ; ⓧ for all 5+ depth headings.
(require 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)
;; ----------------------------------------------------------------------------
;; Habits
;; ----------------------------------------------------------------------------
(add-to-list 'org-modules 'org-habit)
(setq
 org-habit-preceding-days 28 ; 4 weeks.
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
;;; General.el
;; ============================================================================
;; This does not work on Android for some reason?
;; ----------------------------------------------------------------------------
(setq
 which-key-idle-delay 0)
(which-key-mode 1)
(require 'general)
(general-evil-setup t)
(general-auto-unbind-keys t)
(general-create-definer my/set-leader-keys
  :keymaps '(motion insert emacs)
  :prefix "SPC"
  :global-prefix "C-SPC") ; Visual state ("v") sets the mark.
(my/set-leader-keys
  ""    nil
  "SPC" '(counsel-M-x                            :which-key "M-x")
  ;; Toggle 2 top buffers:
  "TAB" '(mode-line-other-buffer                 :which-key "Toggle buf")
  ;; "<spc>-#" is inspired by emacs "C-x #" bindings where # is a number.
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
  "e"   '(:ignore t                              :which-key "Evaluate")
  "f"   '(:ignore t                              :which-key "Files")
  "fS"  '(save-some-buffers                      :which-key "Save all")
  "fa"  '(my/open-agenda-file                    :which-key "Agenda")
  "fd"  '(dired-jump                             :which-key "Dired")
  "ff"  '(counsel-find-file                      :which-key "Find")
  "fi"  '(my/open-init-file                      :which-key "Init")
  "fm"  '(consult-recent-file                    :which-key "Mini recent")
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
  "ll"  '(eval-expression                        :which-key "Expression")
  "ls"  '(eval-last-sexp                         :which-key "Sexp @point")
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
  "ot"  '(evil-org-org-insert-todo-heading-respect-content-below :which-key "New todo")
  "or"  '(:ignore t                              :which-key "Org roam")
  "p"   '(:ignore t                              :which-key "p")
  "q"   '(:ignore t                              :which-key "Quit")
  "qq"  '(my/save-all-kill-emacs-no-prompt       :which-key "Save&kill")
  "qs"  '(save-buffers-kill-emacs                :which-key "Prompt&kill")
  "r"   '(:ignore t                              :which-key "Registers")
  "rl"  '(consult-register-load                  :which-key "Load")
  "rm"  '(counsel-mark-ring                      :which-key "Marks")
  "rr"  '(consult-register-store                 :which-key "Store")
  "rv"  '(exchange-point-and-mark                :which-key "Visual mark")
  "s"   '(:ignore t                              :which-key "Search")
  "so"  '(consult-outline                        :which-key "Outline")
  "sO"  '(occur                                  :which-key "Occur")
  "sr"  '(query-replace                          :which-key "Replace")
  "sR"  '(query-replace-regexp                   :which-key "Rep. regex")
  "ss"  '(swiper                                 :which-key "Swiper")
  "sw"  '(eww                                    :which-key "Web (eww)")
  "t"   '(:ignore t                              :which-key "Toggle")
  "tc"  '(global-centered-cursor-mode            :which-key "Vert.center")
  "tf"  '(mixed-pitch-mode                       :which-key "Font pitch")
  "tg"  '(golden-ratio-mode                      :which-key "Gold ratio")
  "th"  '(global-hl-line-mode                    :which-key "Hl line")
  "tk"  '(keycast-tab-bar-mode                   :which-key "Keycast")
  "tl"  '(global-display-line-numbers-mode       :which-key "Line number")
  "to"  '(outline-minor-mode                     :which-key "Outline")
  "tp"  '(prettify-symbols-mode                  :which-key "Prettify")
  "tr"  '(rainbow-mode                           :which-key "Rainbow")
  "ts"  '(flyspell-mode                          :which-key "Spell")
  "tt"  '(my/toggle-faces                        :which-key "Theme")
  "tw"  '(writegood-mode                         :which-key "Write good")
  "u"   '(universal-argument                     :which-key "Uni arg")
  "v"   '(:ignore t                              :which-key "v")
  "w"   '(:ignore t                              :which-key "Window")
  "wb"  '(evil-window-split                      :which-key "Split below")
  "wd"  '(evil-window-delete                     :which-key "Delete")
  "wh"  '(evil-window-left                       :which-key "Left")
  "wj"  '(evil-window-down                       :which-key "Down")
  "wk"  '(evil-window-up                         :which-key "Up")
  "wl"  '(evil-window-right                      :which-key "Right")
  "wr"  '(evil-window-vsplit                     :which-key "Split right")
  "wR"  '(rotate-frame-anticlockwise             :which-key "Rot. frame")
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
  "zg"  '(global-text-scale-adjust               :which-key "Global")
  "zl"  '(text-scale-adjust                      :which-key "Local"))

;; ============================================================================
;;; Keybindings
;; ============================================================================
(bind-key* "¤" 'evil-normal-state) ; Experimenting with a phone keyboard.
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
 ("M-p"           . consult-yank-pop) ; Paste with `kill-ring' dialog.
 ;; ----------------------------------------------------------------------------
 ;; Operator state keys!!!
 ;; ----------------------------------------------------------------------------
 :map evil-operator-state-map
 ;; Bad things happen if I hit "å" in operator state without this.
 ;; Danish keyboard
 ("å"             . keyboard-escape-quit)
 ;; ----------------------------------------------------------------------------
 ;; Motion state keys!!! (normal, visual and motion)
 ;; ----------------------------------------------------------------------------
 :map evil-motion-state-map
 ("<down>"        . evil-next-visual-line)     ; up/down navigate wrapped lines.
 ("<up>"          . evil-previous-visual-line) ; j/k respect line atoms in vim.
 ("´"             . other-window)
 ("¨"             . next-buffer) ; opposite direction of the "<spc> <tab>" swap.
 ("½"             . tab-new)
 ("gc"            . evilnc-comment-operator)
 ;; Danish keyboard
 ("æ"             . evil-forward-paragraph)
 ("Æ"             . evil-backward-paragraph)
 ("ø"             . evil-end-of-line)
 ("Ø"             . evil-first-non-blank)
 ("å"             . my/org-agenda-custom)
 ("Å"             . my/org-capture-idea)
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
 ;; "S" is dedicated to `evil-surround'. Use "C-s" for `isearch-forward'.
 ("s"             . isearch-forward-thing-at-point) ; With region as input.
 ;; I change Vim's visual state behavior and exit with <esc> like insert state.
 ("v"             . exchange-point-and-mark)
 ;; ----------------------------------------------------------------------------
 ;; Insert state keys!!!
 ;; ----------------------------------------------------------------------------
 ;; I come from Emacs so I like to access some commands in insert state.
 :map evil-insert-state-map
 ("C-g"           . evil-normal-state)
 ("C-b"           . evil-backward-word-begin)
 ("C-B"           . evil-backward-WORD-begin)
 ("C-d"           . backward-kill-word) ; I use "C-w" for forward word.
 ("C-e"           . forward-word) ; The end of the word.
 ("C-p"           . yank)
 ("M-p"           . consult-yank-pop)
 ("C-u"           . universal-argument)
 ("C-w"           . evil-forward-word-begin)
 ("C-W"           . evil-forward-WORD-begin)
 ("C-v"           . set-mark-command) ; "C-<spc>" is used by general.el.
 ("C-0"           . evil-beginning-of-line)
 ;; Danish keyboard
 ("C-æ"           . evil-forward-paragraph)
 ("C-Æ"           . evil-backward-paragraph)
 ("C-ø"           . move-end-of-line)
 ("C-Ø"           . evil-first-non-blank)
 ;; ----------------------------------------------------------------------------
 ;; Other maps
 ;; ----------------------------------------------------------------------------
 :map org-mode-map
 ;; Danish keyboard
 ("C-æ"           . org-next-visible-heading)
 ("C-Æ"           . org-previous-visible-heading)
 :map outline-minor-mode-map
 ("<tab>"         . outline-cycle)
 ([backtab]       . outline-cycle-buffer)
 :map corfu-map
 ("C-M-SPC"       . corfu-insert-separator) ; For orderless.
 ("<tab>"         . corfu-next)
 ([tab]           . corfu-next)
 ([backtab]       . corfu-previous)
 ("S-<tab>"       . corfu-previous)
 :map ccm-map
 ("<prior>"       . nil)
 ("<next>"        . nil)
 :map minibuffer-local-map
 ("C-."           . embark-act)
 ("M-."           . embark-dwim)
 ("<next>"        . marginalia-cycle)
 :map org-present-mode-keymap
 ("<left>"        . org-present-prev)
 ("<right>"       . org-present-next)
 ("<up>"          . org-present-beginning)
 ("<down>"        . org-present-end))
;; ============================================================================
;;;; Keys in maps depending on evil state
;; ============================================================================
(evil-define-key 'normal 'global
  ;; I never use Vim's substitute "s". If I need it "cl" does the same thing.
  "s"         #'isearch-forward                 ; I like Emacs' "C-s" search.
  "S"         #'isearch-forward-thing-at-point) ; A bit like Vim's "*".
(evil-define-key 'normal org-mode-map
  "t"         #'org-todo
  "T"         #'org-todo-yesterday)
(evil-define-key 'motion org-agenda-mode-map
  (kbd "SPC") nil ; Make general.el take over "<spc>".
  (kbd "S-<left>")  #'org-agenda-earlier
  (kbd "S-<right>") #'org-agenda-later
  "a"         #'org-agenda-append-agenda
  "A"         #'org-agenda-archive-default-with-confirmation
  "R"         #'org-agenda-refile
  "T"         #'org-agenda-todo-yesterday
  "gy"        #'org-agenda-year-view
  "sd"        #'org-agenda-deadline
  "ss"        #'org-agenda-schedule
  "n"         #'org-agenda-add-note
  ;; Danish keyboard
  "æ"         #'org-agenda-forward-block
  "Æ"         #'org-agenda-backward-block
  "å"         #'org-agenda-columns
  "Å"         #'my/org-capture-idea)
(evil-define-key 'emacs ibuffer-mode-map
  "´"         #'evil-window-next
  "¨"         #'evil-next-buffer
  ;; Danish keyboard
  "å"         #'my/org-agenda-custom
  "Å"         #'my/org-capture-idea)
(evil-define-key 'normal dired-mode-map
  (kbd "SPC") nil ; Make general.el take over "<spc>".
  "a"         #'dired-hide-details-mode
  "h"         #'dired-up-directory
  "l"         #'dired-find-file
  "s"         #'dired-omit-mode)
(evil-define-key 'normal help-mode-map
  (kbd "SPC") nil) ; Make general.el take over "<spc>".

;; ============================================================================
;;; Startup page
;; ============================================================================
(my/org-agenda-custom)
;; End of init.el
