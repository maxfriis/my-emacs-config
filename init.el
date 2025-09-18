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
 ;; Fallback mono space font.
 ((find-font
   (font-spec :name "DejaVu Sans Mono"))
  (set-face-attribute
   'default
   nil :font "DejaVu Sans Mono")))
;; The variable width Ubuntu font doesn't look good with Ubuntu Mono imo.
(when (find-font
       (font-spec :name "Verdana"))
  (set-face-attribute
   'variable-pitch
   nil :font "Verdana-18"))
;; ----------------------------------------------------------------------------
;; Compile the lisp files I load.
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
;; Use files for faces rather than variables so I see the colors when I edit.
;; ----------------------------------------------------------------------------
(if (eq (user-uid) 0) ; Ansi colors as root.
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)
  ;; else
  (load (locate-user-emacs-file "my-faces.elc") nil t))

;; ============================================================================
;;; Mode line
;; ============================================================================
(setq-default ; `mode-line-format' is buffer local so `setq-default'.
 mode-line-format
 '((:eval ; Eval and list everything.
    (list
     "%e" ; Potentially show a memory error I have never seen.
     " "
     ;; Indicators for writable and modified.
     mode-line-modified
     ;; Custom indicator.
     (cond
      ((and
        (eq (user-uid) 0)
        (mode-line-window-selected-p))
       (propertize
        "#"
        'help-echo "Root access"
        'face 'warning)) ; Emphasize this indicator.
      ((buffer-narrowed-p)
       (propertize
        "n"
        'help-echo "Narrowed, mouse-1: Widen"
        'mouse-face 'mode-line-highlight
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'mode-line-widen)))
      (t " "))
     " "
     ;; Buffer name.
     mode-line-buffer-identification
     " "
     ;; Major mode name made shorter.
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
      'help-echo (concat
                  (symbol-name major-mode) ; Message full name on mouse hover.
                  ", mouse-1: Toggle last two buffers")
      'mouse-face 'mode-line-highlight
      'local-map (make-mode-line-mouse-map
                  'mouse-1 #'mode-line-other-buffer))
     ;; Version control in active window.
     (when (and
            vc-mode
            (mode-line-window-selected-p))
       (replace-regexp-in-string
        "\\` Git" " "
        vc-mode))
     " "
     mode-line-misc-info ; Show `global-mode-string'. It's usually nil.
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
               'help-echo "Column number, mouse-1: Toggle line numbers"
               'mouse-face 'mode-line-highlight
               'local-map (make-mode-line-mouse-map
                           'mouse-1 #'global-display-line-numbers-mode))
              '(-3 "%p")) ; Show "Bot" rather than "Bottom".
           ;; else
           (list
            (propertize
             "%4l,"
             'help-echo "Line number, mouse-1: Toggle line numbers"
             'mouse-face 'mode-line-highlight
             'local-map (make-mode-line-mouse-map
                         'mouse-1 #'global-display-line-numbers-mode))
            (propertize
             "%3c  " ; 2 spaces so the mouse hover highlight include eol.
             'help-echo "Column number, mouse-1: Toggle truncation"
             'mouse-face 'mode-line-highlight
             'local-map (make-mode-line-mouse-map
                         'mouse-1 #'toggle-truncate-lines))))
       ;; else
       (list
        "     "
        '(-3 "%p")))))))

;; ============================================================================
;;; Other vanilla stuff
;; ============================================================================
;; Variables
;; ----------------------------------------------------------------------------
(setq-default ; Buffer local variables.
 indent-tabs-mode nil          ; No tabulator character pollution please.
 fill-column 79                ; `display-fill-column-indicator-mode' use this.
 display-line-numbers-width 3) ; Minimum width.
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
 trash-directory "~/.local/share/Trash/files"
 delete-by-moving-to-trash t
 custom-file "/dev/null"           ; Don't deal with custom.el.
 truncate-partial-width-windows 79 ; Don't wrap lines in narrow windows.
 shell-default-shell 'eshell       ; I rarely use a terminal inside Emacs.
 eshell-ls-initial-args
 '("-agho")
 recentf-exclude
 '("\\`~/org/agenda/.*\\.org\\'"   ; No agenda files.
   "\\`~/org/inbox\\.org\\'"
   "\\`~/\\.emacs\\.d/.*\\.el\\'"  ; No Emacs config files.
   "\\`~/\\.emacs\\.d/bookmarks\\'"
   "\\`~/\\.emacs\\.d/diary\\'")
 ;; ----------------------------------------------------------------------------
 ;; Dired
 ;; ----------------------------------------------------------------------------
 dired-kill-when-opening-new-dired-buffer t
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "\\`[#\\.].*\\'" ; "a" will toggle.
 dired-omit-verbose nil
 auto-revert-verbose nil
 global-auto-revert-non-file-buffers t ; Update dired buffer when files change.
 ;; ----------------------------------------------------------------------------
 ;; Incremental search (I use Emacs rather than Vim tools for search)
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
 '(tab-bar-format-menu-bar
   tab-bar-format-history
   tab-bar-format-tabs
   tab-bar-separator)
 ;; ----------------------------------------------------------------------------
 ;; Display
 ;; ----------------------------------------------------------------------------
 display-time-format "[%Y-%m-%d %a %H:%M]" ; The org timestamp format.
 ;; I swich to 'visual when using `outline'.
 display-line-numbers-type 'relative ; Works best with wraped lines.
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
(add-hook 'after-save-hook  #'vc-refresh-state)        ; Version control.
(add-hook 'dired-mode-hook  #'dired-omit-mode)         ; Toggle rebound to "a".
(add-hook 'dired-mode-hook  #'dired-hide-details-mode) ; Toggle rebound to "s".
(add-hook 'text-mode-hook   #'visual-line-mode)        ; Soft word line breaks.
(add-hook 'prog-mode-hook   #'outline-minor-mode)      ; Cycle with "S-<tab>".
;; Hack: Use visual line numbers when cycling `outline' or `org-mode' headings.
;; Exiting `evil-insert-state' or similar events will return to the default
;; relative line numbers which works better with wrapped lines.
(add-hook
 'outline-view-change-hook
 (lambda ()
   (when display-line-numbers
     (setq
      display-line-numbers 'visual))))
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
;;; My custom functions
;; ============================================================================
;; Open init files
;; ----------------------------------------------------------------------------
(defun my/open-init-file ()
  "Open configuration file init.el."
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
  (unless (eq (char-after) ?\s) ; A line with an item will start with a space.
    (org-agenda-goto-today))) ; This happens if the cursor start on a heading.
;; ----------------------------------------------------------------------------
;; Capture idea
;; ----------------------------------------------------------------------------
(defun my/org-capture-idea ()
  "Capture an idea to inbox.org and make it a NEXT item."
  (interactive)
  (org-capture nil "i")
  (org-save-all-org-buffers)
  (with-current-buffer "*Org Agenda*"
    (org-agenda-redo) ; Might turn `global-hl-line-mode' off?!?
    (when my/hl-line-p ; Hack to turn it back on only when it was on.
      (global-hl-line-mode 1)))
  ;; Somehow org-agenda-redo affects hl/line-p
  (goto-char (point-min))) ; Jump to the newly created NEXT item.
;; ----------------------------------------------------------------------------
;; Three window setup
;; ----------------------------------------------------------------------------
(defun my/3-windows ()
  "Three windows, two on the right and the left focused.
  \nThis nice setup is surprisingly hard to create without this function."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (next-buffer)        ; Why is this not `previous-buffer'?!?
  (split-window-below) ; `scratch-buffer' don't work if scratch is open.
  (other-window 1)
  (switch-to-buffer "*scratch*")
  (other-window -2))   ; Back to initial window.
;; ----------------------------------------------------------------------------
;; Ace window swap
;; ----------------------------------------------------------------------------
(defun my/ace-swap-window ()
  "Swap window contents (prompt if 3+). Keep focusing the current window.
\nThe normal `ace-swap-window' swap two windows, but stays with the current buffer and fucus the new window."
  (interactive)
  (ace-swap-window)
  (aw-flip-window))
;; ----------------------------------------------------------------------------
;; Toggle my faces (theme)
;; ----------------------------------------------------------------------------
(defun my/toggle-faces ()
  "Toggle my two default faces.
\nThey are loaded from my-faces.elc and my-ansi-faces.elc respectively."
  (interactive)
  (if (equal
       (face-background 'default) "#000")
      (load (locate-user-emacs-file "my-faces.elc") nil t)
    ;; else
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)))
;; ----------------------------------------------------------------------------
;; Magit stage and commit
;; ----------------------------------------------------------------------------
(defun my/magit-stage-all-and-commit (message)
  "Easy stage and commit everything.
  \nThis will respect what you have configured to be ignored."
  (interactive "sCommit Message: ")
  (save-some-buffers t)
  (call-process-shell-command
   (format "git commit -a -m \"%s\"" message) nil nil)
  (vc-refresh-state)) ; Doesn't work if the shell command ends with "&".
;; ----------------------------------------------------------------------------
;; Save and quit
;; ----------------------------------------------------------------------------
(defun my/save-all-kill-emacs-no-prompt ()
  "Save all and quit without a prompt.
  \nUse `super-save-mode' and `backup-each-save' to make this less risky."
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
      (copy-file ; Backup to easily debug a broken config with a working Emacs.
       (locate-user-emacs-file "init.elc")
       (locate-user-emacs-file "init.elc~") t)) ; Overwrite.
    (byte-compile-file (locate-user-emacs-file "init.el")))
  (kill-emacs))

;; ============================================================================
;;; Package.el
;; ============================================================================
;; I use the good old package.el manager rather than a newer alternative.
;; The alternatives generally increase garbage collections to a point where
;; you need to know what you are doing to gain anything from using them.
;; I trust packages to sensibly defer and don't want to micromanage.
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
 '(("elpa"  . 2)  ; Minimize bleeding edge. Prefer older versions from elpa.
   ("melpa" . 1)) ; The remaining archives have priority 0.
 package-selected-packages
 '(evil evil-collection evil-nerd-commenter evil-surround evil-numbers evil-org
        org-superstar org-appear org-present magit cape corfu nerd-icons-corfu
        nerd-icons nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion
        avy vertico marginalia orderless consult embark embark-consult lsp-mode
        rainbow-delimiters rainbow-mode golden-ratio ace-window transpose-frame
        recursive-narrow centered-cursor-mode mixed-pitch indent-guide keycast
        undo-tree counsel helpful flycheck writegood-mode auto-package-update
        super-save backup-each-save general))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages) ; This will download the packages.
;; ============================================================================
;;;; Update, save and backup
;; ============================================================================
(setq
 auto-package-update-interval 30
 auto-package-update-hide-results t)
(require 'auto-package-update)
(auto-package-update-maybe)
(require 'savehist)
(savehist-mode 1)
(require 'saveplace)
(save-place-mode 1)
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
(add-hook 'magit-run-post-commit-hook  #'vc-refresh-state)
;; ============================================================================
;;;; Help and guides
;; ============================================================================
(require 'counsel)
(require 'helpful)
(require 'keycast)
(keycast-tab-bar-mode 1)
;; ----------------------------------------------------------------------------
;; Writing tips
;; ----------------------------------------------------------------------------
(require 'flycheck)
(add-hook 'org-mode-hook #'flyspell-mode)
(require 'writegood-mode)
;; ----------------------------------------------------------------------------
;; Color guide
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
;; Thumbnails
;; ----------------------------------------------------------------------------
(when (display-graphic-p)
  (require 'nerd-icons)
  (require 'nerd-icons-dired)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (require 'nerd-icons-ibuffer)
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  (with-eval-after-load 'corfu
    (require 'nerd-icons-corfu)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
  (with-eval-after-load 'marginalia
    (require 'nerd-icons-completion)
    (nerd-icons-completion-mode 1)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))
;; ============================================================================
;;;; Buffers, completion and windows
;; ============================================================================
(setq
 avy-timeout-seconds 1) ; I'm slow.
(require 'avy)
(with-eval-after-load 'org
  (require 'recursive-narrow)
  (put 'narrow-to-region 'disabled nil)) ; Disable a warning.
(setq
 mixed-pitch-set-height t)
(require 'mixed-pitch)
(dolist
    (face
     '(org-special-keyword org-date org-tag org-priority org-todo org-table))
  (add-to-list 'mixed-pitch-fixed-pitch-faces face))
;; ----------------------------------------------------------------------------
;; Undo
;; ----------------------------------------------------------------------------
(setq
 undo-tree-visualizer-diff t
 undo-tree-visualizer-timestamps t
 undo-tree-auto-save-history t)
(require 'undo-tree)
(global-undo-tree-mode 1)
;; ----------------------------------------------------------------------------
;; Mini buffer
;; ----------------------------------------------------------------------------
(setq
 vertico-resize nil)
(require 'vertico)
(vertico-mode 1)
(with-eval-after-load 'vertico
  (require 'marginalia)
  (marginalia-mode 1))
;; Tidy typing directories:
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(setq
 completion-styles
 '(orderless)) ; Other options: (basic substring flex).
(require 'orderless) ; Fuzzy completions.
(require 'consult)   ; Combine functionality (e.g. buffers + recentf).
(require 'embark)    ; I don't really use this yet.
;; ----------------------------------------------------------------------------
;; Buffer completion
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
;; ----------------------------------------------------------------------------
;; Windows
;; ----------------------------------------------------------------------------
(require 'transpose-frame)
(require 'ace-window)
(ace-window-display-mode 1)
(require 'golden-ratio)
(golden-ratio-mode 1)
(require 'centered-cursor-mode)
(add-to-list 'ccm-ignored-commands 'mwheel-scroll) ; Enable mouse wheel scroll.
(global-centered-cursor-mode 1)                    ; Toggle with `Scroll_Lock'.

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
;; The `motion' and `emacs' state have the remaining blue and cyan ansi colors.
;; "Input" states have the colors with two "f"s and bars in common.
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
 evil-undo-system 'undo-tree ; Set this variabel before loading evil!
 evil-want-C-i-jump nil      ; I need "C-i" to make <tab> work in terminal.
 evil-want-keybinding nil    ; `evil-collection' need this.
 evil-want-integration t
 evil-shift-round t ; I don't use evil's search but configure this anyways.
 evil-ex-search-persistent-highlight nil
 evil-ex-substitute-highlight-all nil)
(require 'evil)
(evil-mode 1)
;; ============================================================================
;;;; Addons
;; ============================================================================
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
;;;; Hooks to suspend `hl-line'
;; ============================================================================
(defvar
  my/hl-line-p t
  "Track if `hl-line' was on or off when an `evil-state' suspending it was entered.")
;; I disable `hl-line' in visual state so `region' can have the same face.
(add-hook
 'evil-visual-state-entry-hook
 (lambda ()
   (setq
    my/hl-line-p global-hl-line-mode) ; Track to preserve `hl-line' on exit.
   (global-hl-line-mode 0)))
(add-hook
 'evil-visual-state-exit-hook
 (lambda ()
   (when my/hl-line-p
     (global-hl-line-mode 1))))
;; Evil "input" states have absolute line numbers and suspend the `hl-line'.
(dolist
    (hook
     '(evil-insert-state-entry-hook
       evil-replace-state-entry-hook
       evil-emacs-state-entry-hook))
  (add-hook
   hook
   (lambda ()
     (when display-line-numbers
       (setq
        display-line-numbers t))
     (setq
      my/hl-line-p global-hl-line-mode)
     (global-hl-line-mode 0))))
(dolist
    (hook
     '(evil-insert-state-exit-hook
       evil-replace-state-exit-hook
       evil-emacs-state-exit-hook))
  (add-hook
   hook
   (lambda ()
     (when display-line-numbers
       (setq
        display-line-numbers 'relative))
     (when my/hl-line-p
       (global-hl-line-mode 1)))))
;; ============================================================================
;;;; Evil cursor between model (my controversial sacrilege)
;; ============================================================================
;; I use Emacs' cursor-between-characters model of cursor positioning in
;; `evil-mode' instead of Vim's normal state cursor-on-characters model.
;; ----------------------------------------------------------------------------
;; I use Vim's modal bindings but I rarely use capitalized bindings as `verbs'.
;; `<shift>' just like `<ctrl>' is a layer and I dislike the use of layers.
;; I swap "a"/"A", "o"/"O" and "p"/"P" to minimize the use of capital letters.
;; I only use "p" to paste and "o", "I", "i", "a" or "c" to enter insert state.
;; ----------------------------------------------------------------------------
;; This cursor model is easier to internalize if you are not a power user.
;; It's more consistent to use the same model in both insert and normal state.
;; Editing is still modal but closer to other editing experiences than Vim.
;; More info: [[https://www.dr-qubit.org/evil_cursor_model.html]]
;; Default `evil' behavior will work if the "(load..." line below is omitted.
;; ----------------------------------------------------------------------------
(when (file-newer-than-file-p
       (locate-user-emacs-file "evil-cursor-between-model.el")
       (locate-user-emacs-file "evil-cursor-between-model.elc"))
  (byte-compile-file (locate-user-emacs-file "evil-cursor-between-model.el")))
(load (locate-user-emacs-file "evil-cursor-between-model.elc") nil t)
;; Working on a `minor-mode' called `evil-cursor-between-mode'.

;; ============================================================================
;;; Org.el
;; ============================================================================
;; Create `org-agenda' directories and files unless they exist.
;; ----------------------------------------------------------------------------
(setq
 org-directory "~/org/")
(defvar
  org-agenda-directory (concat org-directory "agenda/")
  "Default `org-agenda' directory.
\nAll `org-files' in this directory will be included in the agenda.")
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
 org-ellipsis " … " ; or ▾ ⤵ ⮷
 ;; ----------------------------------------------------------------------------
 ;; 4 todo states. I use categories and refile rather than more keywords.
 ;; ----------------------------------------------------------------------------
 org-todo-keywords
 '((type     "NEXT(n!/!)" "TODO(t!/!)" "|") ; Active states.
   (type "|" "HOLD(h@/!)" "DONE(d!/!)"))  ; Inactive states.
 org-priority-default ?C
 org-priority-faces                ; This affects rendering in the agenda.
 '((?A . (:slant nil :height 0.8)) ; For some reason the default is slanted.
   (?B . (:slant nil :height 0.8))
   (?C . (:slant nil :height 0.8)))
 org-list-allow-alphabetical t
 org-list-demote-modify-bullet
 '(("+" . "*")
   ("*" . "-")
   ("-" . "+"))
 org-tags-column -75 ; Minus align tags right.
 org-tag-alist
 '(("pc"       . ?c) ; c for computer.
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
 org-M-RET-may-split-line
 '((default . nil))
 org-insert-heading-respect-content t
 org-confirm-babel-evaluate nil
 org-cycle-hide-block-startup t
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
 `(("c" "Category" entry ; Back tick to ",(concat..." for code readability.
    (file ,(concat org-agenda-directory "plan.org"))
    ,(concat
      "* TODO [/] %^{Heading}\n" ; I like the cookie in front of the heading.
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
 org-reverse-note-order t ; Prepend refiles.
 org-refile-targets
 `((,(concat org-agenda-directory "agenda.org") :maxlevel . 1)
   (,(concat org-agenda-directory "plan.org")   :maxlevel . 1))
 org-agenda-files (list (concat org-directory "inbox.org")
                        org-agenda-directory) ; All org files in the directory.
 org-agenda-format-date " [%F %a] "
 org-agenda-block-separator ?﹋ ; Other options: ﹌⎺̅‾﹉
 org-agenda-span 'month
 org-agenda-custom-commands
 '(("c" "Custom agenda setup"
    ((todo
      "NEXT" ; Unblocked short tasks. Often pending scheduling and refiling.
      ((org-agenda-overriding-header "")))
     ;; Agenda supress timestamped active state items that are not current.
     (agenda
      ""
      ((org-agenda-span 'week)))
     ;; TODOs without a timestamp.
     (todo
      "TODO" ; With [/] cookies. This is typically categories.
      ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'notregexp "\\[[0-9]+/[0-9]+\\]"
          ;; or
          'timestamp))))
     (todo
      "TODO" ; Others. Not all TODOs have or should have a timestamp.
      ((org-agenda-overriding-header "") ; share heading (same block)
       (org-agenda-block-separator nil)  ; don't separate
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'regexp "\\[[0-9]+/[0-9]+\\]"
          ;; or
          'timestamp))))
     ;; Inactive states. Not in the agenda even if scheduled.
     (todo
      "HOLD" ; For third party action pending.
      ((org-agenda-overriding-header "") ; same block
       (org-agenda-block-separator nil)))
     (todo
      "DONE" ; Only prioritized.
      ((org-agenda-overriding-header "") ; same block
       (org-agenda-block-separator nil)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'notregexp org-priority-regexp))))))
   ;; Custom options for maintainance. Append to agenda with "a".
   ("d" "DONE TODOs"
    ;; For archiving done tasks.
    ((todo
      "DONE"
      ((org-agenda-overriding-header "DONE TODOs")))))
   ("u" "Untagged TODOs"
    ;; For adding tags.
    ((tags-todo
      "-{.*}"
      ((org-agenda-overriding-header "Untagged TODOs"))))))
 org-agenda-time-grid nil
 org-agenda-prefix-format
 '((agenda   . "  %-6c%-12t%?-6s")
   (timeline . "  %-6c%-12t%?-6s")
   (todo     . "  %-6c%-12e")
   (tags     . "  %-6c%-12e")
   (search   . "  %-6c%-12e"))
 org-agenda-timerange-leaders
 '("Range" "%2d/%d")
 org-agenda-scheduled-leaders
 '("⧄" "⧄%3dx")
 org-agenda-deadline-leaders
 '("⧅" "⧅%3dd" "⧅%3dx")
 org-deadline-warning-days 7
 org-agenda-skip-scheduled-if-done t
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 calendar-week-start-day 1
 org-columns-default-format "%30Item %Clocksum(Used) %Effort(Plan) %Category(Cat.) %Tags %Priority(#) %Todo(Todo)"
 org-global-properties
 '(("effort_all" . "0:05 0:10 0:15 0:20 0:30 0:45 1:00 1:30 2:00"))
 org-clock-in-switch-to-state "NEXT"
 org-clock-out-when-done t
 org-time-stamp-rounding-minutes
 '(15 15)
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
;; ----------------------------------------------------------------------------
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
    prettify-symbols-alist ; Pictograms: ⧈⧇⊡⧆⊞⊟⧄⧅⊠⟏⟎ ▸▾▴◂ ☑☐☒ ✏✎✐
    '(("[ ]"            . ?⊡)
      ("[-]"            . ?⊟)
      ("[X]"            . ?⊠)
      ("SCHEDULED:"     . ?⧄)
      ("DEADLINE:"      . ?⧅)
      ("CLOSED:"        . ?⊠)
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
;; Update "[/]" cookies with a TODO state change
;; ----------------------------------------------------------------------------
(add-hook
 'org-after-todo-state-change-hook
 (lambda ()
   (mapcar
    (lambda (buffer)
      (with-current-buffer buffer
        (org-update-statistics-cookies t)))
    (org-buffer-list)))) ; All org buffers.
;; ----------------------------------------------------------------------------
;; Insert state in `org-capture' and on `org-add-note'/`org-refile'
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
 org-habit-preceding-days 28 ; 4 weeks. I think the default is 3.
 org-habit-graph-column   60
 org-appear-autolinks t)
(require 'org-appear) ; this will load org-agenda.
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
;; Does not work on Android for some reason?
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
  ;; "<spc> #" is inspired by Emacs' "C-x #" bindings where # is a number.
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
  "¨"   '(tab-bar-close-tab-by-name              :which-key "Close tab")
  "a"   '(:ignore t                              :which-key "App")
  "aC"  '(full-calc                              :which-key "Full calc")
  "ac"  '(calc                                   :which-key "Calc")
  "as"  '(eshell                                 :which-key "Eshell")
  "au"  '(undo-tree-visualize                    :which-key "Undo tree")
  "b"   '(:ignore t                              :which-key "Buffer")
  "bb"  '(consult-buffer                         :which-key "Mini menu")
  "bd"  '(kill-this-buffer                       :which-key "Delete")
  "bi"  '(ibuffer                                :which-key "IBuffer")
  "bj"  '(next-buffer                            :which-key "Next")
  "bk"  '(previous-buffer                        :which-key "Previous")
  "bm"  '(view-echo-area-messages                :which-key "Messages")
  "bs"  '(scratch-buffer                         :which-key "Scratch")
  "c"   '(:ignore t                              :which-key "c")
  "d"   '(:ignore t                              :which-key "d")
  "e"   '(:ignore t                              :which-key "e")
  "f"   '(:ignore t                              :which-key "File")
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
  "lb"  '(eval-buffer                            :which-key "Buffer")
  "ll"  '(eval-expression                        :which-key "Expression")
  "ls"  '(eval-last-sexp                         :which-key "Sexp @point")
  "m"   '(:ignore t                              :which-key "m")
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
  "r"   '(:ignore t                              :which-key "Register")
  "rl"  '(consult-register-load                  :which-key "Load")
  "rr"  '(counsel-mark-ring                      :which-key "Mini marks")
  "rs"  '(consult-register-store                 :which-key "Store")
  "rv"  '(exchange-point-and-mark                :which-key "Visual mark")
  "s"   '(:ignore t                              :which-key "Search")
  "so"  '(consult-outline                        :which-key "Outline")
  "sO"  '(occur                                  :which-key "Occur")
  "sr"  '(query-replace                          :which-key "Replace")
  "sR"  '(query-replace-regexp                   :which-key "Rep. regex")
  "ss"  '(swiper                                 :which-key "Swiper")
  "sw"  '(eww                                    :which-key "Web (eww)")
  "t"   '(:ignore t                              :which-key "Toggle")
  "tb"  '(global-visual-line-mode                :which-key "Soft breaks")
  "tc"  '(global-centered-cursor-mode            :which-key "Vert.center")
  "tf"  '(mixed-pitch-mode                       :which-key "Font pitch")
  "tg"  '(golden-ratio-mode                      :which-key "Gold ratio")
  "th"  '(global-hl-line-mode                    :which-key "Hl line")
  "ti"  '(display-fill-column-indicator-mode     :which-key "Fill Indi.")
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
  "xp"  '(transpose-chars                        :which-key "Swap chars")
  "xu"  '(insert-char                            :which-key "Unicode")
  "xx"  '(just-one-space                         :which-key "One space")
  "xz"  '(:ignore t                              :which-key "Zoom")
  "xzg" '(global-text-scale-adjust               :which-key "Global")
  "xzl" '(text-scale-adjust                      :which-key "Local")
  "y"   '(:ignore t                              :which-key "y")
  "z"   '(:ignore t                              :which-key "z"))

;; ============================================================================
;;; Keybindings
;; ============================================================================
(bind-keys
 ;; ----------------------------------------------------------------------------
 ;; Global keys (minor mode maps will override)
 ;; ----------------------------------------------------------------------------
 :map global-map
 ("<escape>"      . keyboard-escape-quit)
 ("<Scroll_Lock>" . centered-cursor-mode)
 ("<next>"        . evil-scroll-down)
 ("<prior>"       . evil-scroll-up)
 ("C-<tab>"       . tab-next)
 ("C-S-<tab>"     . tab-previous)
 ("C-¨"           . tab-line-mode)
 ("M-p"           . consult-yank-pop) ; Paste with `kill-ring' dialog.
 ;; ----------------------------------------------------------------------------
 ;; Operator state
 ;; ----------------------------------------------------------------------------
 :map evil-operator-state-map
 ;; Bad things happen if I hit "å" in operator state without this.
 ("å"             . keyboard-escape-quit) ; Danish keyboard.
 ;; ----------------------------------------------------------------------------
 ;; Motion state (normal, visual and motion)
 ;; ----------------------------------------------------------------------------
 :map evil-motion-state-map
 ("<down>"        . evil-next-visual-line)     ; up/down navigate wrapped lines.
 ("<up>"          . evil-previous-visual-line) ; j/k respect Vim's line atoms.
 ("´"             . next-buffer)
 ("½"             . other-window)
 ("¨"             . tab-new)
 ("gc"            . evilnc-comment-operator)
 ("C-S-o"         . evil-jump-forward) ; I use "C-i" for <tab>.
 ("C-i"           . outline-cycle)     ; This make <tab> work in terminal too.
 ("<backtab>"     . outline-cycle-buffer)
 ;; Danish keyboard (some keys are not easily accessible).
 ("æ"             . forward-paragraph)   ; "}" use the equivalent evil command.
 ("Æ"             . backward-paragraph)  ; "{" use the equivalent evil command.
 ("ø"             . end-of-line)         ; "$" use the equivalent evil command.
 ("Ø"             . back-to-indentation) ; "^" use the equivalent evil command.
 ("å"             . my/org-agenda-custom)
 ("Å"             . my/org-capture-idea)
 ("C-å"           . org-cycle-agenda-files)
 ;; ----------------------------------------------------------------------------
 ;; Normal state
 ;; ----------------------------------------------------------------------------
 :map evil-normal-state-map
 ("g+"            . evil-numbers/inc-at-pt)
 ("g-"            . evil-numbers/dec-at-pt)
 ;; ----------------------------------------------------------------------------
 ;; Visual state
 ;; ----------------------------------------------------------------------------
 :map evil-visual-state-map
 ;; "S" is used by `evil-surround' in visual state. Use "C-s" for `isearch'.
 ("s"             . isearch-forward-thing-at-point) ; With the region as input.
 ;; Don't use "v" to exit visual state. <esc> or a command works.
 ("v"             . exchange-point-and-mark)
 ;; ----------------------------------------------------------------------------
 ;; Insert state
 ;; ----------------------------------------------------------------------------
 ;; I come from Emacs so I like to access some commands in insert state.
 :map evil-insert-state-map
 ("C-g"           . evil-normal-state)
 ("C-b"           . evil-backward-word-begin)
 ("C-B"           . evil-backward-WORD-begin)
 ("C-d"           . backward-kill-word) ; I use "C-w" for forward word.
 ("C-e"           . forward-word)       ; The end of the word.
 ("C-p"           . yank)
 ("M-p"           . consult-yank-pop)
 ("C-u"           . universal-argument)
 ("C-w"           . evil-forward-word-begin)
 ("C-W"           . evil-forward-WORD-begin)
 ("C-v"           . set-mark-command) ; "C-<spc>" is used by general.el.
 ("C-0"           . beginning-of-line)
 ;; Danish keyboard.
 ("C-æ"           . forward-paragraph)
 ("C-Æ"           . backward-paragraph)
 ("C-ø"           . end-of-line)
 ("C-Ø"           . back-to-indentation)
 :map org-mode-map
 ;; Danish keyboard.
 ("C-æ"           . org-next-visible-heading)
 ("C-Æ"           . org-previous-visible-heading)
 :map org-present-mode-keymap
 ("<left>"        . org-present-prev)
 ("<right>"       . org-present-next)
 ("<up>"          . org-present-beginning)
 ("<down>"        . org-present-end)
 :map ccm-map
 ("<prior>"       . nil)
 ("<next>"        . nil)
 :map corfu-map
 ("C-."           . corfu-insert-separator) ; For orderless.
 ("C-i"           . corfu-next) ; This make <tab> work in terminal too.
 ("<backtab>"     . corfu-previous)
 :map minibuffer-local-map
 ("C-."           . embark-act)
 ("M-."           . embark-dwim)
 ("<next>"        . marginalia-cycle))
;; ============================================================================
;;;; Keybindings in maps depending on evil state with `evil-define-key'
;; ============================================================================
(evil-define-key 'normal global-map
  ;; I never use Vim's substitute "s". If I need it, "cl" does the same thing.
  "s"           #'isearch-forward ; I prefer Emacs' isearch. "C-s" to repeat.
  "S"           #'isearch-forward-thing-at-point) ; This is like Vim's "*".
(evil-define-key 'normal help-mode-map
  (kbd "SPC") nil) ; Make general.el take over <spc>.
(evil-define-key 'normal dired-mode-map
  (kbd "SPC") nil  ; Make general.el take over <spc>.
  "h"           #'dired-up-directory
  "l"           #'dired-find-file
  "a"           #'dired-omit-mode           ; Like "ls -a" toggle.
  "s"           #'dired-hide-details-mode   ; Like "ls -l" toggle.
  ;; Personal swap to make "o" open files with system default program.
  "W"           #'dired-sort-toggle-or-edit ; Normally bound to "o".
  "o"           #'browse-url-of-dired-file) ; Normally bound to "W".
(evil-define-key 'normal org-mode-map
  "t"           #'org-todo            ; In operator state "t" work as in Vim.
  "T"           #'org-todo-yesterday) ; "ct." (change to ".") work as expected.
(evil-define-key 'motion org-agenda-mode-map
  (kbd "SPC") nil  ; Make general.el take over <spc>.
  (kbd "S-<left>")      #'org-agenda-earlier ; (kbd... is necessary when used.
  (kbd "S-<right>")     #'org-agenda-later
  "´"           #'next-buffer
  "a"           #'org-agenda-append-agenda
  "A"           #'org-agenda-archive-default
  "R"           #'org-agenda-refile
  "T"           #'org-agenda-todo-yesterday
  "gy"          #'org-agenda-year-view
  "sd"          #'org-agenda-deadline
  "ss"          #'org-agenda-schedule
  "n"           #'org-agenda-add-note
  ;; Danish keyboard.
  "æ"           #'org-agenda-forward-block
  "Æ"           #'org-agenda-backward-block
  "å"           #'org-agenda-columns
  "Å"           #'my/org-capture-idea)

;; ============================================================================
;;; Startup page
;; ============================================================================
(my/org-agenda-custom)
;; End of init.el
