;; -*- lexical-binding: t; -*-
;; #+title: Emacs config init.el

;; ============================================================================
;;; Fonts and faces (my themes)
;; ============================================================================
(if (find-font (font-spec :name "Ubuntu Mono"))
    (set-face-font 'default "Ubuntu Mono 18")
  ;; else
  (message "The default font is set to %s" (font-get-system-font)))
;; The variable width Ubuntu font doesn't look good with Ubuntu Mono imo.
(when (find-font (font-spec :name "Verdana"))
  (set-face-font 'variable-pitch "Verdana 18"))
;; ----------------------------------------------------------------------------
;; I use files for faces rather than variables so I see the colors when I edit.
(when (file-newer-than-file-p
       (locate-user-emacs-file "my-ansi-faces.el")
       (locate-user-emacs-file "my-ansi-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "my-ansi-faces.el")))
(when (file-newer-than-file-p
       (locate-user-emacs-file "my-faces.el")
       (locate-user-emacs-file "my-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "my-faces.el")))
(if (eq (user-uid) 0) ; Ansi colors as root.
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t)
  ;; else
  (load (locate-user-emacs-file "my-faces.elc") nil t))
;; ----------------------------------------------------------------------------
;; Additional keywords for more coloring.
(font-lock-add-keywords
 'emacs-lisp-mode
 '(;; ----------------------------------------------------------------------------
   ;; Logic.
   ("(\\(not\\|and\\|x?or\\)[\s\n]"          . (1 'font-lock-function-call-face))
   ("(\\([<>=]\\|[<>/]=\\)[\s\n]"            . (1 'font-lock-function-call-face))
   ("(\\(eq\\(\\(ua\\)?l\\)?\\)[\s\n]"       . (1 'font-lock-function-call-face))
   ("(\\(mem\\(ber\\|ql?\\)\\)[\s\n]"        . (1 'font-lock-function-call-face))
   ("(\\([^\s]+-p\\)[\s\n)]"                 . (1 'font-lock-function-call-face))
   ("(\\([be]o[bl]p\\))"                     . (1 'font-lock-function-call-face))
   ;; ----------------------------------------------------------------------------
   ;; Booleans, numbers/floats and calculation.
   ("\\(\\(\s+(*\\(nil\\|t\\)\\)+\\)[\s\n)]" . (1 'font-lock-number-face))
   ("\\(\\(\\([(\s]-\\)?\\.?[0-9]\\)+\\)[\\.:/\s\n)]" . (1 'font-lock-number-face))
   ("(\\([\\+-\\*/]\\)[\s\n]"                . (1 'font-lock-number-face))
   ("(\\(1[\\+-]\\)[\s\n]"                   . (1 'font-lock-number-face))
   ;; ----------------------------------------------------------------------------
   ;; Functions closely related to strings.
   ("(\\(concat\\)[\s\n]"                    . (1 'font-lock-string-face))
   ("(\\(format\\)[\s\n]"                    . (1 'font-lock-string-face))
   ("(\\(propertize\\)[\s\n]"                . (1 'font-lock-string-face))
   ;; ----------------------------------------------------------------------------
   ;; Other functions.
   ("(\\(cons\\)[\s\n]"                      . (1 'font-lock-keyword-face))
   ("(\\(list\\)[\s\n]"                      . (1 'font-lock-keyword-face))
   ("(\\(load\\)[\s\n]"                      . (1 'font-lock-keyword-face))
   ("[(']\\(add-[^\s]+\\)[\s\n]"             . (1 'font-lock-keyword-face))
   ("[(']\\(set-[^\s]+\\)[\s\n]"             . (1 'font-lock-keyword-face))))

;; ============================================================================
;;; Mode line
;; ============================================================================
(setq-default ; `mode-line-format' is buffer local so `setq-default'.
 mode-line-format
 ;; ----------------------------------------------------------------------------
 ;; `ace-window-display-mode' start the mode line with a window number.
 '((:eval ; Eval and list everything.
    (list
     ;; ----------------------------------------------------------------------------
     ;; Indicators.
     (if (and (eq (user-uid) 0) ; I link ~/.emacs.d in /root.
              (mode-line-window-selected-p))
         (propertize
          "#"
          'help-echo "Root access"
          'face 'warning) ; Emphasize this.
       ;; else
       " ")
     mode-line-modified ; Writable and modified.
     (if (buffer-narrowed-p)
         (propertize
          "="
          'help-echo "Narrowed, mouse-1: Widen"
          'mouse-face 'mode-line-highlight
          'local-map (make-mode-line-mouse-map
                      'mouse-1 #'mode-line-widen))
       ;; else
       " ")
     " "
     ;; ----------------------------------------------------------------------------
     ;; Buffer name.
     mode-line-buffer-identification
     " "
     ;; ----------------------------------------------------------------------------
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
     ;; ----------------------------------------------------------------------------
     (if (mode-line-window-selected-p)
         (list
          ;; Version control.
          (if vc-mode
              (replace-regexp-in-string
               "\\` Git" ""
               vc-mode)
            ;; else
            "")
          " "
          ;; Display `global-mode-string' used by e.g. `display-time-mode'.
          mode-line-misc-info
          ;; Gap for alignment.
          (propertize
           " "
           'display '((space
                       :align-to (- (+ right right-fringe right-margin) 7)))
           'face    'mode-line-inactive)
          ;; Cursor position.
          (if display-line-numbers-mode
              (list
               (propertize
                "%3c "
                'help-echo "Column number, mouse-1: Toggle line numbers"
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map
                            'mouse-1 #'display-line-numbers-mode))
               '(-3 "%o")) ; Show "Bot" rather than "Bottom".
            ;; else
            (list
             (propertize
              "%3c,"
              'help-echo "Column number, mouse-1: Toggle line numbers"
              'mouse-face 'mode-line-highlight
              'local-map (make-mode-line-mouse-map
                          'mouse-1 #'display-line-numbers-mode))
             (propertize
              "%3l "
              'help-echo "Line number, mouse-1: Toggle the scroll bar"
              'mouse-face 'mode-line-highlight
              'local-map (make-mode-line-mouse-map
                          'mouse-1 #'scroll-bar-mode)))))
       ;; else
       (list
        ;; Gap for alignment.
        (propertize
         " "
         'display '((space
                     :align-to (- (+ right right-fringe right-margin) 4))))
        '(-3 "%o")))))))

;; ============================================================================
;;; Custom functions and variables
;; ============================================================================
;; Misc variables.
(defvar my/hl-line t
  "Track what `hl-line-mode' was, when it's temporarily suspended.")
(defvar org-agenda-directory nil ; Set latter.
  "Default `org-agenda' directory.
\nInclude this directory in the list `org-agenda-files'.
All org files in the directory will be in the agenda.")
;; ----------------------------------------------------------------------------
;; Open init files.
(defun my/find-init-file ()
  "Open configuration file init.el."
  (interactive)
  (find-file (locate-user-emacs-file "early-init.el"))
  (find-file (locate-user-emacs-file "init.el")))
;; ----------------------------------------------------------------------------
;; Open agenda and plan file.
(defun my/find-agenda-file ()
  "Open my agenda file agenda.org.
\nThese agenda files are usually opened in buffers by `org-agenda'.
The function will organize the `buffer-list' and focus agenda.org."
  (interactive)
  (find-file (concat org-agenda-directory "/plan.org"))
  (find-file (concat org-agenda-directory "/agenda.org")))
;; ----------------------------------------------------------------------------
;; Open note and date file.
(defun my/find-note-file ()
  "Open my notes file note.org.
\nThese agenda files are usually opened in buffers by `org-agenda'.
The function will organize the `buffer-list' and focus note.org."
  (interactive)
  (find-file (concat org-agenda-directory "/date.org"))
  (find-file (concat org-agenda-directory "/note.org")))
;; ----------------------------------------------------------------------------
;; Custom agenda.
(defun my/org-agenda-custom ()
  "Custom agenda with NEXT items, this weeks agenda and TODO/HOLD items."
  (interactive)
  (delete-other-windows)
  (org-agenda nil "c")
  (unless (eq (char-after) ?\s) ; If on a heading, not a space.
    (org-agenda-goto-today)))
;; ----------------------------------------------------------------------------
;; Capture idea.
(defun my/org-capture-idea ()
  "Capture an idea to inbox.org and make it a NEXT item."
  (interactive)
  (org-capture nil "i")
  (org-save-all-org-buffers)
  (when (equal (buffer-name) "*Org Agenda*")
    (org-agenda-redo) ; Might turn `hl-line-mode' off for some reason?!?
    (when my/hl-line  ; Hack to often turn it back on when it was on.
      (global-hl-line-mode 1))
    (goto-char (point-min)))) ; Jump to the newly created NEXT item.
;; ----------------------------------------------------------------------------
;; Toggle my faces (theme).
(defun my/toggle-faces ()
  "Toggle my two default faces.
\nThe faces are loaded from my-faces.elc and my-ansi-faces.elc respectively."
  (interactive)
  (if (equal (face-background 'default) "#000")
      (load (locate-user-emacs-file "my-faces.elc") nil t)
    ;; else
    (load (locate-user-emacs-file "my-ansi-faces.elc") nil t))
  (message "Toggled to the faces with \"%s\" background" (face-background 'default)))
;; ----------------------------------------------------------------------------
;; Toggle maximized window.
(defvar my/window-configuration nil
  "Track window configuration, when it's suspended.")
(defun my/toggle-window-maximize ()
  "Toggle one window and the window configuration from last maximize."
  (interactive)
  (if (one-window-p)
      (when my/window-configuration
        (set-window-configuration my/window-configuration)
        (message "Window configuration restored"))
    (setq
     my/window-configuration (current-window-configuration))
    (delete-other-windows)
    (message "Window maximized")))
;; ----------------------------------------------------------------------------
;; Four window setup.
(defun my/4-windows ()
  "Four windows, three on the right and the left focused.
  \nThis nice window setup is surprisingly hard to create without this function.
\nThe content of window 1 will be unchanged. By default the day-agenda goes in
window 2, Messages in window 3 and window 1's `next-buffer' in window 4.
An existing \"*Org Agenda*\" buffer might change the configuration a bit."
  (interactive)
  (delete-other-windows)
  (when (equal (buffer-name) "*Org\sAgenda*") ; Would affect `org-agenda-list'.
    (org-agenda-Quit)
    (ibuffer))
  (split-window-right)
  (other-window 1)
  (next-buffer)                   ; 4th window.
  (when (equal (buffer-name) "*Org\sAgenda*")
    (switch-to-buffer "*scratch*"))
  (split-window-below)
  (switch-to-buffer "*Messages*") ; 3rd window.
  (split-window-below)
  (org-agenda-list)               ; 2nd window.
  (other-window -1)               ; Back to 1st window.
  (message "4 window setup"))
;; ----------------------------------------------------------------------------
;; Ace window swap.
(defun my/ace-swap-window ()
  "Swap two window contents (prompt if 3+). Keep focus on the current window.
\nThe normal `ace-swap-window' swap two windows, but stays with the current buffer
and focus the window you swapped to."
  (interactive)
  (ace-swap-window)
  (aw-flip-window)
  (message "Windows swapped"))
;; ----------------------------------------------------------------------------
;; Magit stage and commit.
(defun my/magit-stage-all-and-commit (message)
  "Stage and commit everything.
  \nWill respect what is configured to be ignored."
  (interactive "sCommit Message: ")
  (save-some-buffers t)
  (call-process-shell-command
   (format "git commit -a -m \"%s\"" message) nil nil)
  (vc-refresh-state))
;; ----------------------------------------------------------------------------
;; Save and quit.
(defun my/save-all-kill-emacs-no-prompt ()
  "Save all and quit without a prompt.
  \nUse something like `auto-save-mode' and `backup-each-save' to make this
less risky."
  (interactive)
  (save-some-buffers t)
  (when (file-newer-than-file-p
         (locate-user-emacs-file "early-init.el")
         (locate-user-emacs-file "early-init.elc"))
    (byte-compile-file (locate-user-emacs-file "early-init.el")))
  (when (file-newer-than-file-p
         (locate-user-emacs-file "init.el")
         (locate-user-emacs-file "init.elc"))
    (when (file-exists-p (locate-user-emacs-file "init.elc"))
      (copy-file (locate-user-emacs-file "init.elc")
                 (locate-user-emacs-file "init.elc~") t)) ; Overwrite.
    (byte-compile-file (locate-user-emacs-file "init.el")))
  (kill-emacs))

;; ============================================================================
;;; Vanilla stuff
;; ============================================================================
;; Variables.
(setq-default ; Buffer local variables.
 indent-tabs-mode nil ; No tabulator character pollution please.
 display-line-numbers-width 3
 fill-column 79)
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
 delete-by-moving-to-trash t
 trash-directory "~/.local/share/Trash/files"
 custom-file "/dev/null"           ; Don't deal with custom.el.
 truncate-partial-width-windows 59 ; Only wrap long lines.
 recentf-exclude
 '("\\`~/\\.emacs\\.d/.*\\.el[cd]?\\'" ; Elisp config and data files.
   "\\`~/\\.emacs\\.d/diary\\'"
   "\\`~/org/agenda/.*\\.org\\'"       ; Agenda files. These are in buffers.
   "\\`~/org/inbox\\.org\\'"
   "\\`~/.*\\.pdf\\'"
   ".*\\.el\\.gz\\'")                  ; Source files browsed via Emacs help.
 ;; ----------------------------------------------------------------------------
 ;; Dired.
 dired-kill-when-opening-new-dired-buffer t
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-listing-switches "-agho --group-directories-first"
 dired-omit-files "\\`[#\\.].*\\'" ; "a" will toggle.
 dired-omit-verbose  nil
 auto-revert-verbose nil
 global-auto-revert-non-file-buffers t ; Update dired buffer when files change.
 ;; ----------------------------------------------------------------------------
 ;; Ediff.
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 ;; ----------------------------------------------------------------------------
 ;; Incremental search (I use Emacs' `isearch' rather than Vim tools to search).
 search-whitespace-regexp ".*?"
 isearch-lazy-count t
 lazy-count-prefix-format "%s/%s "
 lazy-count-suffix-format nil
 ;; ----------------------------------------------------------------------------
 ;; Tab bar.
 tab-bar-show                  1
 tab-bar-close-button-show     nil
 tab-bar-new-button            nil
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
 ;; Display.
 display-time-format "[%Y-%m-%d %a %H:%M]" ; The org timestamp format.
 display-line-numbers-type 'relative ; Best default for wrapped lines.
 ;; Focus popups. The default is to not switch focus to them.
 display-buffer-alist
 '(("\\`\\*Org\sAgenda\\*\\'"
    (display-buffer-same-window)
    (body-function . select-window))
   ("\\`\s?\\*.*\\*\s?\\'"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (body-function . select-window))))
;; ============================================================================
;;;; Native hooks
;; ============================================================================
(add-hook 'after-save-hook  #'vc-refresh-state)        ; Version control.
(add-hook 'dired-mode-hook  #'dired-omit-mode)         ; Toggle rebound to "a".
(add-hook 'dired-mode-hook  #'dired-hide-details-mode) ; Toggle rebound to "s".
(add-hook 'text-mode-hook   #'visual-line-mode)        ; Line breaks at words.
(add-hook 'prog-mode-hook   #'outline-minor-mode)      ; Cycle with "S-<tab>".
;; Visual line numbers when `outline-cycle-buffer'.
(add-hook 'outline-view-change-hook ; Why is this considered obsolete?
          (lambda ()
            (when (and display-line-numbers
                       outline-minor-mode)
              ;; With `whitespace-cleanup' the `char-after' the `buffer-size'-
              ;; position is a single "C-j" (new line char) which ironically
              ;; is always considered visible. `buffer-size'-1 is not a "C-j".
              (if (outline-invisible-p (1- (buffer-size)))
                  (setq
                   display-line-numbers 'visual)     ; For folded lines.
                ;; else
                (setq
                 display-line-numbers 'relative))))) ; For wrapped lines.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message
             "Emacs ready in %s with %d garbage collections."
             (format
              "%.1f seconds"
              (float-time (time-subtract after-init-time before-init-time)))
             gcs-done)))
;; ============================================================================
;;;; Native minor modes
;; ============================================================================
(recentf-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(auto-save-mode 1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

;; ============================================================================
;;; Package.el
;; ============================================================================
;; I use the good old package.el manager rather than a newer alternative.
;; I trust packages to defer sensibly and don't want to micro manage.
(setq
 load-prefer-newer t ; Use .el if newer than .elc.
 package-archives
 '(("elpa"         . "https://elpa.gnu.org/packages/")
   ("melpa"        . "https://melpa.org/packages/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
   ("org"          . "https://orgmode.org/elpa/"))
 package-archive-priorities
 '(("elpa"  . 2)  ; Prefer older versions from elpa.
   ("melpa" . 1)) ; The remaining archives have priority 0.
 package-selected-packages
 '(evil evil-collection evil-nerd-commenter evil-surround evil-numbers evil-org org-superstar org-appear org-present magit cape corfu nerd-icons-corfu nerd-icons nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion avy vertico marginalia orderless consult counsel embark embark-consult lsp-mode rainbow-delimiters rainbow-mode recursive-narrow centered-cursor-mode golden-ratio ace-window transpose-frame mixed-pitch indent-guide keycast undo-tree flycheck writegood-mode auto-package-update backup-each-save general))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages t)
;; ============================================================================
;;;; Update, backup and undo
;; ============================================================================
(require 'auto-package-update)
(setq
 auto-package-update-interval 30
 auto-package-update-hide-results t)
(auto-package-update-maybe)
(require 'backup-each-save)
(setq
 backup-each-save-mirror-location "~/.backup-emacs-saved")
(add-hook 'after-save-hook #'backup-each-save)
;; ----------------------------------------------------------------------------
;; Magit.
(require 'magit)
(with-eval-after-load 'evil
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (evil-set-initial-state 'magit-log-edit-mode 'insert))
(add-hook 'magit-run-post-commit-hook #'vc-refresh-state)
;; ----------------------------------------------------------------------------
;; Undo.
(require 'undo-tree)
(setq
 undo-tree-visualizer-timestamps t
 undo-tree-auto-save-history t
 undo-tree-visualizer-buffer-name "*undo-tree*") ; The default include a space.
(global-undo-tree-mode 1)
;; ============================================================================
;;;; Buffers, completion and windows
;; ============================================================================
(require 'avy)
(setq
 avy-timeout-seconds 1) ; I'm slow.
(require 'recursive-narrow)
(put 'narrow-to-region 'disabled nil) ; Disable a warning somehow?
(require 'mixed-pitch)
(dolist (face '(org-date
                org-priority
                org-special-keyword
                org-tag
                org-todo))
  (add-to-list 'mixed-pitch-fixed-pitch-faces face))
;; ----------------------------------------------------------------------------
;; Mini buffer.
(require 'vertico)
(setq
 vertico-resize nil)
(vertico-mode 1)
(with-eval-after-load 'vertico
  (require 'marginalia)
  (marginalia-mode 1))
;; Tidy typing directories:
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(require 'orderless) ; Fuzzy completions.
(setq
 completion-styles
 '(orderless))
(require 'embark) ; I don't really use this yet.
;; ----------------------------------------------------------------------------
;; Completion.
(require 'corfu)
(setq
 corfu-auto t
 corfu-auto-delay .1
 corfu-auto-prefix 3
 corfu-count 5
 corfu-quit-at-boundary 'separator)
(global-corfu-mode 1)
(corfu-echo-mode 1)
(corfu-history-mode 1)
;; ----------------------------------------------------------------------------
;; Windows.
(require 'transpose-frame)
(require 'ace-window)
(ace-window-display-mode 1)
(require 'golden-ratio)
(setq
 golden-ratio-exclude-buffer-regexp
 '("\\`\s?\\*undo-tree.*\\*\\'"
   "\\`\\*Ediff\sControl\sPanel\\*\\'"))
(golden-ratio-mode 1)
(require 'centered-cursor-mode)
(add-to-list 'ccm-ignored-commands 'mwheel-scroll) ; Enable mouse wheel scroll.
(global-centered-cursor-mode 1)                    ; Toggle with `Scroll_Lock'.
;; ============================================================================
;;;; Help and guides
;; ============================================================================
(require 'consult) ; Combine functionality (e.g. buffers + recentf).
(require 'counsel)
(require 'keycast)
(keycast-tab-bar-mode 1)
;; ----------------------------------------------------------------------------
;; Writing tips.
(require 'flycheck)
(add-hook 'text-mode-hook #'flyspell-mode)
(require 'writegood-mode)
;; ----------------------------------------------------------------------------
;; Color guide.
(require 'lsp-mode)
(require 'indent-guide)
(add-hook 'prog-mode-hook #'indent-guide-mode)
(require 'rainbow-delimiters)
(setq
 rainbow-delimiters-max-face-count 3)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook  #'rainbow-delimiters-mode)
(require 'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
;; ----------------------------------------------------------------------------
;; Thumbnails.
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
;;; Evil.el
;; ============================================================================
(setq
 evil-undo-system 'undo-tree ; Set these variables before loading evil!
 evil-want-keybinding nil    ; `evil-collection' need this.
 evil-want-integration t)
(require 'evil)
;; ----------------------------------------------------------------------------
;; Cursor colors. I combine f and 0 to indicate the evil state.
(setq
 evil-motion-state-cursor   '(box        "#00f")
 evil-normal-state-cursor   '(box        "#0f0")
 evil-operator-state-cursor '(box        "#f00")
 evil-insert-state-cursor   '((bar  . 4) "#ff0")
 evil-emacs-state-cursor    '((bar  . 4) "#f0f")
 evil-replace-state-cursor  '((hbar . 4) "#0ff")
 evil-visual-state-cursor   '(hollow     "#fff")
 ;; Make <tab> work in terminal:
 evil-want-C-i-jump nil) ; "C-i" is <tab>. I rebind jump forward to "C-S-o".
(evil-mode 1)
;; ============================================================================
;;;; Addons
;; ============================================================================
(require 'evil-collection)
(evil-collection-init)
(require 'evil-surround)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
(global-evil-surround-mode 1)
(require 'evil-nerd-commenter)
(require 'evil-numbers)
(require 'evil-org)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(add-hook 'org-mode-hook
          (lambda ()
            (evil-org-mode 1)))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
;; ============================================================================
;;;; Hooks to suspend `hl-line-mode'
;; ============================================================================
;; Suspend `hl-line' if it's on, so `region' can use the same face.
(add-hook 'evil-visual-state-entry-hook
          (lambda ()
            (setq
             my/hl-line global-hl-line-mode) ; Preserve `hl-line' on exit.
            (global-hl-line-mode 0)))
(add-hook 'evil-visual-state-exit-hook
          (lambda ()
            (when my/hl-line
              (global-hl-line-mode 1))))
;; ----------------------------------------------------------------------------
;; "Input" states suspend the `hl-line' and have absolute line numbers.
(dolist (hook '(evil-insert-state-entry-hook
                evil-replace-state-entry-hook
                evil-emacs-state-entry-hook))
  (add-hook hook
            (lambda ()
              (when display-line-numbers
                (setq
                 display-line-numbers t)) ; No j/k navigation.
              (setq
               my/hl-line global-hl-line-mode)
              (global-hl-line-mode 0))))
(dolist (hook '(evil-insert-state-exit-hook
                evil-replace-state-exit-hook
                evil-emacs-state-exit-hook))
  (add-hook hook
            (lambda ()
              (when display-line-numbers
                (setq
                 display-line-numbers 'relative)) ; Back to my default.
              (when my/hl-line
                (global-hl-line-mode 1))
              (whitespace-cleanup)))) ; `whitespace-cleanup' on exit.
;; ----------------------------------------------------------------------------
;; Don't `whitespace-cleanup' before save while in an "input" state.
(add-hook 'before-save-hook
          (lambda ()
            (unless (memq evil-state '(insert replace emacs))
              (whitespace-cleanup))))
;; ============================================================================
;;;; `evil-cursor-between-mode' (my controversial sacrilege)
;; ============================================================================
;; I use Emacs' cursor between characters model for cursor positioning in
;; `evil-mode' instead of Vim `normal-state's cursor on characters model.
;; ----------------------------------------------------------------------------
;; <shift> just like <ctrl> is a layer and I try to minimize the use of layers.
;; "a"/"A", "o"/"O" and "p"/"P" is swapped for less use of capital bindings.
;; Just use "p" to paste and "o", "I/i/a" or "c" to enter insert state.
(when (file-newer-than-file-p
       (locate-user-emacs-file "evil-cursor-between-mode.el")
       (locate-user-emacs-file "evil-cursor-between-mode.elc"))
  (byte-compile-file (locate-user-emacs-file "evil-cursor-between-mode.el")))
(load (locate-user-emacs-file "evil-cursor-between-mode.elc") nil t)
;; (require 'evil-cursor-between-mode)
;; (evil-cursor-between-mode 1) ; I do this in the file to avoid a warning.

;; ============================================================================
;;; Org.el
;; ============================================================================
;; Create `org-agenda' directories and basic files unless they exist.
(setq
 org-directory "~/org/" ; I prefer a trailing slash on directory names.
 org-agenda-directory (concat org-directory "agenda/")) ; Defined earlier.
(unless (file-exists-p org-directory)
  (make-directory org-directory))
(unless (file-exists-p org-agenda-directory)
  (make-directory org-agenda-directory))
;; Create empty capture and refile targets.
(unless (file-exists-p (concat org-directory "archive.org"))
  (make-empty-file (concat org-directory "archive.org")))
(unless (file-exists-p (concat org-directory "inbox.org"))
  (make-empty-file (concat org-directory "inbox.org")))
(unless (file-exists-p (concat org-agenda-directory "note.org"))
  (make-empty-file (concat org-agenda-directory "note.org")))
;; Create plan.org with a header.
(unless (file-exists-p (concat org-agenda-directory "plan.org"))
  (find-file (concat org-agenda-directory "plan.org"))
  (insert
   (concat
    "#+title: Projects\n"
    "#+startup: content hideblocks\n"
    "\n"))
  (save-buffer)
  (switch-to-buffer "*scratch*"))
;; Create agenda.org with two refile targets.
(unless (file-exists-p (concat org-agenda-directory "agenda.org"))
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
 org-ellipsis " … " ; ⤵⮷▾
 org-todo-keywords ; I use categories and refile rather than more keywords.
 '((type     "NEXT(n!/!)" "TODO(t!/!)" "|") ; Active states.
   (type "|" "HOLD(h@/!)" "DONE(d!/!)"))  ; Inactive states.
 org-priority-default ?C
 org-priority-faces                ; This affects rendering in the agenda.
 '((?A . (:slant nil :height .8)) ; For some reason the default is slanted.
   (?B . (:slant nil :height .8))
   (?C . (:slant nil :height .8)))
 org-list-allow-alphabetical t
 org-list-demote-modify-bullet
 '(("+" . "*")
   ("*" . "-")
   ("-" . "+"))
 org-tags-column -75 ; The minus align tags right.
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
 ;; Logbook.
 org-log-into-drawer 'logbook
 org-log-done        'time
 org-log-refile      'time
 org-log-reschedule  'time
 org-log-note-headings
 '((state       . "State %6s from %-9S %t") ; Align with capture.
   (note        . "Note                        %t")
   (refile      . "Refiled                     %t")
   (done        . "Closing note                %t")
   (clock-out   . "Timer stopped               %t")
   (reschedule  . "Rescheduled                 %t from %S")
   (delschedule . "Unscheduled                 %t, was %S")
   (redeadline  . "New deadline                %t from %S")
   (deldeadline . "Deadline removed            %t, was %S"))
 ;; ----------------------------------------------------------------------------
 ;; Export.
 org-html-postamble      nil
 org-latex-title-command nil
 org-export-with-smart-quotes t
 org-export-backends
 '(ascii latex beamer texinfo html odt md org)
 org-file-apps
 '(("\\.docx\\'"    . default)
   ("\\.mm\\'"      . default)
   ("\\.x?html?\\'" . default)
   ("\\.pdf\\'"     . "evince %s")
   (directory       . emacs)
   (auto-mode       . emacs))
 ;; ----------------------------------------------------------------------------
 ;; Diary. "D" in agenda to toggle `org-agenda-include-diary'.
 holiday-bahai-holidays    nil
 holiday-hebrew-holidays   nil
 holiday-islamic-holidays  nil
 holiday-oriental-holidays nil
 holiday-general-holidays  nil
 holiday-other-holidays ; Custom diary.
 '((holiday-float  5  0  2 "Mors dag")
   (holiday-float 12  0 -5 "1. søndag i advent")
   (holiday-fixed  3  8    "Kvindernes kamp dag")
   (holiday-fixed  3 14    "π dag")
   (holiday-fixed  4  1    "Aprilsnar")
   (holiday-fixed  5  1    "Arbejdernes kamp dag")
   (holiday-fixed  6  5    "Grundlovs og fars dag")
   (holiday-fixed  6 23    "Sanct Hans")
   (holiday-fixed 12 24    "Juleaften")))
;; ============================================================================
;;;; Agenda
;; ============================================================================
;; System to organize tasks and suppress out of date information.
(setq
 org-agenda-window-setup 'current-window
 org-archive-location (concat org-directory "archive.org::* Archive")
 org-reverse-note-order t ; Prepend refiles.
 org-refile-targets
 `((,(concat org-agenda-directory "agenda.org") :maxlevel . 1)
   (,(concat org-agenda-directory "plan.org")   :maxlevel . 1))
 org-agenda-files (list org-agenda-directory ; All org files in the directory.
                        (concat org-directory "inbox.org"))
 calendar-week-start-day 1
 org-agenda-format-date " [%F %a] "
 org-agenda-block-separator ?﹋ ; ﹌⎺̅‾﹉
 org-agenda-span 'day
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
 org-columns-default-format "%30Item %Clocksum(Used) %Effort(Plan) %Category(Cat.) %Tags %Priority(#) %Todo(Todo)"
 org-global-properties
 '(("effort_all" . "0:05 0:10 0:15 0:20 0:30 0:45 1:00 1:30 2:00"))
 org-clock-in-switch-to-state "NEXT"
 org-clock-out-when-done t
 org-time-stamp-rounding-minutes
 '(15 15))
(setq
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
      "TODO" ; Sort [/] cookies on top. This is typically categories.
      ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'notregexp "\\[[0-9]+/[0-9]+\\]"
          ;; or
          'timestamp))))
     (todo
      "TODO" ; Others. Not all TODOs have or should have a timestamp.
      ((org-agenda-overriding-header "") ; Share heading (same block).
       (org-agenda-block-separator nil)  ; Don't separate.
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'regexp "\\[[0-9]+/[0-9]+\\]"
          ;; or
          'timestamp))))
     ;; Inactive states. Not in the agenda even if scheduled.
     (todo
      "HOLD" ; For third party action pending.
      ((org-agenda-overriding-header "") ; Same block.
       (org-agenda-block-separator nil)))
     (todo
      "DONE" ; Only prioritized.
      ((org-agenda-overriding-header "") ; Same block.
       (org-agenda-block-separator nil)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'notregexp org-priority-regexp))))))
   ;; Custom options. Append to agenda with "a".
   ("f" "Fortnight agenda"
    ((agenda
      ""
      ((org-agenda-span 14)
       (org-agenda-start-with-log-mode t)))))
   ("d" "DONE TODOs"
    ;; For archiving done tasks.
    ((todo
      "DONE"
      ((org-agenda-overriding-header "DONE TODOs")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'regexp org-priority-regexp))))))
   ("u" "Untagged TODOs"
    ;; For adding tags.
    ((tags-todo
      "-{.*}"
      ((org-agenda-overriding-header "Untagged TODOs")))))))
;; ============================================================================
;;;; Capture
;; ============================================================================
(setq
 org-capture-templates
 `(("c" "Category" entry ; Back tick (`) to ",(concat..." for code readability.
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
;;;; Addons
;; ============================================================================
;; Bullets.
(require 'org-superstar)
(setq
 org-superstar-headline-bullets-list
 '(?⓪ ?① ?② ?③ ?ⓧ) ; ④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳Ⓧ
 org-superstar-cycle-headline-bullets nil)
(add-hook 'org-mode-hook #'org-superstar-mode)
;; ----------------------------------------------------------------------------
;; Habits.
(require 'org-habit)
(require 'org-appear)
(setq
 org-habit-preceding-days 28
 org-habit-graph-column   60
 org-appear-autolinks t)
(add-to-list 'org-modules 'org-habit)
(add-hook 'org-mode-hook #'org-appear-mode)
;; ----------------------------------------------------------------------------
;; Present.
(require 'org-present)
(setq-local
 org-blank-before-new-entry
 '((heading . t)
   (plain-list-item . nil)))
(with-eval-after-load 'evil
  (evil-collection-org-present-setup))
;; ----------------------------------------------------------------------------
;; A pictogram is often better than a word.
(add-hook 'org-mode-hook
          (lambda ()
            (setq
             prettify-symbols-alist ; ⧈⧇⊡⧆⊞⊟⧄⧅⊠⟏⟎ ▸▾▴◂ ☑☐☒ ✏✎✐
             '(("[ ]"            . ?⊡)
               ("[-]"            . ?⊟)
               ("[X]"            . ?⊠)
               ("SCHEDULED:"     . ?⧄)
               ("DEADLINE:"      . ?⧅)
               ("CLOSED:"        . ?⊠)
               (":PROPERTIES:"   . ?⚙) ; Settings.
               (":LOGBOOK:"      . ?☰) ; Meta data.
               ("CLOCK:"         . ?–) ; Like other items in the logbook.
               (":END:"          . ?▴)
               ("#+begin_export" . ?▾)
               ("#+end_export"   . ?▴)
               ("#+begin_src"    . ?▾)
               ("#+end_src"      . ?▴))
             prettify-symbols-unprettify-at-point t)
            (prettify-symbols-mode 1)))
;; ----------------------------------------------------------------------------
;; `rainbow-delimiters-mode' don't like if "<"/">" is used for other stuff.
;; No nested rainbow colors for "<"/">". Active dates use the `org-date' face.
(modify-syntax-entry ?< "_" org-mode-syntax-table)
(modify-syntax-entry ?> "_" org-mode-syntax-table)
;; ----------------------------------------------------------------------------
;; Update "[/]" cookies in all open org buffers after a TODO state change.
(add-hook 'org-after-todo-state-change-hook
          (lambda ()
            (mapcar
             (lambda (buffer)
               (with-current-buffer buffer
                 (org-update-statistics-cookies t)))
             (org-buffer-list))))
;; ----------------------------------------------------------------------------
;; Insert state in "C-c C-c" buffers.
(with-eval-after-load 'evil
  (add-hook 'org-capture-mode-hook     #'evil-insert-state)
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state))

;; ============================================================================
;;; General.el
;; ============================================================================
;; Does not work on Android for some reason? I should make my own maps.
(setq
 which-key-idle-delay 1)
(which-key-mode 1)
(require 'general)
(general-evil-setup t)
(general-auto-unbind-keys t)
(general-create-definer my/set-leader-keys
  :keymaps '(motion insert emacs)
  :prefix "SPC"
  :global-prefix "C-SPC") ; Visual state set the mark.
(my/set-leader-keys
  ""    nil
  "SPC" '(counsel-M-x                             :which-key "M-x")
  ;; Toggle 2 top buffers:
  "TAB" '(mode-line-other-buffer                  :which-key "Toggle buf")
  ;; "<spc> #" is inspired by Emacs' "C-x #" bindings where # is a number.
  "0"   '(delete-window                           :which-key "Del window")
  "1"   '(my/toggle-window-maximize               :which-key "Maximize")
  "2"   '(split-window-below                      :which-key "Below")
  "3"   '(split-window-right                      :which-key "Right")
  "4"   '(my/4-windows                            :which-key "Four")
  "5"   '(my/ace-swap-window                      :which-key "Swap")
  "6"   '(ace-window                              :which-key "Select")
  "7"   '(transpose-frame                         :which-key "Transpose")
  "8"   '(rotate-frame-anticlockwise              :which-key "Rotate")
  "9"   '(other-window-prefix                     :which-key "Prefix")
  "a"   '(:ignore t                               :which-key "App")
  "aC"  '(full-calc                               :which-key "Full calc")
  "ac"  '(calc                                    :which-key "Calc")
  "as"  '(eshell                                  :which-key "Eshell")
  "au"  '(undo-tree-visualize                     :which-key "Undo tree")
  "b"   '(:ignore t                               :which-key "Buffer")
  "bb"  '(consult-buffer                          :which-key "Mini menu")
  "bd"  '(kill-current-buffer                     :which-key "Delete")
  "bi"  '(ibuffer                                 :which-key "IBuffer")
  "bj"  '(next-buffer                             :which-key "Next")
  "bk"  '(previous-buffer                         :which-key "Previous")
  "bm"  '(view-echo-area-messages                 :which-key "Messages")
  "bs"  '(scratch-buffer                          :which-key "Scratch")
  "c"   '(:ignore t                               :which-key "c")
  "d"   '(:ignore t                               :which-key "d")
  "e"   '(:ignore t                               :which-key "e")
  "f"   '(:ignore t                               :which-key "File")
  "fS"  '(save-some-buffers                       :which-key "Save all")
  "fa"  '(my/find-agenda-file                     :which-key "Agenda")
  "fd"  '(dired-jump                              :which-key "Dired")
  "fe"  '(ediff-files                             :which-key "Ediff")
  "ff"  '(counsel-find-file                       :which-key "Find")
  "fi"  '(my/find-init-file                       :which-key "Init")
  "fm"  '(consult-recent-file                     :which-key "Mini recent")
  "fn"  '(my/find-note-file                       :which-key "Notes")
  "fp"  '(find-file-at-point                      :which-key "At point")
  "fr"  '(recentf-open-files                      :which-key "Recent")
  "fs"  '(basic-save-buffer                       :which-key "Save")
  "fw"  '(write-file                              :which-key "Save as")
  "g"   '(:ignore t                               :which-key "Git")
  "gb"  '(magit-blame                             :which-key "Blame")
  "gc"  '(my/magit-stage-all-and-commit           :which-key "Commit")
  "gg"  '(magit                                   :which-key "Magit")
  "gs"  '(magit-status                            :which-key "Status")
  "h"   '(:ignore t                               :which-key "Help")
  "hC"  '(describe-command                        :which-key "Command")
  "hF"  '(counsel-describe-face                   :which-key "Face")
  "hK"  '(counsel-descbinds                       :which-key "Bindings")
  "hc"  '(describe-char                           :which-key "Char")
  "hf"  '(counsel-describe-function               :which-key "Function")
  "hk"  '(describe-key                            :which-key "Key")
  "hm"  '(describe-mode                           :which-key "Mode")
  "hv"  '(counsel-describe-variable               :which-key "Variable")
  "i"   '(:ignore t                               :which-key "Insert")
  "j"   '(avy-goto-char-timer                     :which-key "Jump")
  "k"   '(:ignore t                               :which-key "k")
  "l"   '(:ignore t                               :which-key "Lisp")
  "lb"  '(eval-buffer                             :which-key "Buffer")
  "le"  '(eval-expression                         :which-key "Expression")
  "ll"  '(eval-last-sexp                          :which-key "Last sexp")
  "lp"  '(eval-print-last-sexp                    :which-key "Print result")
  "lr"  '(elisp-eval-region-or-buffer             :which-key "Region")
  "m"   '(:ignore t                               :which-key "m")
  "n"   '(:ignore t                               :which-key "Narrow")
  "nf"  '(narrow-to-defun                         :which-key "Function")
  "nn"  '(recursive-narrow-or-widen-dwim          :which-key "Dwim")
  "no"  '(org-narrow-to-subtree                   :which-key "Org tree")
  "nr"  '(narrow-to-region                        :which-key "Region")
  "nw"  '(widen                                   :which-key "Widen")
  "o"   '(:ignore t                               :which-key "Org")
  "o."  '(org-time-stamp                          :which-key "Timestamp")
  "oA"  '(org-archive-subtree-default             :which-key "Archive")
  "oE"  '(org-latex-export-to-pdf                 :which-key "Latex pdf")
  "oF"  '(org-agenda-file-to-front                :which-key "Agenda file")
  "oG"  '(org-goto                                :which-key "Goto")
  "oI"  '(org-clock-in                            :which-key "Clock in")
  "oL"  '(org-store-link                          :which-key "Store link")
  "oO"  '(org-clock-out                           :which-key "Clock out")
  "oP"  '(org-present                             :which-key "Present")
  "oR"  '(org-refile                              :which-key "Refile")
  "oS"  '(org-sort                                :which-key "Sort")
  "oT"  '(orgtbl-mode                             :which-key "Tables")
  "oa"  '(org-agenda                              :which-key "Agenda")
  "ob"  '(org-insert-structure-template           :which-key "Block")
  "oc"  '(org-capture                             :which-key "Capture")
  "od"  '(org-deadline                            :which-key "Deadline")
  "oe"  '(org-export-dispatch                     :which-key "Export")
  "og"  '(counsel-org-goto-all                    :which-key "Goto head")
  "ol"  '(org-insert-link                         :which-key "Ins. link")
  "on"  '(org-add-note                            :which-key "Add note")
  "oo"  '(org-open-at-point                       :which-key "Open link")
  "op"  '(org-set-property                        :which-key "Property")
  "os"  '(org-schedule                            :which-key "Schedule")
  "ot"  '(evil-org-org-insert-todo-heading-respect-content-below :which-key "New todo")
  "or"  '(:ignore t                               :which-key "Org roam")
  "p"   '(:ignore t                               :which-key "p")
  "q"   '(:ignore t                               :which-key "Quit")
  "qq"  '(my/save-all-kill-emacs-no-prompt        :which-key "Save&kill")
  "qs"  '(save-buffers-kill-emacs                 :which-key "Prompt&kill")
  "r"   '(:ignore t                               :which-key "Register")
  "rl"  '(consult-register-load                   :which-key "Load")
  "rr"  '(counsel-mark-ring                       :which-key "Mini marks")
  "rs"  '(consult-register-store                  :which-key "Store")
  "rv"  '(exchange-point-and-mark                 :which-key "Visual mark")
  "s"   '(:ignore t                               :which-key "Search")
  "sO"  '(occur                                   :which-key "Occur")
  "sR"  '(query-replace-regexp                    :which-key "Rep. regex")
  "so"  '(consult-outline                         :which-key "Outline")
  "sr"  '(query-replace                           :which-key "Replace")
  "ss"  '(swiper                                  :which-key "Swiper")
  "sw"  '(eww                                     :which-key "Web (eww)")
  "t"   '(:ignore t                               :which-key "Toggle")
  "tb"  '(toggle-truncate-lines                   :which-key "Line breaks")
  "tc"  '(centered-cursor-mode                    :which-key "Vert.center")
  "td"  '(display-time-mode                       :which-key "Date/time")
  "te"  '(evil-cursor-between-mode                :which-key "Evil between")
  "tf"  '(mixed-pitch-mode                        :which-key "Font pitch")
  "tg"  '(golden-ratio-mode                       :which-key "Gold. ratio")
  "th"  '(global-hl-line-mode                     :which-key "Hl line")
  "ti"  '(:ignore t                               :which-key "Indicate")
  "tic" '(display-fill-column-indicator-mode      :which-key "Column 79")
  "tii" '(indent-guide-mode                       :which-key "Indentation")
  "tin" '(whitespace-newline-mode                 :which-key "Newline")
  "tip" '(whitespace-page-delimiters-mode         :which-key "Newline")
  "tis" '(whitespace-mode                         :which-key "Spaces")
  "tk"  '(:ignore t                               :which-key "Keycast")
  "tkh" '(keycast-header-line-mode                :which-key "Header")
  "tkl" '(keycast-log-mode                        :which-key "Log frame")
  "tkm" '(keycast-mode-line-mode                  :which-key "Mode line")
  "tkt" '(keycast-tab-bar-mode                    :which-key "Tab bar")
  "tl"  '(display-line-numbers-mode               :which-key "Line numbers")
  "tm"  '(mode-line-invisible-mode                :which-key "Mode line")
  "to"  '(outline-minor-mode                      :which-key "Outline")
  "tp"  '(prettify-symbols-mode                   :which-key "Prettify")
  "tr"  '(rainbow-mode                            :which-key "Rainbow")
  "ts"  '(flyspell-mode                           :which-key "Spell")
  "tt"  '(my/toggle-faces                         :which-key "Theme")
  "tv"  '(visual-line-mode                        :which-key "Visual lines")
  "tw"  '(writegood-mode                          :which-key "Write good")
  "t¨"  '(tab-bar-mode                            :which-key "Tab bar")
  "u"   '(universal-argument                      :which-key "Uni.arg.")
  "v"   '(:ignore t                               :which-key "v")
  "w"   '(evil-window-map                         :which-key "Window")
  "x"   '(:ignore t                               :which-key "Text")
  "xC"  '(evil-upcase                             :which-key "Upcase")
  "xc"  '(evil-downcase                           :which-key "Downcase")
  "xi"  '(evil-invert-char                        :which-key "Invert case")
  "xp"  '(transpose-chars                         :which-key "Swap chars")
  "xu"  '(insert-char                             :which-key "Unicode")
  "xx"  '(just-one-space                          :which-key "One space")
  "xz"  '(global-text-scale-adjust                :which-key "Zoom")
  "y"   '(:ignore t                               :which-key "y")
  "z"   '(:ignore t                               :which-key "z")
  "¨"   '(tab-bar-close-tab-by-name               :which-key "Close tab"))

;; ============================================================================
;;; Keybindings
;; ============================================================================
(bind-keys
 ;; ----------------------------------------------------------------------------
 ;; Global map (minor maps will override).
 :map global-map
 ("<escape>"      . keyboard-escape-quit)
 ("<Scroll_Lock>" . centered-cursor-mode)
 ("<C-tab>"       . tab-next)     ; This does not work in terminal atm.
 ("<C-backtab>"   . tab-previous) ; This does not work in terminal atm.
 ("C-¨"           . tab-line-mode)
 ("M-p"           . consult-yank-pop) ; Paste with `kill-ring' dialog.
 ;; ----------------------------------------------------------------------------
 ;; Operator state.
 :map evil-operator-state-map
 ;; Bad things happen if I hit "å" in operator state without this.
 ("å"             . keyboard-escape-quit) ; Danish keyboard.
 ;; ----------------------------------------------------------------------------
 ;; Motion state (normal, visual and motion).
 :map evil-motion-state-map
 ("<down>"        . evil-next-visual-line)     ; up/down navigate wrapped lines.
 ("<up>"          . evil-previous-visual-line) ; j/k respect Vim's line atoms.
 ("´"             . next-buffer)
 ("¨"             . tab-new)
 ("½"             . other-window)
 ("C-i"           . outline-cycle)       ; "C-i" = <tab>.
 ("<backtab>"     . outline-cycle-buffer)
 ("C-S-o"         . evil-jump-forward)   ; "C-i" is used for <tab>.
 ("gc"            . evilnc-comment-operator)
 ;; I `isearch' so I don't need "n"/"N" for `evil-search'. I use it to scroll.
 ("n"             . evil-scroll-down)    ; Use "C-s" to repeat `isearch'.
 ("N"             . evil-scroll-up)      ; In `isearch' "C-r" search backward.
 ;; Danish keyboard (some keys are not easily accessible).
 ("æ"             . forward-paragraph)   ; "}" use the equivalent evil command.
 ("Æ"             . backward-paragraph)  ; "{" use the equivalent evil command.
 ("ø"             . end-of-line)         ; "$" use the equivalent evil command.
 ("Ø"             . back-to-indentation) ; "^" use the equivalent evil command.
 ("å"             . my/org-agenda-custom)
 ("Å"             . my/org-capture-idea)
 ("C-å"           . org-cycle-agenda-files)
 ;; ----------------------------------------------------------------------------
 ;; Normal state.
 :map evil-normal-state-map
 ("g+"            . evil-numbers/inc-at-pt)
 ("g-"            . evil-numbers/dec-at-pt)
 ;; ----------------------------------------------------------------------------
 ;; Visual state.
 :map evil-visual-state-map
 ;; "S" is used by `evil-surround'. Use "C-s" for normal `isearch'.
 ("s"             . isearch-forward-thing-at-point) ; Input the visual region.
 ;; Don't use "v" to exit visual state. <escape> or a command works.
 ("v"             . exchange-point-and-mark)
 ;; ----------------------------------------------------------------------------
 ;; Insert state. I like to access some commands in insert state.
 :map evil-insert-state-map
 ("C-g"           . evil-normal-state)
 ("C-b"           . evil-backward-word-begin)
 ("C-B"           . evil-backward-WORD-begin)
 ("C-d"           . backward-kill-word) ; In evil this is normally bound "C-w".
 ("C-e"           . forward-word)       ; Not using `evil-cursor-between-mode'.
 ("C-p"           . yank)               ; "M-p" is in the `global-map'.
 ("C-v"           . set-mark-command)   ; "<C-SPC>" is used by general.el.
 ("C-w"           . evil-forward-word-begin) ; Not `evil-delete-backward-word'.
 ("C-W"           . evil-forward-WORD-begin)
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
 ;; :map org-columns-map
 ;; ("<escape>"      . org-columns-quit)
 :map org-present-mode-keymap
 ("<left>"        . org-present-prev)
 ("<right>"       . org-present-next)
 ("<up>"          . org-present-beginning)
 ("<down>"        . org-present-end)
 :map corfu-map
 ("C-."           . corfu-insert-separator) ; For orderless.
 ("C-i"           . corfu-next) ; "C-i" make <tab> work in terminal too.
 ("<backtab>"     . corfu-previous)
 :map minibuffer-local-map
 ("C-."           . embark-act)
 ("M-."           . embark-dwim)
 ("<backtab>"     . marginalia-cycle))
;; ============================================================================
;;;; Keybindings in maps depending on the evil state with `evil-define-key'
;; ============================================================================
(evil-define-key 'normal global-map
  ;; I never use Vim's substitute "s". If I need it, "cl" does the same thing.
  "s"           #'isearch-forward ; I prefer `isearch'. "C-s" to repeat.
  "S"           #'isearch-forward-thing-at-point) ; This is like Vim's "*".
(evil-define-key 'normal help-mode-map
  (kbd "SPC") nil ; Make general.el take over <SPC>.
  (kbd "<escape>") #'quit-window)
(evil-define-key 'normal Info-mode-map
  (kbd "SPC") nil
  (kbd "<escape>") #'quit-window)
(evil-define-key 'normal dired-mode-map
  (kbd "SPC") nil
  "h"           #'dired-up-directory
  "l"           #'dired-find-file
  "a"           #'dired-omit-mode            ; Like "ls -a" toggle.
  "s"           #'dired-hide-details-mode    ; Like "ls -l" toggle.
  ;; Personal swap to make "o" open files with system default program.
  "o"           #'browse-url-of-dired-file   ; Normally bound to "W".
  "W"           #'dired-sort-toggle-or-edit) ; Normally bound to "o".
(evil-define-key 'normal org-mode-map
  "t"           #'org-todo            ; In operator state "t" work as in Vim.
  "T"           #'org-todo-yesterday) ; "ct." (change to ".") work as expected.
;; :map org-columns-map
(evil-define-key 'motion org-agenda-mode-map
  (kbd "SPC") nil
  (kbd "<S-left>")  #'org-agenda-earlier
  (kbd "<S-right>") #'org-agenda-later
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
  "å"           #'org-capture
  "Å"           #'my/org-capture-idea)

;; ============================================================================
;;; Startup page
;; ============================================================================
(my/org-agenda-custom)
;; End of init.el
