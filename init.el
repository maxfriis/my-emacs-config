;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;; ============================================================================
;;; Fonts and faces
;; ============================================================================
(when (display-graphic-p)
  (if (find-font (font-spec :name "Ubuntu Mono 18"))
      (set-face-font 'default "Ubuntu Mono 18")
    ;; else
    (message "The font is set to %s" (font-get-system-font)))
  ;; The variable width Ubuntu font doesn't look good with Ubuntu Mono imo.
  (when (find-font (font-spec :name "Verdana 24"))
    (set-face-font 'variable-pitch "Verdana 24")))
;; ----------------------------------------------------------------------------
;; Face colors.
(when (file-newer-than-file-p
       (locate-user-emacs-file "ansi-faces.el")
       (locate-user-emacs-file "ansi-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "ansi-faces.el")))
(when (file-newer-than-file-p
       (locate-user-emacs-file "brown-faces.el")
       (locate-user-emacs-file "brown-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "brown-faces.el")))
(when (file-newer-than-file-p
       (locate-user-emacs-file "white-faces.el")
       (locate-user-emacs-file "white-faces.elc"))
  (byte-compile-file (locate-user-emacs-file "white-faces.el")))
(if (and (/= (user-uid) 0)    ; Ansi colors as root
         (display-graphic-p)) ; or in terminal.
    (load (locate-user-emacs-file "brown-faces.elc") nil t)
  ;; else
  (load (locate-user-emacs-file "ansi-faces.elc") nil t))
;; ----------------------------------------------------------------------------
;; Additional coloring.
(font-lock-add-keywords
 'emacs-lisp-mode
 '(;; Functions closely related to strings (cyan).
   ("(\\(concat\\)[\s\n]"                     . (1 'font-lock-string-face))
   ("(\\(format\\)[\s\n]"                     . (1 'font-lock-string-face))
   ("(\\(propertize\\)[\s\n]"                 . (1 'font-lock-string-face))
   ;; Other functions not colored by default (yellow).
   ("(\\(cons\\|list\\|load\\)[\s\n]"        . (1 'font-lock-keyword-face))
   ("(\\(\\(add-\\|set-\\)[^\s]+\\)[\s\n]"   . (1 'font-lock-keyword-face))
   ("(\\(a?\\(cos\\|sin\\|tan\\)\\)[\s\n]"   . (1 'font-lock-keyword-face))
   ("(\\(sqrt\\|expt?\\|log\\)[\s\n]"        . (1 'font-lock-keyword-face))
   ("(\\(1?[+-]\\|[*/]\\)[\s\n]"             . (1 'font-lock-keyword-face))
   ;; Boolean logic functions (green).
   ("(\\(not\\|and\\|x?or\\)[\s\n]"    . (1 'font-lock-function-call-face))
   ("(\\([^\s]*[<>=]\\)[\s\n]"         . (1 'font-lock-function-call-face))
   ("(\\(eq\\(\\(ua\\)?l\\)?\\)[\s\n]" . (1 'font-lock-function-call-face))
   ("(\\(mem\\(ber\\|ql?\\)\\)[\s\n]"  . (1 'font-lock-function-call-face))
   ("(\\([be]o[bl]p\\))"               . (1 'font-lock-function-call-face))
   ("(\\([^\s]+-p\\)[\s\n)]"           . (1 'font-lock-function-call-face))
   ;; Prefix for constants and function arguments (blue).
   ("\\(float-\\(e\\|pi\\)\\)[\s\n)]"       . (1 'font-lock-constant-face))
   ("\\(#'\\)"                               . (1 'font-lock-builtin-face))
   ;; Booleans and number/float/fraction/date/time (magenta).
   ("\\(\\([(\s]+\\(nil\\|t\\)\\)+\\)[\s\n)]" . (1 'font-lock-number-face))
   ("\\(\\(\\([(\s]\\|^\\)[0-9]*[/:-]?[0-9]*[.:-]?[0-9]+\\)+\\)[\s\n)/.:-]"
    . (1 'font-lock-number-face))))
;; ----------------------------------------------------------------------------
;; Two basic size faces.
(defface shrink '((t :height .8))
  "Basic shrink face.  Size: 4/5=.8.
This does the opposite of the `grow' face."
  :group 'basic-faces)
(defface grow '((t :height 1.25))
  "Basic grow face.  Size: 5/4=1.25.
This does the opposite of the `shrink' face."
  :group 'basic-faces)

;; ============================================================================
;;; Mode line
;; ============================================================================
(setq-default ; `mode-line-format' is buffer local so `setq-default'.
 mode-line-format
 ;; ----------------------------------------------------------------------------
 ;; `ace-window-display-mode' start the mode line with a window number.
 '((:eval ; Eval and list everything.
    (list
     ;; Indicators.
     (if (and (= (user-uid) 0) ; I link ~/.emacs.d in /root.
              (mode-line-window-selected-p))
         (propertize
          "#"
          'help-echo "Root access"
          'mouse-face 'mode-line-highlight
          'face 'warning) ; Emphasize.
       ;; else
       " ")
     (if evil-mode
         (propertize
          evil-mode-line-tag
          'face 'shrink)
       ;; else
       (propertize
        "🅴"
        'help-echo "evil-mode disabled, mouse-1: Enable"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'evil-mode)
        'mouse-face 'mode-line-highlight
        'face 'shrink))
     (unless (display-graphic-p) " ")
     mode-line-modified
     (cond
      ((buffer-narrowed-p)
       (propertize
        "="
        'help-echo "Narrowed, mouse-1: Widen"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'mode-line-widen)
        'mouse-face 'mode-line-highlight))
      ((window-dedicated-p)
       (propertize
        "!"
        'help-echo "Window dedicated to buffer"
        'mouse-face 'mode-line-highlight
        'face 'bold))
      ;; Local minor modes.
      ((and (not centered-cursor-mode)
            (mode-line-window-selected-p))
       (propertize
        "÷"
        'help-echo "centered-cursor-mode disabled, mouse-1: Enable"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'centered-cursor-mode)
        'mouse-face 'mode-line-highlight))
      ((and rainbow-mode
            (mode-line-window-selected-p))
       (propertize
        "#"
        'help-echo "rainbow-mode enabled, mouse-1: Disable"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'rainbow-mode)
        'mouse-face 'mode-line-highlight))
      ;; Global minor modes.
      ((not golden-ratio-mode)
       (propertize
        "+"
        'help-echo "golden-ratio-mode disabled, mouse-1: Enable"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'golden-ratio-mode)
        'mouse-face 'mode-line-highlight))
      ((not auto-save-visited-mode)
       (propertize
        "*"
        'help-echo "Auto save disabled, mouse-1: Enable"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'auto-save-visited-mode)
        'mouse-face 'mode-line-highlight))
      ((not evil-emacs-cursor-model-mode)
       (propertize
        "?"
        'help-echo "evil-emacs-cursor-model-mode disabled, mouse-1: Enable"
        'local-map (make-mode-line-mouse-map
                    'mouse-1 #'evil-emacs-cursor-model-mode)
        'mouse-face 'mode-line-highlight
        'face 'bold))
      (t
       (propertize
        "|"
        'help-echo "The default minor modes are on"
        'mouse-face 'mode-line-highlight)))
     " "
     ;; Buffer name.
     mode-line-buffer-identification
     " "
     ;; Major mode with less redundant information. I don't use `mode-name'.
     (propertize
      (concat
       "("
       (string-replace
        "-" " "
        (replace-regexp-in-string
         "\\`emacs-" "e"
         (replace-regexp-in-string
          "\\`org-" ""
          (replace-regexp-in-string
           "-buffer\\'" ""
           (replace-regexp-in-string
            "-mode\\'" ""
            (downcase (symbol-name major-mode)))))))
       ")")
      'help-echo (symbol-name major-mode) ; Full name on mouse hover.
      'mouse-face 'mode-line-highlight)
     ;; Active vs. inactive window.
     (if (mode-line-window-selected-p)
         (list
          ;; Version control.
          (if vc-mode
              (propertize ; Replace the built in `propertize' of vc-mode.
               (replace-regexp-in-string "\\` Git" "" vc-mode)
               'help-echo "Version control, mouse-1: Magit"
               'local-map (make-mode-line-mouse-map
                           'mouse-1 #'magit)
               'mouse-face 'mode-line-highlight)
            "")
          " "
          ;; Display `global-mode-string' used by e.g. `display-time-mode'.
          mode-line-misc-info
          ;; Gap for alignment.
          (propertize
           " "
           'display `((space :align-to (- (+ right right-fringe right-margin)
                                          ,(if (display-graphic-p) 7 9))))
           'face    'mode-line-inactive)
          ;; Horizontal position.
          (propertize
           "%3c"
           'help-echo "Column number")
          " "
          ;; Vertical position.
          (if display-line-numbers-mode
              '(-3 "%o")
            ;; else
            (propertize
             "%3l"
             'help-echo "Line number")))
       ;; else
       (list
        (propertize
         " "
         'display `((space :align-to (- (+ right right-fringe right-margin)
                                        ,(if (display-graphic-p) 4 5)))))
        '(-3 "%o")))))))

;; ============================================================================
;;; Vanilla stuff
;; ============================================================================
;; Variables.
(setq-default
 indent-tabs-mode nil ; No tab pollution.
 display-line-numbers-width 4
 fill-column 79)
(setq
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      ;; else
      "%b")))
 frame-background-mode 'dark ; Terminal can't detect.
 tab-width 4
 warning-minimum-level :error
 visible-bell t
 use-dialog-box nil
 use-short-answers t
 initial-scratch-message nil
 large-file-warning-threshold nil
 delete-by-moving-to-trash t
 trash-directory "~/.local/share/Trash/files"
 bookmark-default-file (locate-user-emacs-file ".bookmarks.eld")
 custom-file           (locate-user-emacs-file ".custom.eld")
 recentf-save-file     (locate-user-emacs-file ".recentf.eld")
 savehist-file         (locate-user-emacs-file ".history.eld")
 save-place-file       (locate-user-emacs-file ".places.eld")
 diary-file            (locate-user-emacs-file ".diary")
 truncate-partial-width-windows 59 ; Only wrap lines in wide windows.
 recentf-exclude
 '("\\`~/\\.backup.*\\'"               ; Don't edit backup files.
   "\\`~/\\.emacs\\.d/.*\\.el[cd]?\\'" ; Elisp config and data files.
   "\\`~/\\.emacs\\.d/diary\\'"
   "\\`~/org/agenda/.*\\.org\\'"       ; Agenda files. These are buffers.
   "\\`~/org/inbox\\.org\\'"
   "\\`~/.*\\.pdf\\'"
   ".*\\.el\\.gz\\'")
 ;; ----------------------------------------------------------------------------
 ;; Dired.
 dired-kill-when-opening-new-dired-buffer t
 dired-dwim-target t
 dired-isearch-filenames t
 dired-recursive-copies 'always
 dired-recursive-deletes 'always ; Risky but I move to trash.
 dired-listing-switches "-aghov --group-directories-first"
 dired-omit-files "\\`[#\\.].*\\'"
 dired-omit-verbose  nil
 auto-revert-verbose nil
 global-auto-revert-non-file-buffers t ; Auto update dired when files change.
 ;; ----------------------------------------------------------------------------
 ;; Ediff.
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 ;; ----------------------------------------------------------------------------
 ;; Emacs' `isearch'.
 search-whitespace-regexp "[^\n]*?"
 isearch-repeat-on-direction-change t
 isearch-lazy-count t
 lazy-count-prefix-format "%s/%s "
 ;; ----------------------------------------------------------------------------
 ;; Erc irc-chat.
 erc-server "irc.libera.chat"
 erc-nick "maxfriis"
 erc-user-full-name "Peter"
 erc-track-shorten-start 8
 erc-autojoin-channels-alist '(("lead.libera.chat" "#emacs" "#emacs-beginner"))
 erc-kill-buffer-on-part t
 ;; ----------------------------------------------------------------------------
 ;; Tab bar.
 tab-bar-show                  1
 tab-bar-close-button-show     nil
 tab-bar-close-last-tab-choice 'tab-bar-mode-disable
 tab-bar-new-button            nil
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
 ;; Time/display.
 tmr-mode-line-prefix "⏲"
 tmr-mode-line-separator "|"
 world-clock-list ; For `world-clock'.
 '(("UTC" "UTC")
   ("Europe/Copenhagen" "Copenhagen")
   ("Europe/Athens" "Athens")
   ("Asia/Dubai" "Dubai")
   ("Asia/Kolkata" "Mumbai")
   ("Asia/Jakarta" "Jakarta")
   ("Asia/Tokyo" "Tokyo")
   ("Australia/Sydney" "Sydney")
   ("America/Los_Angeles" "Seattle")
   ("America/New_York" "New York")
   ("America/Nuuk" "Nuuk"))
 world-clock-time-format "[%F %a %02H:%M] %Z"
 display-time-format "[%F %a %02H:%M]" ; org timestamp.
 display-line-numbers-type 'relative
 ;; Focus popups. The default is to not focus them.
 display-buffer-alist
 '(("\\`\\*Org Agenda\\*\\'"
    (display-buffer-same-window)
    (body-function . select-window))
   ("\\`\s?\\*.*\\*\\'"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (body-function . select-window))))
;; ============================================================================
;;;; Native hooks
;; ============================================================================
(add-hook 'after-save-hook #'vc-refresh-state)        ; Version control.
(add-hook 'text-mode-hook  #'visual-line-mode)        ; Wrap lines at words.
(add-hook 'prog-mode-hook  #'outline-minor-mode)      ; Cycle with "<backtab>".
(add-hook 'dired-mode-hook #'dired-omit-mode)         ; Toggle with ")".
(add-hook 'dired-mode-hook #'dired-hide-details-mode) ; Toggle with "(".
;; Visual line numbers when `outline-cycle-buffer' or `outline-cycle' fold.
(add-hook 'outline-view-change-hook ; This is obsolete. Why?
          #'(lambda ()
              (when display-line-numbers
                (if (or (outline-invisible-p (pos-bol))
                        (outline-invisible-p (pos-eol)))
                    (setq display-line-numbers 'visual)     ; Folded lines.
                  ;; else
                  (setq display-line-numbers 'relative))))) ; Wrapped lines.
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message
               "Emacs ready in %s with %d garbage collections."
               (format
                "%.1f seconds"
                (float-time (time-subtract after-init-time before-init-time)))
               gcs-done)))
;; ============================================================================
;;;; Native global minor modes
;; ============================================================================
(auto-save-visited-mode 1) ; Almost edit files rather than buffers.
(save-place-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

;; ============================================================================
;;; Custom stuff
;; ============================================================================
;; Toggle my faces.
(defun my-toggle-faces ()
  "Toggle my two default faces.
\nThe faces are loaded from brown-faces.elc and ansi-faces.elc respectively."
  (interactive)
  (if (equal (face-background 'default) "#000")
      (load (locate-user-emacs-file "brown-faces.elc") nil t)
    ;; else
    (load (locate-user-emacs-file "ansi-faces.elc") nil t))
  (message "%s background faces" (face-background 'default)))
;; ----------------------------------------------------------------------------
;; Dired in new tab.
(defun my-split-dired-tab (directory)
  "Split dired in new tab."
  (interactive "DDirectory: ")
  (dired-other-tab directory)
  (split-window-right)
  (find-file "~/")
  (ace-swap-window)
  (aw-flip-window))
;; ----------------------------------------------------------------------------
;; Open init files.
(defun my-find-init-file ()
  "Open configuration file init.el.
\n`Previous-buffer' will be early-init.el."
  (interactive)
  (find-file (locate-user-emacs-file "early-init.el"))
  (find-file (locate-user-emacs-file "init.el")))
;; ----------------------------------------------------------------------------
;; Open theme files.
(defun my-find-my-faces-file ()
  "Open my theme file brown-faces.el.
\n`Previous-buffer' will be ansi-faces.el."
  (interactive)
  (find-file (locate-user-emacs-file "ansi-faces.el"))
  (find-file (locate-user-emacs-file "brown-faces.el")))
;; ----------------------------------------------------------------------------
;; Open agenda and plan file.
(defun my-find-agenda-file ()
  "Open my agenda file agenda.org.
\nThese agenda files are usually opened in buffers by `org-agenda'.
The function will organize the `buffer-list' and focus agenda.org."
  (interactive)
  (find-file (concat org-agenda-directory "plan.org"))
  (find-file (concat org-agenda-directory "agenda.org")))
;; ----------------------------------------------------------------------------
;; Open note and date file.
(defun my-find-note-file ()
  "Open my notes file note.org.
\nThese agenda files are usually opened in buffers by `org-agenda'.
The function will organize the `buffer-list' and focus note.org."
  (interactive)
  (find-file (concat org-agenda-directory "date.org"))
  (find-file (concat org-agenda-directory "note.org")))
;; ----------------------------------------------------------------------------
;; Custom agenda.
(defun my-org-agenda-custom ()
  "Custom agenda with NEXT items, this week's agenda and TODO/HOLD items."
  (interactive)
  (org-agenda nil "c")
  (unless (eq (char-after) ?\s) ; Stay on a NEXT item (start with a space).
    (org-agenda-goto-today)))   ; If on a heading (doesn't).
;; ----------------------------------------------------------------------------
;; Capture idea.
(defun my-org-capture-idea ()
  "Capture an idea to inbox.org and make it a NEXT item."
  (interactive)
  (org-capture nil "i")
  (org-save-all-org-buffers)
  (when (eq major-mode 'org-agenda-mode)
    (org-agenda-redo) ; Might turn `global-hl-line-mode' off for some reason?
    (global-hl-line-mode 1)
    (goto-char (point-min)))) ; Jump to the newly prepended NEXT item.
;; ----------------------------------------------------------------------------
;; Toggle maximized window.
(defvar my-window-configuration nil
  "Track window configuration, when it's suspended.")
(defun my-toggle-window-maximize ()
  "Toggle one maximized window and the window configuration from last maximize."
  (interactive)
  (if (one-window-p)
      (when my-window-configuration
        (set-window-configuration my-window-configuration)
        (message "Window configuration restored"))
    ;; else
    (setq my-window-configuration (current-window-configuration))
    (delete-other-windows)
    (message "Window maximized")))
;; ----------------------------------------------------------------------------
;; Four window setup.
(defun my-4-windows ()
  "Four windows, three on the right and the left focused.
\nThis nice window setup is surprisingly hard to create without this function.
\nThe content of window 1 will be unchanged.  By default the day-agenda goes in
window 2, Messages in window 3 and window 1's `next-buffer' in window 4."
  (interactive)
  (delete-other-windows)
  (when (eq major-mode 'org-agenda-mode) ; Would affect `org-agenda-list'.
    (org-agenda-quit)
    (ibuffer))
  (split-window-right) ; 4th window.
  (other-window 1)
  (next-buffer)
  (while (memq major-mode '(dired-mode
                            org-agenda-mode
                            messages-buffer-mode))
    (next-buffer))
  (split-window-below) ; 3rd window.
  (switch-to-buffer "*Messages*")
  (split-window-below) ; 2nd window.
  (org-agenda-list)
  (set-window-dedicated-p nil t)
  (set-window-dedicated-p (next-window) t) ; 3rd.
  (other-window -1)
  (message "Four window setup"))
;; ----------------------------------------------------------------------------
;; Ace window swap.
(defun my-ace-swap-window ()
  "Swap two window contents (prompt if 3+).  Keep focus on the current window.
\nThe normal `ace-swap-window' swap two windows, but stays with the current buffer
and focus the window you swapped to."
  (interactive)
  (ace-swap-window)
  (aw-flip-window)
  (message "Windows swapped"))
;; ----------------------------------------------------------------------------
;; Magit stage and commit.
(defun my-git-stage-all-and-commit (message)
  "Stage and commit everything with a commit MESSAGE.
\nWill respect what is configured to be ignored."
  (interactive "sCommit Message: ")
  (save-some-buffers t)
  (call-process-shell-command (format "git commit -a -m \"%s\"" message))
  (vc-refresh-state))
;; ----------------------------------------------------------------------------
;; Save and quit.
(defun my-save-all-kill-emacs-no-prompt ()
  "Save all and quit without a prompt.
\nUse something like `backup-each-save' to make this less risky."
  (interactive)
  (save-some-buffers t)
  (when (file-newer-than-file-p
         (locate-user-emacs-file "early-init.el")
         (locate-user-emacs-file "early-init.elc"))
    (byte-compile-file (locate-user-emacs-file "early-init.el")))
  (when (file-newer-than-file-p
         (locate-user-emacs-file "init.el")
         (locate-user-emacs-file "init.elc"))
    (when (file-readable-p (locate-user-emacs-file "init.elc"))
      (copy-file (locate-user-emacs-file "init.elc")
                 (locate-user-emacs-file "init.elc~") t t)) ; Overwrite.
    (byte-compile-file (locate-user-emacs-file "init.el")))
  (kill-emacs))

;; ============================================================================
;;; Package.el
;; ============================================================================
;; I use the good old package.el manager rather than a newer alternative.
;; I trust packages to defer sensibly and don't want to micro manage.
(setq
 load-prefer-newer t ; Use .el if newer than .elc.
 package-archives
 '(("elpa"         . "https://elpa.gnu.org/packages/")
   ("elpa-devel"   . "https://elpa.gnu.org/devel/")
   ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
   ("org"          . "https://orgmode.org/elpa/")
   ("melpa-stable" . "https://stable.melpa.org/packages/")
   ("melpa"        . "https://melpa.org/packages/"))
 package-archive-priorities
 '(("elpa"   . 3)  ; Prefer older versions from elpa.
   ("melpa"  . 2)
   ("nongnu" . 1)) ; The remaining archives have priority 0.
 package-selected-packages
 '(evil-collection evil-nerd-commenter evil-surround evil-numbers evil-org org-superstar org-appear org-present magit dired-subtree cape corfu nerd-icons-corfu nerd-icons-dired nerd-icons-ibuffer nerd-icons-completion avy vertico marginalia orderless embark-consult counsel tmr rainbow-delimiters rainbow-mode recursive-narrow centered-cursor-mode golden-ratio ace-window transpose-frame mixed-pitch indent-guide casual-suite keycast undo-tree flycheck writegood-mode auto-package-update backup-each-save package-lint))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents nil))
(package-install-selected-packages t)
;; ============================================================================
;;;; Files, backup and undo
;; ============================================================================
(require 'dired-subtree)
(require 'auto-package-update)
(setq
 auto-package-update-interval 30
 auto-package-update-hide-results t)
(auto-package-update-maybe)
(require 'backup-each-save)
(setq backup-each-save-mirror-location "~/.backup-emacs-save")
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
 undo-tree-auto-save-history t)
(global-undo-tree-mode 1)
;; ============================================================================
;;;; Buffers, completion and windows
;; ============================================================================
(require 'avy)
(setq avy-timeout-seconds 1)
(require 'recursive-narrow)
(put 'narrow-to-region 'disabled nil) ; Disable a warning somehow?
(require 'mixed-pitch)
(dolist (face '(org-date
                org-priority
                org-special-keyword
                org-tag
                org-todo))
  (add-to-list 'mixed-pitch-fixed-pitch-faces face t))
;; ----------------------------------------------------------------------------
;; Mini buffer.
(require 'vertico)
(setq vertico-resize nil)
(vertico-mode 1)
(with-eval-after-load 'vertico
  (require 'marginalia)
  (marginalia-mode 1))
;; Tidy typing directories (rfn=read-file-name):
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(require 'orderless)
(setq completion-styles '(orderless)) ; basic substring.
(require 'embark)
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
(require 'cape)
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
(add-hook 'completion-at-point-functions #'cape-elisp-block)
(add-hook 'completion-at-point-functions #'cape-history)
;; ----------------------------------------------------------------------------
;; Windows.
(require 'golden-ratio)
(setq
 golden-ratio-exclude-buffer-regexp
 '("\\`\s?\\*undo-tree.*\\*\\'"
   "\\`\\*Ediff\sControl\sPanel\\*\\'"))
(golden-ratio-mode 1)
(require 'centered-cursor-mode)
(add-to-list 'ccm-ignored-commands 'mwheel-scroll)
(global-centered-cursor-mode 1) ; Local toggle with `Scroll_Lock'.
(require 'transpose-frame)
(require 'ace-window)
(ace-window-display-mode 1)
(tmr-mode-line-mode 1)
;; ============================================================================
;;;; Guides
;; ============================================================================
(require 'consult)
(require 'counsel)
(require 'keycast)
(keycast-tab-bar-mode 1)
;; ----------------------------------------------------------------------------
;; Writing tips.
(require 'flycheck)
(add-hook 'text-mode-hook #'flyspell-mode)
(require 'writegood-mode)
;; ----------------------------------------------------------------------------
;; Color guides.
(require 'indent-guide)
(add-hook 'prog-mode-hook #'indent-guide-mode)
(require 'rainbow-delimiters)
(setq rainbow-delimiters-max-face-count 3)
(with-eval-after-load 'org
  (modify-syntax-entry ?< "_" org-mode-syntax-table)
  (modify-syntax-entry ?> "_" org-mode-syntax-table))
(add-hook 'org-mode-hook  #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(require 'rainbow-mode) ; Toggle with "<spc> t r".
;; ----------------------------------------------------------------------------
;; Thumbnails.
(when (display-graphic-p)
  (require 'nerd-icons)
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)) ; MS Windows?
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
 evil-undo-system 'undo-tree ; Set before loading evil!
 evil-want-keybinding nil    ; `evil-collection' need this.
 evil-want-integration t
 evil-want-C-i-jump nil)     ; I rebind jump forward to "C-S-o".
(require 'evil)
;; ----------------------------------------------------------------------------
;; Cursor colors. I combine f and 0 to indicate the evil state.
(setq
 evil-operator-state-cursor '(box        "#f00")
 evil-normal-state-cursor   '(box        "#0f0")
 evil-motion-state-cursor   '(box        "#00f")
 evil-visual-state-cursor   '((bar  . 4) "#0ff")
 evil-replace-state-cursor  '((hbar . 4) "#f0f")
 evil-insert-state-cursor   '((bar  . 4) "#ff0")
 evil-emacs-state-cursor    '(box        "#f00"))
;; ----------------------------------------------------------------------------
;; Tags for `evil-mode-line-tag'.
(setq
 evil-operator-state-tag "🅾"
 evil-normal-state-tag   "🅽"
 evil-motion-state-tag   "🅼"
 evil-visual-char-tag    "🆅"
 evil-visual-line-tag    "🅻"
 evil-visual-block-tag   "🅱"
 evil-replace-state-tag  "🆁"
 evil-insert-state-tag   "🅸"
 evil-emacs-state-tag    "🆉")
(evil-mode 1)
;; ============================================================================
;;;; Addons
;; ============================================================================
(require 'evil-collection)
(evil-collection-init)
(require 'evil-surround)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()
              (add-to-list 'evil-surround-pairs-alist '(?` . ("`" . "'")))))
(global-evil-surround-mode 1)
(require 'evil-nerd-commenter)
(require 'evil-numbers)
(require 'evil-org)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(add-hook 'org-mode-hook #'evil-org-mode)
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
;; ============================================================================
;;;; `evil-emacs-cursor-model-mode' (my sacrilege to "the curch of Vim")
;; ============================================================================
(unless (package-installed-p 'evil-emacs-cursor-model-mode)
  (package-install-file (locate-user-emacs-file "evil-emacs-cursor-model-mode.el")))
;; (require 'evil-emacs-cursor-model-mode)
(load (locate-user-emacs-file "evil-emacs-cursor-model-mode") nil t) ; Testing.
(evil-emacs-cursor-model-mode 1)
;; ============================================================================
;;;; Hooks to suspend `global-hl-line-mode'
;; ============================================================================
;; Suspend the `hl-line' so `region' can use the same background color.
;; ----------------------------------------------------------------------------
;; The buffer-local `hl-line-mode' doesn't properly respect "specificity" so it
;; will only override `global-hl-line-mode' when called interactively?
(add-hook 'evil-visual-state-entry-hook
          #'(lambda () (global-hl-line-mode 0)))
(add-hook 'evil-visual-state-exit-hook #'global-hl-line-mode)
;; ----------------------------------------------------------------------------
;; Suspend the `hl-line' and have absolute line numbers in "input" states.
(dolist (hook '(evil-insert-state-entry-hook
                evil-replace-state-entry-hook
                evil-emacs-state-entry-hook))
  (add-hook hook
            #'(lambda ()
                (when display-line-numbers
                  (setq display-line-numbers t)) ; No j/k navigation.
                (global-hl-line-mode 0))))
(dolist (hook '(evil-insert-state-exit-hook
                evil-replace-state-exit-hook
                evil-emacs-state-exit-hook))
  (add-hook hook
            #'(lambda ()
                (when display-line-numbers
                  (setq display-line-numbers 'relative)) ; Back to default.
                (global-hl-line-mode 1)
                (whitespace-cleanup))))
;; ----------------------------------------------------------------------------
;; `whitespace-cleanup'.
(add-hook 'before-save-hook
          #'(lambda ()
              (unless (memq evil-state '(insert replace emacs))
                (whitespace-cleanup))))

;; ============================================================================
;;; Org.el
;; ============================================================================
;; Create `org-agenda' directories.
(setq
 org-directory "~/org/") ; Trailing slash.
(defvar org-agenda-directory (concat org-directory "agenda/")
  "Default `org-agenda' directory.
\nI include this directory in the list `org-agenda-files'.
All org files in the directory will then be in the agenda.")
(unless (file-exists-p org-directory)
  (make-directory org-directory))
(unless (file-exists-p org-agenda-directory)
  (make-directory org-agenda-directory))
;; ----------------------------------------------------------------------------
;; Create `org-capture' and `org-refile' targets.
(unless (file-exists-p (concat org-directory "archive.org"))
  (make-empty-file (concat org-directory "archive.org")))
(unless (file-exists-p (concat org-directory "inbox.org"))
  (make-empty-file (concat org-directory "inbox.org")))
(unless (file-exists-p (concat org-agenda-directory "note.org"))
  (make-empty-file (concat org-agenda-directory "note.org")))
(unless (file-exists-p (concat org-agenda-directory "plan.org"))
  (find-file (concat org-agenda-directory "plan.org"))
  (insert
   (concat
    "#+title: Projects\n"
    "#+startup: content hideblocks\n"
    "\n"))
  (save-buffer)
  (switch-to-buffer "*scratch*"))
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
;;;; Variables setting up org-mode
;; ============================================================================
(setq
 org-default-notes-file (concat org-directory "inbox.org")
 org-ellipsis " … "
 org-todo-keywords ; I use categories and refile rather than more keywords.
 '((type     "NEXT(n!/!)" "TODO(t!/!)" "|") ; Active states.
   (type "|" "HOLD(h@/!)" "DONE(d!/!)"))  ; Inactive states.
 org-priority-default ?C
 org-list-allow-alphabetical t
 org-list-demote-modify-bullet
 '(("+" . "*")
   ("*" . "-")
   ("-" . "+"))
 org-emphasis-alist
 '(("*" bold)
   ("/" italic)
   ("_" underline)
   ("+" '(shadow shrink))
   ("=" org-verbatim)
   ("~" org-code))
 org-tags-column -75 ; Right aligned.
 org-tag-alist
 '(("pc"       . ?c) ; c for computer.
   ("family"   . ?f)
   ("game"     . ?g)
   ("@home"    . ?h)
   (:newline)
   ("idea"     . ?i)
   ("money"    . ?m)
   ("phone"    . ?p)
   ("@work"    . ?w)
   (:newline)
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
 ;; Export.
 org-html-postamble nil
 org-latex-title-command nil
 org-export-with-smart-quotes t
 ;; org-export-exclude-tags '("noexport" "NOEXPORT")
 org-export-backends
 '(ascii latex beamer html odt md)
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
 org-reverse-note-order t ; Prepend refiles.
 org-refile-targets
 `((,(concat org-agenda-directory "agenda.org") :maxlevel . 1)
   (,(concat org-agenda-directory "plan.org")   :maxlevel . 1))
 org-archive-location (concat org-directory "archive.org::* Archive")
 org-agenda-files (list org-agenda-directory ; All org files in the directory.
                        (concat org-directory "inbox.org"))
 calendar-week-start-day 1
 org-agenda-format-date " [%F %a] "
 org-agenda-block-separator ?﹋ ; ﹌⎺̅‾﹉
 org-agenda-span 'day ; For `org-agenda-list'.
 org-agenda-time-leading-zero t
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
;; ----------------------------------------------------------------------------
;;;;; Custom agenda commands.
(setq
 org-agenda-custom-commands
 '(("c" "Custom agenda setup"
    ((todo
      "NEXT" ; Unblocked short tasks often pending scheduling and refiling.
      ((org-agenda-overriding-header "")))
     ;; Agenda supress timestamped active state items that are not current.
     (agenda
      ""
      ((org-agenda-span 'week)))
     ;; TODOs without a timestamp.
     (todo
      "TODO" ; Sort [/] cookies on top. This is typically categories.
      ;; I would prefer to use `org-agenda-sorting-strategy' but I have not
      ;; figured out `org-agenda-cmp-user-defined'.
      ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'notregexp "\\[[0-9]*/[0-9]*\\]"
          ;; or
          'timestamp))))
     (todo
      "TODO" ; Others. Not all TODOs have or should have a timestamp.
      ((org-agenda-overriding-header "") ; Share heading (same block).
       (org-agenda-block-separator nil)  ; Don't separate.
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'regexp "\\[[0-9]*/[0-9]*\\]"
          ;; or
          'timestamp))))
     ;; Inactive states. Not in the agenda even if scheduled.
     (todo
      "HOLD" ; For third party action pending.
      ((org-agenda-overriding-header "") ; Same block.
       (org-agenda-block-separator nil)))
     (todo
      "DONE" ; Only when prioritized.
      ((org-agenda-overriding-header "") ; Same block.
       (org-agenda-block-separator nil)
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'notregexp org-priority-regexp))))))
   ;; Append to agenda with "a".
   ("d" "DONE TODOs"
    ((todo
      "DONE"
      ((org-agenda-overriding-header "DONE TODOs")
       (org-agenda-skip-function
        '(org-agenda-skip-entry-if
          'regexp org-priority-regexp))))))
   ("f" "Fortnight agenda"
    ((agenda
      ""
      ((org-agenda-span 14)
       (org-agenda-start-with-log-mode t)))))
   ("u" "Untagged TODOs"
    ((tags-todo
      "-{.*}"
      ((org-agenda-overriding-header "Untagged TODOs")))))))
;; ============================================================================
;;;; Capture
;; ============================================================================
(setq
 org-capture-templates
 `(("c" "Category" entry ; Back tick (`) to use ",(concat...".
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
    :before-finalize (org-id-get-create)
    :immediate-finish t
    :prepend t)
   ("i" "Idea" entry
    (file ,(concat org-directory "inbox.org"))
    ,(concat
      "* NEXT %^{Idea}\n"
      ":LOGBOOK:\n"
      "- State \"NEXT\" from \"Capture\" %U\n"
      ":END:\n"
      "%i")
    :immediate-finish t
    :prepend t)
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
;; ----------------------------------------------------------------------------
;;;;; Logbook.
(setq
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
   (deldeadline . "Deadline removed            %t, was %S")))
;; ============================================================================
;;;; Addons
;; ============================================================================
;; Bullets.
(require 'org-superstar)
(setq
 org-superstar-headline-bullets-list
 '(?⓪ ?① ?② ?③ ?ⓧ) ; ④⑤⑥⑦⑧⑨⑩⑪⑫⑬⑭⑮⑯⑰⑱⑲⑳Ⓧ ; Italics change size.
 org-superstar-cycle-headline-bullets nil)
(add-hook 'org-mode-hook #'org-superstar-mode)
;; ----------------------------------------------------------------------------
;; Habits.
(require 'org-habit)
(require 'org-appear)
(setq
 org-habit-preceding-days 28
 org-habit-graph-column 59
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
          #'(lambda ()
              (setq
               prettify-symbols-alist ; ⧈⧇⊡⧆⊞⊟⧄⧅⊠⟏⟎ ▸▾▴◂ ☑☐☒ ✏✎✐ ×÷
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
;; Update "[/]"-cookies in all org buffers after a TODO state change.
(add-hook 'org-after-todo-state-change-hook
          #'(lambda ()
              (mapcar
               #'(lambda (buffer)
                   (with-current-buffer buffer
                     (org-update-statistics-cookies t)))
               (org-buffer-list))))
;; ----------------------------------------------------------------------------
;; Insert state in "C-c C-c" buffers.
(with-eval-after-load 'evil
  (add-hook 'org-capture-mode-hook     #'evil-insert-state)
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state))

;; ============================================================================
;;; Keybindings
;; ============================================================================
(bind-keys
 :map global-map
 ("C-g"           . evil-normal-state)
 ("<escape>"      . keyboard-escape-quit)
 ("<Scroll_Lock>" . centered-cursor-mode)
 ("M-p"           . consult-yank-pop) ; Paste with `kill-ring' dialog.
 ("C-,"           . embark-act)
 ("M-,"           . embark-dwim)
 :map evil-operator-state-map
 ;; Bad things happen if I hit "å" in operator state without this.
 ("å"             . keyboard-escape-quit) ; Danish keyboard.
 :map evil-visual-state-map
 ;; "S" is used by `evil-surround'. Use "C-s" for normal `isearch'.
 ("s"             . isearch-forward-thing-at-point) ; Input the region.
 ;; Don't use "v" to exit visual state. <escape> or a command works.
 ("v"             . exchange-point-and-mark)
 :map evil-normal-state-map
 ("SPC"           . nil) ; Don't override the motion map.
 ("S"             . nil)
 ("m"             . nil)
 ("s"             . nil)
 ("g+"            . evil-numbers/inc-at-pt)
 ("g-"            . evil-numbers/dec-at-pt)
 :map evil-motion-state-map
 ;; I don't need "n"/"N" for `evil-search'. I use `isearch'.
 ("½"             . mode-line-other-buffer)
 ("¨"             . tab-bar-new-tab)
 ("´"             . evil-set-marker) ; "m" is `backward-paragraph'.
 ("'"             . evil-goto-mark)  ; for my keyboard.
 ("`"             . counsel-evil-marks)
 ("<down>"        . evil-next-visual-line)     ; <up>/<down> wrapped lines.
 ("<up>"          . evil-previous-visual-line) ; j/k respect line atoms.
 ("<backtab>"     . outline-cycle-buffer)
 ("C-i"           . outline-cycle) ; "C-i" = <tab>.
 ("C-S-o"         . evil-jump-forward)
 ("C-S-n"         . evil-search-previous)
 ("N"             . evil-search-next)
 ("S"             . isearch-forward-thing-at-point) ; This is like Vim's "*".
 ("gc"            . evilnc-comment-operator)
 ("m"             . backward-paragraph) ; Below "k". "´" is `evil-set-marker'.
 ("n"             . forward-paragraph)  ; Below "j".
 ("s"             . isearch-forward) ; I prefer `isearch' and "C-s" to repeat.
 ("Æ"             . evil-scroll-up)      ; "C-u" is `universal-argument'.
 ("Ø"             . back-to-indentation) ; "^" use the evil command.
 ("Å"             . my-org-capture-idea)
 ("æ"             . evil-scroll-down)
 ("ø"             . end-of-line)         ; "$" use the evil command.
 ("å"             . my-org-agenda-custom)
 ("C-å"           . org-cycle-agenda-files)
 :map evil-insert-state-map
 ("C-0"           . beginning-of-line)
 ("C-S-b"         . evil-backward-WORD-begin)
 ("C-b"           . evil-backward-word-begin)
 ("C-d"           . backward-kill-word) ; In evil this is bound "C-w".
 ("C-e"           . forward-word)       ; Not `evil-emacs-cursor-model-mode'.
 ("C-p"           . yank)               ; "M-p" is in the `global-map'.
 ("C-v"           . set-mark-command)
 ("C-S-w"         . evil-forward-WORD-begin)
 ("C-w"           . evil-forward-word-begin) ; Not `evil-delete-backward-word'.
 ("C-S-ø"         . back-to-indentation)
 ("C-ø"           . end-of-line)
 :map org-present-mode-keymap
 ("<left>"        . org-present-prev)
 ("<right>"       . org-present-next)
 ("<up>"          . org-present-beginning)
 ("<down>"        . org-present-end)
 :map isearch-mode-map
 ("C-S-s"         . isearch-query-replace) ; "C-S" does not work.
 :map corfu-map
 ("C-,"           . corfu-insert-separator) ; Fuzzy completion.
 ("C-i"           . corfu-next) ; "C-i" = <tab>.
 ("<backtab>"     . corfu-previous)
 :map minibuffer-local-map
 ("<backtab>"     . marginalia-cycle))
;; ============================================================================
;;;; Keybindings in maps given states with `evil-define-key'
;; ============================================================================
(evil-define-key 'normal org-mode-map
  "T"           #'org-todo-yesterday
  "t"           #'org-todo ; In operator state e.g. "ct." work as in Vim.
  "Æ"           #'org-previous-visible-heading
  "æ"           #'org-next-visible-heading)
(evil-define-key 'motion org-agenda-mode-map
  (kbd "<S-left>")  #'org-agenda-earlier
  (kbd "<S-right>") #'org-agenda-later
  (kbd "SPC") nil ; Use <SPC> as a leader key.
  "A"           #'org-agenda-archive-default
  "R"           #'org-agenda-refile
  "T"           #'org-agenda-todo-yesterday
  "a"           #'org-agenda-append-agenda
  "gy"          #'org-agenda-year-view
  "n"           #'org-agenda-add-note
  "sd"          #'org-agenda-deadline
  "ss"          #'org-agenda-schedule
  "t"           #'org-agenda-todo
  "Æ"           #'org-agenda-backward-block
  "Å"           #'my-org-capture-idea
  "æ"           #'org-agenda-forward-block
  "å"           #'org-agenda-capture)
(evil-define-key 'normal dired-mode-map
  (kbd "SPC") nil
  ")"           #'dired-omit-mode ; "(" is toggle `dired-hide-details-mode'.
  "h"           #'dired-up-directory
  "l"           #'dired-find-file)
(evil-define-key 'normal tmr-tabulated-mode-map
  "+"           #'tmr
  "*"           #'tmr-with-details
  "R"           #'tmr-remove-finished
  "T"           #'tmr-with-details
  "a"           #'tmr-toggle-acknowledge
  "c"           #'tmr-clone
  "e"           #'tmr-edit-description
  "k"           #'tmr-cancel
  "l"           #'tmr-tabulated-view
  "r"           #'tmr-remove
  "s"           #'tmr-reschedule
  "t"           #'tmr)
(evil-define-key 'normal debugger-mode-map
  (kbd "SPC") nil
  (kbd "<escape>") #'quit-window)
(evil-define-key 'normal help-mode-map
  (kbd "SPC") nil
  (kbd "<escape>") #'quit-window)
(evil-define-key 'normal Info-mode-map
  (kbd "SPC") nil
  (kbd "<escape>") #'quit-window)

;; ============================================================================
;;; Leader key
;; ============================================================================
;; I use `which-key' rather than `transient'.
(unless (version< emacs-version "30.1")
  (which-key-mode 1)
  (setq
   which-key-sort-order 'which-key-prefix-then-key-order ; nil prefix works.
   which-key-prefix-prefix nil
   which-key-idle-delay 3.0)
  (which-key-setup-minibuffer))
;; ----------------------------------------------------------------------------
;; Keymaps for my leader key.
(when (file-newer-than-file-p
       (locate-user-emacs-file "keymaps.el")
       (locate-user-emacs-file "keymaps.elc"))
  (byte-compile-file (locate-user-emacs-file "keymaps.el")))
(load (locate-user-emacs-file "keymaps.elc") nil t)
;; ----------------------------------------------------------------------------
;; Leader key is <SPC> with <f9> as backup.
(keymap-set evil-motion-state-map "SPC"  my-root-spc-map)
(keymap-set global-map            "<f9>" my-root-spc-map)
;; ----------------------------------------------------------------------------
;; Additions to existing keymaps.
(keymap-set evil-window-map "d" #'toggle-window-dedicated)
(keymap-set evil-window-map "g" #'golden-ratio-mode)
(keymap-set help-map        "B" #'embark-bindings)
(keymap-set help-map        "F" #'describe-face)
(keymap-set help-map        "G" #'Info-goto-emacs-command-node)
(keymap-set help-map        "c" #'describe-char)
(keymap-set help-map        "h" #'describe-symbol) ; Swap "h" and "o".
(keymap-set help-map        "o" #'view-hello-file)
;; ----------------------------------------------------------------------------
;; <f8> menus. Mainly `transient' from `casual-suite'.
(with-eval-after-load 'calc
  (keymap-set calc-mode-map          "<f8>" #'casual-calc-tmenu)
  (keymap-set calc-alg-map           "<f8>" #'casual-calc-tmenu))
(with-eval-after-load 'dired
  (keymap-set dired-mode-map         "<f8>" #'casual-dired-tmenu))
(with-eval-after-load 'ibuffer
  (keymap-set ibuffer-mode-map       "<f8>" #'casual-ibuffer-tmenu))
(with-eval-after-load 'isearch
  (keymap-set isearch-mode-map       "<f8>" #'casual-isearch-tmenu))
(with-eval-after-load 'org-agenda
  (keymap-set org-agenda-mode-map    "<f8>" #'casual-agenda-tmenu))
(with-eval-after-load 'tmr
  (keymap-set tmr-tabulated-mode-map "<f8>" tmr-prefix-map))

;; ============================================================================
;;; Startup
;; ============================================================================
(my-org-agenda-custom)

;;; init.el ends here
