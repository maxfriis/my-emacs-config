;; -*- lexical-binding: t; -*-
;; #+title: Emacs config init.el

;; ============================================================================
;;; Modern computers are able to handle more than the defaults
;; ============================================================================
;; Garbage collection start at 50MB rather than 800k etc.
;; ----------------------------------------------------------------------------
(setq
 gc-cons-threshold (* 50 1024 1024)
 gc-cons-percentage 0.5 ; Use more memory.
 inhibit-compacting-font-caches t
 read-process-output-max (* 1024 1024))

;; ============================================================================
;;; Maximize the frame
;; ============================================================================
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq frame-resize-pixelwise t)
(set-frame-position nil 0 0)
(set-frame-size     nil (display-pixel-width) (display-pixel-height) t)

;; ============================================================================
;;; Cleanup vanilla defaults
;; ============================================================================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(set-default-coding-systems 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ============================================================================
;;; Modeline
;; ============================================================================
(setq mode-line-buffer-identification-keymap nil)
(setq-default ; modeline need setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   ;; mode-line-modified
   "%*"
   " "
   mode-line-buffer-identification
   " "
   (:eval
    (propertize
     (concat
      "⌞"
      (string-replace ; short major mode
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
     'help-echo  (symbol-name major-mode)
     'mouse-face 'mode-line-highlight))
   (:eval
    (when (mode-line-window-selected-p) ; only active window
      (list
       (if (eq vc-mode nil)
           ""
         (replace-regexp-in-string ; else
          "^ Git" " "
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
       mode-line-misc-info
       " "
       (propertize ; gap for alignment.
        " "
        'display '((space :align-to (- (+ right right-fringe right-margin) 7)))
        'face    'mode-line-inactive)
       " "
       mode-line-percent-position
       mode-line-position-column-format)))))

;; ============================================================================
;;; Vanilla variables
;; ============================================================================
(setq
 inhibit-startup-message t
 visible-bell t
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      "%b")))
 initial-scratch-message
 (concat
  ";; Lisp evaluation and unsaved text.\n"
  ";;  _  _  ____  __    __     __\n"
  ";; / )( \\(  __)(  )  (  )   /  \\\n"
  ";; ) __ ( ) _) / (_/\\/ (_/\\(  O )\n"
  ";; \\_)(_/(____)\\____/\\____/ \\__/\n"
  ";;  _  _   __  ____  __    ____  _\n"
  ";; / )( \\ /  \\(  _ \\(  )  (    \\/ \\\n"
  ";; \\ /\\ /(  O ))   // (_/\\ ) D (\\_/\n"
  ";; (_/\\_) \\__/(__\\_)\\____/(____/(_)\n")
 large-file-warning-threshold nil
 use-dialog-box nil
 use-short-answers t
 delete-by-moving-to-trash t
 trash-directory "~/.trash"
 custom-file "/dev/null"
 dired-listing-switches "-agho --group-directories-first"
 dired-dwim-target t
 dired-recursive-copies 'always
 dired-kill-when-opening-new-dired-buffer t
 global-auto-revert-non-file-buffers t ; update dired buffer
 indent-tabs-mode nil
 tab-width 4
 fill-column 80
 display-line-numbers-type 'relative
 display-buffer-alist
 '(("\\*Help\\*"
    (display-buffer-reuse-mode-window
     display-buffer-below-selected)
    (body-function . select-window)))
 shell-default-shell 'eshell ; I use a terminal outside emacs when needed.
 eshell-ls-initial-args
 '("-agho")
 ;; ----------------------------------------------------------------------------
 ;; Incremental search
 ;; ----------------------------------------------------------------------------
 isearch-lazy-count t
 lazy-count-suffix-format nil
 lazy-count-prefix-format "%s/%s "
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
 '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))

;; ============================================================================
;;; Faces
;; ============================================================================
;; 3 sizes: small (4/5=0.8), normal (1.0) and large (5/4=1.25).
;; 8 colors: #??? combining f, 9 and 0, and some grayish colors.
;; |------+----------------+------+----------------+------+-----------------|
;; | #bba | default text   | #09f | link/timestamp | #0f9 | comment/tags    |
;; | #332 | shadow/hl-line | #9f0 | success/done   | #f90 | warning/heading |
;; | #221 | background     | #f09 | error/todo     | #90f | not used        |
;; |------+----------------+------+----------------+------+-----------------|
;; Keep it simple, systematic, predictable and aesthetically pleasing.
;; ----------------------------------------------------------------------------
;; When I configure evil I add 8 colors reserved for the cursor and depending
;; on the evil state. Unspecified faces are handled by the vanilla theme.
;; ----------------------------------------------------------------------------
(set-face-attribute
 'default                       nil :height 200  :foreground "#bba" :background "#221" :font "Ubuntu Mono")
(set-face-attribute
 'fixed-pitch                   nil                                                    :font "Ubuntu Mono")
(set-face-attribute
 'variable-pitch                nil :height 180                                        :font "Ubuntu")
(set-face-attribute
 'error                         nil              :foreground "#f09")
(set-face-attribute
 'warning                       nil              :foreground "#f90")
(set-face-attribute
 'success                       nil              :foreground "#9f0")
(set-face-attribute
 'shadow                        nil              :foreground "#332")
(set-face-attribute
 'match                         nil                                 :background "#332")
;; ----------------------------------------------------------------------------
;; font-lock faces
;; ----------------------------------------------------------------------------
(set-face-attribute
 'font-lock-constant-face       nil              :foreground "#f90")
(set-face-attribute
 'font-lock-function-name-face  nil              :foreground "#9f0")
(set-face-attribute
 'font-lock-builtin-face        nil              :foreground "#9f0")
(set-face-attribute
 'font-lock-comment-face        nil              :foreground "#0f9")
(set-face-attribute
 'font-lock-string-face         nil              :foreground "#0f9")
(set-face-attribute
 'font-lock-keyword-face        nil              :foreground "#09f")
;; ----------------------------------------------------------------------------
;; Decorations
;; ----------------------------------------------------------------------------
(set-face-attribute
 'highlight                     nil                                 :background "#332")
(set-face-attribute
 'vertical-border               nil              :foreground "#332")
(set-face-attribute
 'fringe                        nil              :foreground "#332" :background "#221")
(set-face-attribute
 'line-number                   nil :height 0.8  :foreground "#332" :background "#221")
(set-face-attribute
 'line-number-current-line      nil              :foreground "#221" :background "#332")
(set-face-attribute
 'mode-line                     nil :height 0.8  :foreground "#221" :background "#332" :box nil)
(set-face-attribute
 'mode-line-inactive            nil              :foreground "#332" :background "#221" :box nil :overline t)
;; ----------------------------------------------------------------------------
;; Tab bar
;; ----------------------------------------------------------------------------
(set-face-attribute
 'tab-bar                       nil :height 0.8  :foreground "#332" :background "#221" :weight 'bold :inherit 'default)
(set-face-attribute
 'tab-bar-tab                   nil              :foreground "#221" :background "#332" :box nil)
(set-face-attribute
 'tab-bar-tab-inactive          nil              :foreground "#332" :background "#221" :box nil)

;; ============================================================================
;;; Color keywords
;; ============================================================================
(font-lock-add-keywords
 'emacs-lisp-mode
 '(("add-hook"           . font-lock-keyword-face)
   ("add-to-list"        . font-lock-keyword-face)
   ("set-face-attribute" . font-lock-keyword-face)))

;; ============================================================================
;;; Use-package
;; ============================================================================
(require 'package)
(setq
 load-prefer-newer t ; use .el if newer than .elc. Fewer temporary compile warnings
 package-archives
 '(("org"   . "https://orgmode.org/elpa/")
   ("elpa"  . "https://elpa.gnu.org/packages/")
   ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
;; ----------------------------------------------------------------------------
;; Update packages automatically
;; ----------------------------------------------------------------------------
(use-package auto-compile
  :ensure t
  :defer nil
  :config (auto-compile-on-load-mode))
(use-package auto-package-update
  :ensure t
  :init
  (setq
   auto-package-update-interval 7
   auto-package-update-hide-results t
   auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-maybe))
;; ----------------------------------------------------------------------------
;; Remember stuff
;; ----------------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :init
  (setq
   undo-tree-visualizer-diff t
   undo-tree-visualizer-timestamps t
   ;; undo-tree-history-directory-alist
   ;; '(("." . "~/.emacs.d/undo"))
   ;; (locate-user-emacs-file) ; will this work with directories?
   undo-tree-auto-save-history t)
  (global-undo-tree-mode 1))
(use-package saveplace
  :ensure t
  :init
  (setq
   save-place-forget-unreadable-files nil)
  (save-place-mode 1))
(use-package savehist
  :ensure t
  :init
  (setq savehist-save-minibuffer-history 1)
  (savehist-mode 1))
;; ----------------------------------------------------------------------------
;; Completion and tips in minibuffer (M-x)
;; ----------------------------------------------------------------------------
(use-package vertico
  :ensure t
  :init
  (setq
   vertico-cycle t
   vertico-resize nil)
  (vertico-mode 1)
  (use-package marginalia
    :ensure t
    :after vertico
    :init
    (marginalia-mode 1)))
(use-package orderless ; Fuzzy completions
  :ensure t
  :init
  (setq
   completion-styles
   '(orderless))) ; (basic substring flex)
;; ----------------------------------------------------------------------------
;; Completion in buffer
;; ----------------------------------------------------------------------------
(use-package company ; Corfu is an alternative
  :ensure t
  :init
  (setq company-minimum-prefix-length 2
        company-idle-delay 0
        company-show-quick-access t)
  (company-tng-configure-default)
  (global-company-mode 1)
  :config
  (use-package company-box
    :ensure t
    :hook
    (company-mode . company-box-mode)))
;; ----------------------------------------------------------------------------
;; Key completion tips
;; ----------------------------------------------------------------------------
(use-package which-key
  :ensure t
  :init
  (setq
   which-key-idle-delay 0)
  (which-key-mode 1))
;; ----------------------------------------------------------------------------
;; Git
;; ----------------------------------------------------------------------------
(use-package magit
  :ensure t
  :commands
  (magit)
  :config
  (with-eval-after-load 'evil
    (add-hook 'git-commit-mode-hook #'evil-insert-state)
    (evil-set-initial-state 'magit-log-edit-mode 'insert)))
;; ----------------------------------------------------------------------------
;; Thumbnails and icons
;; ----------------------------------------------------------------------------
(use-package nerd-icons ; all-the-icons' alignment sucks. Nerd works
  :ensure t
  :if
  (display-graphic-p)
  :config
  (use-package nerd-icons-dired
    :ensure t
    :hook
    (dired-mode . nerd-icons-dired-mode))
  (use-package nerd-icons-ibuffer
    :ensure t
    :hook
    (ibuffer-mode . nerd-icons-ibuffer-mode))
  (use-package nerd-icons-completion
    :ensure t
    :after
    marginalia
    :config
    (nerd-icons-completion-mode 1)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))
;; ----------------------------------------------------------------------------
;; Mark indentation
;; ----------------------------------------------------------------------------
(use-package indent-guide
  :ensure t
  :config
  (set-face-attribute
   'indent-guide-face nil :foreground "#332")
  (add-hook 'prog-mode-hook #'indent-guide-mode))
;; ----------------------------------------------------------------------------
;; Matching parenthesis and color codes
;; ----------------------------------------------------------------------------
(use-package rainbow-delimiters
  :ensure t
  :init
  (setq
   rainbow-delimiters-max-face-count 3)
  :config
  (set-face-attribute 'rainbow-delimiters-base-error-face nil :foreground "#f09" :background "#332" :weight 'bold)
  (set-face-attribute 'show-paren-match                   nil :foreground "#f90" :background "#332" :weight 'bold)
  (set-face-attribute 'rainbow-delimiters-depth-1-face    nil :foreground "#9f0")
  (set-face-attribute 'rainbow-delimiters-depth-2-face    nil :foreground "#09f")
  (set-face-attribute 'rainbow-delimiters-depth-3-face    nil :foreground "#0f9")
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))
;; ----------------------------------------------------------------------------
;; Toggle pitch
;; ----------------------------------------------------------------------------
(use-package mixed-pitch
  :ensure t
  :commands
  (mixed-pitch-mode)
  :config
  (setq mixed-pitch-set-height t)
  (dolist (face '(org-special-keyword org-date org-tag org-priority org-todo org-table))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))
;; ----------------------------------------------------------------------------
;; Toggle narrow
;; ----------------------------------------------------------------------------
(use-package recursive-narrow
  :ensure t)
;; ----------------------------------------------------------------------------
;; Combine functionality (e.g. buffers + recentf)
;; ----------------------------------------------------------------------------
(use-package consult
  :ensure t)
;; ----------------------------------------------------------------------------
;; Navigation: Jump to char
;; ----------------------------------------------------------------------------
(use-package avy
  :ensure t
  :commands
  (evil-avy-goto-char-timer))
;; ----------------------------------------------------------------------------
;; "Scroll lock"
;; ----------------------------------------------------------------------------
(use-package centered-cursor-mode
  :ensure t
  :bind
  (("<Scroll_Lock>" . centered-cursor-mode)
   :map
   ccm-map
   ("<prior>"       . evil-scroll-up)
   ("<next>"        . evil-scroll-down))
  :init
  (global-centered-cursor-mode 1))
;; ----------------------------------------------------------------------------
;; Window manipulation
;; ----------------------------------------------------------------------------
(use-package golden-ratio
  :ensure t
  :init
  (golden-ratio-mode 1))
(use-package ace-window
  :ensure t
  :init
  (setq
   aw-keys
   '(?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
  :config
  (set-face-attribute
   'aw-leading-char-face nil :height 1.25 :foreground "#f90" :weight 'bold))
(use-package transpose-frame
  :ensure t)
;; ----------------------------------------------------------------------------
;; Key casting
;; ----------------------------------------------------------------------------
(use-package keycast
  :ensure t
  :init
  (keycast-tab-bar-mode 1) ; Cast in the tab bar
  :config
  (set-face-attribute
   'keycast-key nil :height 0.8  :foreground "#221" :background "#332" :box nil))
;; ----------------------------------------------------------------------------
;; Better help
;; ----------------------------------------------------------------------------
(use-package counsel
  :ensure t
  :config
  (set-face-attribute
   'ivy-current-match nil :foreground "#9f0" :background "#332"))
(use-package helpful
  :ensure t
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command]  . helpful-command)
  ([remap describe-key]      . helpful-key))
;; ----------------------------------------------------------------------------
;; Spell checking and writing tips
;; ----------------------------------------------------------------------------
(use-package flycheck
  :ensure t
  :hook
  (org-mode . flyspell-mode)
  :commands
  (flyspell-mode))
(use-package writegood-mode
  :ensure t)

;; ============================================================================
;;; Custom functions for general.el
;; ============================================================================
;; Open init file
;; ----------------------------------------------------------------------------
(defun my/init-file ()
  "Open emacs dotfile init.el"
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))
;; ----------------------------------------------------------------------------
;; Open notes file
;; ----------------------------------------------------------------------------
(defun my/note-file ()
  "Open my notes file note.org"
  (interactive)
  (find-file (concat org-directory "/agenda/note.org")))
;; ----------------------------------------------------------------------------
;; Open agenda file
;; ----------------------------------------------------------------------------
(defun my/agenda-file ()
  "Open my agenda file agenda.org"
  (interactive)
  (find-file (concat org-directory "/agenda/agenda.org")))
;; ----------------------------------------------------------------------------
;; Custom agenda
;; ----------------------------------------------------------------------------
(defun my/org-agenda-custom ()
  "My custom agenda with NEXT, agenda and TODO/HOLD"
  (interactive)
  (org-agenda nil "c"))
;; ----------------------------------------------------------------------------
;; Capture idea
;; ----------------------------------------------------------------------------
(defun my/org-capture-idea ()
  "My idea capture to inbox.org"
  (interactive)
  (org-capture nil "i"))
;; ----------------------------------------------------------------------------
;; Ace window swap
;; ----------------------------------------------------------------------------
(defun my/ace-swap-window ()
  "Swap the content of two windows (prompt if 3+), focus the current window"
  (interactive)
  (ace-swap-window)
  (aw-flip-window))

;; ============================================================================
;;; General
;; ============================================================================
(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer my/set-leader-keys
    :keymaps '(motion visual normal insert emacs)
    :prefix "SPC"
    :global-prefix "C-SPC") ; I don't use the Emacs mark.
  (my/set-leader-keys
    "SPC" '(counsel-M-x                            :which-key "M-x")
    ;; this will toggle back and forth between 2 buffers.
    "TAB" '(mode-line-other-buffer                 :which-key "Toggle buf")
    "0"   '(evil-window-delete                     :which-key "Del win")
    "1"   '(delete-other-windows                   :which-key "Max win")
    "2"   '(evil-window-split                      :which-key "Win below")
    "3"   '(evil-window-vsplit                     :which-key "Win right")
    "4"   '(other-window-prefix                    :which-key "Win prefix")
    "5"   '(dired-other-window                     :which-key "Dired win")
    "6"   '(ace-select-window                      :which-key "Select win")
    "7"   '(my/ace-swap-window                     :which-key "Swap win")
    "8"   '(rotate-frame-clockwise                 :which-key "Rot. frame")
    "9"   '(transpose-frame                        :which-key "Transpose")
    "¨"   '(evil-prev-buffer                       :which-key "Prev buf")
    "´"   '(evil-window-prev                       :which-key "Prev win")
    "½"   '(tab-bar-close-tab-by-name              :which-key "Close tab")
    "a"   '(:ignore t                              :which-key "Apps")
    "aC"  '(full-calc                              :which-key "Full calc")
    "ac"  '(calc                                   :which-key "Calc")
    "as"  '(eshell                                 :which-key "Eshell")
    "au"  '(undo-tree-visualize                    :which-key "Undo tree")
    "b"   '(:ignore t                              :which-key "Buffer")
    "bb"  '(consult-buffer                         :which-key "Buffers")
    "bd"  '(evil-delete-buffer                     :which-key "Delete")
    "bi"  '(ibuffer                                :which-key "IBuffer")
    "bj"  '(next-buffer                            :which-key "Next")
    "bk"  '(previous-buffer                        :which-key "Previous")
    "bo"  '(counsel-switch-buffer-other-window     :which-key "Other win")
    "bs"  '(scratch-buffer                         :which-key "Scratch")
    "c"   '(my/org-capture-idea                    :which-key "Catch idea")
    "d"   '(:ignore t                              :which-key "d")
    "e"   '(:ignore t                              :which-key "e")
    "f"   '(:ignore t                              :which-key "Files")
    "fS"  '(save-some-buffers                      :which-key "Save all")
    "fa"  '(my/agenda-file                         :which-key "Agenda")
    "ff"  '(counsel-find-file                      :which-key "Find")
    "fd"  '(dired-jump                             :which-key "Dired")
    "fi"  '(my/init-file                           :which-key "Init")
    "fn"  '(my/note-file                           :which-key "Note")
    "fp"  '(find-file-at-point                     :which-key "At point")
    "fr"  '(recentf-open-files                     :which-key "Recent")
    "fs"  '(evil-save                              :which-key "Save")
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
    "j"   '(evil-avy-goto-char-timer               :which-key "Jump")
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
    "oI"  '(org-clock-in                           :which-key "Clock in")
    "oL"  '(org-store-link                         :which-key "Store link")
    "oO"  '(org-clock-out                          :which-key "Clock out")
    "oR"  '(org-refile                             :which-key "Refile")
    "oS"  '(org-sort                               :which-key "Sort")
    "oa"  '(org-agenda                             :which-key "Agenda")
    "ob"  '(org-insert-structure-template          :which-key "Block")
    "oc"  '(org-capture                            :which-key "Capture")
    "od"  '(org-deadline                           :which-key "Deadline")
    "oe"  '(org-export-dispatch                    :which-key "Export")
    "og"  '(counsel-org-goto-all                   :which-key "Goto task")
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
    "tc"  '(display-fill-column-indicator-mode     :which-key "Column 80")
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
    "v"   '(:ignore t                              :which-key "v")
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
    "å"   '(:ignore t                              :which-key "å")))

;; ============================================================================
;;; Evil
;; ============================================================================
(use-package evil
  :ensure t
  :init
  (setq
   evil-want-keybinding nil ; needed by evil-collection
   evil-want-integration t
   evil-want-C-u-scroll t
   evil-ex-substitute-highlight-all nil
   evil-ex-search-persistent-highlight nil
   evil-shift-round t
   evil-undo-system 'undo-tree
   vim-style-remap-Y-to-y$ t
   ;; ----------------------------------------------------------------------------
   ;; Evil cursors
   ;; ----------------------------------------------------------------------------
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
   evil-operator-state-cursor    '(box        "#f00")
   evil-normal-state-cursor      '(box        "#0f0")
   evil-motion-state-cursor      '(box        "#00f")
   evil-emacs-state-cursor       '((bar  . 4) "#0ff")
   evil-replace-state-cursor     '((hbar . 4) "#f0f")
   evil-insert-state-cursor      '((bar  . 4) "#ff0")
   evil-visual-state-cursor      '(hollow     "#fff"))
  (set-face-attribute 'region nil :background "#000")
  (evil-mode 1)
  :bind
  ;; ----------------------------------------------------------------------------
  ;; Global keys!!! (minor mode maps will overwrite)
  ;; ----------------------------------------------------------------------------
  (("<escape>"  . keyboard-escape-quit)
   ("<next>"    . evil-scroll-down)
   ("<prior>"   . evil-scroll-up)
   ;; ----------------------------------------------------------------------------
   ;; Operator state keys!!!
   ;; ----------------------------------------------------------------------------
   :map
   evil-operator-state-map
   ;; Bad things happen if I hit "å" (next to "ø") in operator state without this
   ("å"         . keyboard-escape-quit)
   ;; ----------------------------------------------------------------------------
   ;; Motion state keys!!! (normal, visual and motion)
   ;; ----------------------------------------------------------------------------
   :map
   evil-motion-state-map
   ("<down>"    . evil-next-visual-line)     ; up/down keys navigate wrapped lines while
   ("<up>"      . evil-previous-visual-line) ; j/k respect the line atom approach in vim.
   ("´"         . evil-window-next)
   ("¨"         . evil-next-buffer)
   ("½"         . tab-new)
   ("C-½"       . tab-bar-mode)
   ("C-<tab>"   . tab-next)
   ("C-S-<tab>" . tab-previous)
   ("æ"         . evil-forward-paragraph)
   ("Æ"         . evil-backward-paragraph)
   ("ø"         . evil-end-of-line)
   ("Ø"         . evil-first-non-blank)
   ("å"         . my/org-agenda-custom)
   ("Å"         . org-agenda)
   ;; ----------------------------------------------------------------------------
   ;; Normal state keys!!!
   ;; ----------------------------------------------------------------------------
   :map
   evil-normal-state-map
   ;; "/" is not easily available on my keyboard so I use "cl" for substitute.
   ;; This also free up "s" for surround in visual state.
   ;; I find myself primarely using isearch and swiper for searching though.
   ("s"         . evil-ex-search-forward)
   ("S"         . evil-ex-search-backward)
   ;; ----------------------------------------------------------------------------
   ;; Insert state keys!!!
   ;; ----------------------------------------------------------------------------
   ;; I come from Emacs so I like to access some navigation in insert state.
   :map
   evil-insert-state-map
   ("C-g"       . evil-normal-state)
   ("C-½"       . tab-bar-mode)
   ("C-<tab>"   . tab-next)
   ("C-S-<tab>" . tab-previous)
   ("C-b"       . evil-backward-word-begin)
   ("C-B"       . evil-backward-WORD-begin)
   ("C-d"       . backward-kill-word) ; Usually "C-w" but I use that for the navigation
   ("C-e"       . forward-word)
   ("C-E"       . forward-word-strictly)
   ("C-p"       . yank)
   ("M-p"       . yank-pop)
   ("C-u"       . universal-argument)
   ("C-w"       . evil-forward-word-begin)
   ("C-W"       . evil-forward-WORD-begin)
   ("C-æ"       . evil-forward-paragraph)
   ("C-Æ"       . evil-backward-paragraph)
   ("C-ø"       . move-end-of-line)
   ("C-Ø"       . evil-first-non-blank)
   ("C-0"       . evil-beginning-of-line))
  :config
  ;; ----------------------------------------------------------------------------
  ;; Keys in maps depending on state!!!
  ;; ----------------------------------------------------------------------------
  ;; Ibuffer don't have a hook
  (evil-define-key
    'emacs ibuffer-mode-map
    (kbd "´") 'evil-window-next
    (kbd "¨") 'evil-next-buffer
    (kbd "å") 'my/org-agenda-custom)
  (use-package evil-collection ; enable evil keybindings in misc modes
    :ensure t
    :after
    evil
    :config
    (evil-collection-init))
  (use-package evil-nerd-commenter
    :ensure t
    :after
    evil
    :bind
    (:map
     evil-motion-state-map
     ("gc"        . evilnc-comment-operator)))
  (use-package evil-numbers
    :ensure t
    :after
    evil
    :bind
    (:map
     evil-normal-state-map
     ("g+" . evil-numbers/inc-at-pt)
     ("g-" . evil-numbers/dec-at-pt)))
  (use-package evil-surround ; start with "ys", then object and delimiter
    :ensure t
    :after
    evil
    :init
    (global-evil-surround-mode 1)
    :bind ; visual state surround keys
    (:map
     evil-visual-state-map
     ("s"         . evil-surround-region)
     ("S"         . evil-Surround-region)))
  ;; ----------------------------------------------------------------------------
  ;; Evil org
  ;; ----------------------------------------------------------------------------
  (use-package evil-org
    :ensure t
    :after
    (org evil)
    :hook
    (org-mode . (lambda () evil-org-mode))
    :config
    (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
    (bind-keys
     :map
     org-mode-map
     ("C-æ" . org-next-visible-heading)
     ("C-Æ" . org-previous-visible-heading))
    ;; ----------------------------------------------------------------------------
    ;; Evil agenda
    ;; ----------------------------------------------------------------------------
    (require
     'evil-org-agenda)
    (evil-org-agenda-set-keys)
    (evil-define-key
      'motion org-agenda-mode-map
      (kbd "SPC")       'org-agenda-quit ; C-SPC for the general package
      (kbd "´")         'evil-window-next
      (kbd "¨")         'evil-next-buffer
      (kbd "a")         'org-agenda-append-agenda
      (kbd "A")         'org-agenda-archive-default-with-confirmation
      (kbd "R")         'org-agenda-refile
      (kbd "gt")        'org-agenda-goto-today
      (kbd "gw")        'org-agenda-week-view
      (kbd "gy")        'org-agenda-year-view
      (kbd "gh")        'org-agenda-earlier
      (kbd "gl")        'org-agenda-later
      (kbd "n")         'org-agenda-add-note
      (kbd "Æ")         'org-agenda-backward-block
      (kbd "æ")         'org-agenda-forward-block
      (kbd "å")         'org-agenda-columns
      (kbd "½")         'tab-new
      (kbd "C-½")       'tab-bar-mode
      (kbd "C-<tab>")   'tab-next
      (kbd "C-S-<tab>") 'tab-previous)))

;; ============================================================================
;;; Org
;; ============================================================================
(use-package org
  :ensure t
  :init
  (setq
   org-directory "~/org"
   org-default-notes-file (concat org-directory "/inbox.org")
   org-ellipsis " …"
   ;; ----------------------------------------------------------------------------
   ;; Todo states: I use :category: and refile rather than more keywords.
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
   ;; Capture
   ;; ----------------------------------------------------------------------------
   org-capture-templates
   `(("i" "Idea" entry ; backtick to concat
      (file ,(concat org-directory "/inbox.org"))
      ,(concat
        "* NEXT %^{Idea} %^G\n"
        ":LOGBOOK:\n"
        "- State \"NEXT\" from \"Capture\" %U\n"
        ":END:\n"
        "%i")
      :immediate-finish t :prepend t)
     ("t" "Task" entry
      (file+olp ,(concat org-directory "/agenda/agenda.org") "Task")
      ,(concat
        "* TODO %^{Do what?} %^G\n"
        ":LOGBOOK:\n"
        "- State \"TODO\" from \"Capture\" %U\n"
        ":END:\n"
        "%?%i"))
     ("m" "Meet" entry
      (file+olp ,(concat org-directory "/agenda/agenda.org") "Meet")
      ,(concat
        "* TODO %^{Meet who?} %^G\n"
        "SCHEDULED: %^{When?}t\n"
        ":LOGBOOK:\n"
        "- State \"TODO\" from \"Capture\" %U\n"
        ":END:\n"
        "%?%i"))
     ("n" "Note" entry
      (file+datetree ,(concat org-directory "/agenda/note.org"))
      ,(concat
        "* %U\n"
        "%?%i")
      :tree-type month)
     ("x" "Log clipboard" entry
      (file+olp+datetree ,(concat org-directory "/archive.org") "Clipboard")
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
   ;; This is a system to organize and supress information about tasks.
   ;; It's not primarily a calendar. It's all about task management.
   org-agenda-window-setup 'current-window
   org-archive-location (concat org-directory "/archive.org::* Archive")
   org-refile-targets
   `((,(concat org-directory "/agenda/agenda.org") :maxlevel . 1))
   org-agenda-files (list (concat org-directory "/agenda")
                          (concat org-directory "/inbox.org")
                          (concat org-directory "/bgbog/bg.org"))
   org-agenda-format-date "  [%F %a]"
   org-agenda-block-separator ?⎺ ; also separate "a" appended agenda
   org-agenda-custom-commands
   '(("c" "Custom agenda setup"
      ((todo "NEXT"
             ;; All NEXT (including timestamped and priority).
             ((org-agenda-overriding-header "")))
       (todo "TODO|HOLD|DONE"
             ;; Prioritized TODO, HOLD or even DONE.
             ((org-agenda-overriding-header "")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if
                 'notregexp org-priority-regexp))))
       (agenda "" ((org-agenda-span 3)))
       (todo "TODO"
             ;; Not every TODO has or even should have a timestamp.
             ((org-agenda-overriding-header "No timestamp TODO or HOLD:")
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if
                 'regexp org-priority-regexp
                 'timestamp))))
       (todo "HOLD"
             ;; HOLD for third party action pending (include timestamped).
             ((org-agenda-overriding-header "") ; share heading with the item above.
              (org-agenda-block-separator nil) ; don't separate
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if
                 'regexp org-priority-regexp)))))))
   org-agenda-prefix-format
   '((agenda   . "  %-6c%-12t%?-11s")
     (timeline . "  %-6c%-12t%?-11s")
     (todo     . "  %-6c%-12e")
     (tags     . "  %-6c%-12e")
     (search   . "  %-6c%-12e"))
   org-agenda-time-grid nil
   org-agenda-current-time-string nil
   org-agenda-start-with-log-mode t
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
     ("\\.pdf\\'"     . "evince %s")
     (directory       . emacs)
     (auto-mode       . emacs)))
  :config
  ;; ----------------------------------------------------------------------------
  ;; Normal state is for editing. To input raw text, insert state is prefferable.
  ;; ----------------------------------------------------------------------------
  ;; org-log-buffer-setup-hook works on org-add-note which has no hook, but it
  ;; also works on other notes. It's like insert state after pressing "o".
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)
  (add-hook 'org-capture-mode-hook     #'evil-insert-state)
  ;; ----------------------------------------------------------------------------
  ;; ToDo keybindings
  ;; ----------------------------------------------------------------------------
  (add-hook
   'org-mode-hook
   (lambda ()
     (bind-keys
      :map
      evil-normal-state-map
      ("t"           . org-todo)
      ("T"           . org-todo-yesterday))))
  ;; ----------------------------------------------------------------------------
  ;; Org faces
  ;; ----------------------------------------------------------------------------
  (set-face-attribute
   'org-document-title            nil :height 1.25 :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-1                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-2                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-3                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-4                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-5                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-6                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-7                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-level-8                   nil :height 1.0  :foreground "#f90" :weight 'bold)
  (set-face-attribute
   'org-headline-done             nil              :foreground "#0f9")
  (set-face-attribute
   'org-document-info-keyword     nil :height 0.8  :foreground "#0f9")
  (set-face-attribute
   'org-done                      nil :height 0.8  :foreground "#9f0")
  (set-face-attribute
   'org-todo                      nil :height 0.8  :foreground "#f09")
  (set-face-attribute
   'org-link                      nil              :foreground "#09f")
  (set-face-attribute
   'org-date                      nil :height 0.8  :foreground "#09f" :underline nil)
  (set-face-attribute
   'org-footnote                  nil :height 0.8  :foreground "#09f" :underline nil)
  (set-face-attribute
   'org-drawer                    nil :height 0.8  :foreground "#09f")
  (set-face-attribute
   'org-block-begin-line          nil :height 0.8  :foreground "#09f")
  (set-face-attribute
   'org-block-end-line            nil              :foreground "#09f")
  (set-face-attribute
   'org-block                     nil              :foreground "#bba")
  (set-face-attribute
   'org-ellipsis                  nil :height 0.8  :foreground "#0f9" :weight 'normal :underline nil)
  (set-face-attribute
   'org-document-info-keyword     nil :height 0.8  :foreground "#0f9" :weight 'normal)
  (set-face-attribute
   'org-special-keyword           nil :height 0.8  :foreground "#0f9" :weight 'normal)
  (set-face-attribute
   'org-checkbox                  nil :height 0.8  :foreground "#0f9" :background "#221" :box nil)
  (set-face-attribute
   'org-tag                       nil              :foreground "#0f9" :weight 'normal)
  (set-face-attribute
   'org-formula                   nil :height 0.8  :foreground "#0f9")
  (set-face-attribute
   'org-table                     nil :height 0.8  :foreground "#0f9")
  (set-face-attribute
   'org-meta-line                 nil :height 0.8)
  ;; ----------------------------------------------------------------------------
  ;; Agenda faces
  ;; ----------------------------------------------------------------------------
  (set-face-attribute
   'org-agenda-structure          nil :height 1.0  :foreground "#f90" :background "#221" :box nil :weight 'bold)
  (set-face-attribute
   'org-agenda-date               nil              :foreground "#09f" :background "#221" :box nil :weight 'normal)
  (set-face-attribute
   'org-agenda-date-weekend       nil              :foreground "#09f" :background "#221" :box nil :weight 'normal :underline nil)
  (set-face-attribute
   'org-agenda-date-today         nil              :foreground "#09f" :background "#221" :box nil :weight 'normal :slant 'normal :inverse-video nil)
  (set-face-attribute
   'org-agenda-current-time       nil              :foreground "#09f")
  (set-face-attribute
   'org-agenda-done               nil              :foreground "#0f9" :slant 'normal)
  (set-face-attribute
   'org-agenda-calendar-event     nil              :foreground "#bba")
  (set-face-attribute
   'calendar-weekday-header       nil              :foreground "#0f9")
  (set-face-attribute
   'calendar-weekend-header       nil              :foreground "#0f9")
  (set-face-attribute
   'org-time-grid                 nil              :foreground "#0f9")
  (set-face-attribute
   'org-warning                   nil              :foreground "#f09")
  (set-face-attribute
   'org-upcoming-distant-deadline nil              :foreground "#9f0")
  (set-face-attribute
   'org-upcoming-deadline         nil              :foreground "#9f0")
  (set-face-attribute
   'org-imminent-deadline         nil              :foreground "#f90" :weight 'normal)
  (set-face-attribute
   'org-scheduled                 nil              :foreground "#9f0")
  (set-face-attribute
   'org-scheduled-today           nil              :foreground "#9f0")
  (set-face-attribute
   'org-scheduled-previously      nil              :foreground "#f90")
  ;; ----------------------------------------------------------------------------
  ;; A pictogram is often better than a word
  ;; ----------------------------------------------------------------------------
  (add-hook
   'org-mode-hook
   (lambda ()
     (setq
      prettify-symbols-alist ; utf8's that work with most fonts even in the terminal.
      '(("[-]"            . ?⊟) ; Not a "ballot" icon.
        ("[ ]"            . ?☐)
        ("[X]"            . ?☒)
        ("CLOSED:"        . ?☑) ; The checkmark displays different in some fonts.
        ("SCHEDULED:"     . ?☐) ; That's OK because the "CLOSED:" inactive timestamp
        ("DEADLINE:"      . ?☒) ; is different than the active timestamps.
        (":PROPERTIES:"   . ?⚙) ; Settings.
        (":LOGBOOK:"      . ?☰) ; Meta data.
        ("CLOCK:"         . ?–) ; Items in logbook have a dash bullet.
        (":END:"          . ?✐)
        ("#+begin_export" . ?✎)
        ("#+end_export"   . ?✐)
        ("#+begin_src"    . ?✎)
        ("#+end_src"      . ?✐))
      prettify-symbols-unprettify-at-point t)
     (prettify-symbols-mode 1)))
  ;; ----------------------------------------------------------------------------
  ;; Bullets
  ;; ----------------------------------------------------------------------------
  (use-package org-superstar
    :ensure t
    :init
    (setq
     org-superstar-cycle-headline-bullets nil
     org-superstar-headline-bullets-list
     '(?① ?② ?③ ?④ ?ⓧ))
    :hook
    (org-mode . org-superstar-mode)
    :config
    (set-face-attribute
     'org-superstar-leading         nil :height 0.8  :foreground "#332")  ; the dots marking the deapt
    (set-face-attribute
     'org-superstar-item            nil :height 0.8  :foreground "#f90")) ; the bullet face
  ;; ----------------------------------------------------------------------------
  ;; Habits
  ;; ----------------------------------------------------------------------------
  (use-package org-appear
    :ensure t
    :init
    (add-to-list 'org-modules 'org-habit)
    (setq
     org-habit-preceding-days 28
     org-habit-graph-column   60
     org-appear-autolinks t)
    :hook
    (org-mode . org-appear-mode)
    :config
    ;; ----------------------------------------------------------------------------
    ;; Habit faces
    ;; ----------------------------------------------------------------------------
    (set-face-attribute
     'org-habit-alert-face          nil :height 0.8  :foreground "#f09" :background "#f90" :weight 'bold)
    (set-face-attribute
     'org-habit-alert-future-face   nil :height 0.8                     :background "#f90")
    (set-face-attribute
     'org-habit-overdue-face        nil :height 0.8  :foreground "#f90" :background "#f09")
    (set-face-attribute
     'org-habit-overdue-future-face nil :height 0.8                     :background "#f09")
    (set-face-attribute
     'org-habit-ready-face          nil :height 0.8  :foreground "#f90" :background "#9f0")
    (set-face-attribute
     'org-habit-ready-future-face   nil :height 0.8                     :background "#9f0")
    (set-face-attribute
     'org-habit-clear-face          nil :height 0.8  :foreground "#f90" :background "#332")
    (set-face-attribute
     'org-habit-clear-future-face   nil :height 0.8                     :background "#332"))
  ;; ----------------------------------------------------------------------------
  ;; Present
  ;; ----------------------------------------------------------------------------
  (use-package org-present
    :ensure t))

;; ============================================================================
;;; Vanilla (global) modes
;; ============================================================================
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)

;; ============================================================================
;;; Dirty hacks
;; ============================================================================
;; I like the cursor below the initial scratch buffer message.
(kill-buffer "*scratch*")
(scratch-buffer)
;; Render font colors correctly in org buffers loaded by agenda.
(sit-for .01) ; A short pause is needed for some reason
(add-hook 'after-init-hook  #'my/org-agenda-custom)

;; ============================================================================
;;; Vanilla hooks
;; ============================================================================
(add-hook 'before-save-hook #'whitespace-cleanup)
(add-hook 'dired-mode-hook  #'dired-hide-details-mode)
(add-hook 'text-mode-hook   #'display-line-numbers-mode) ; Where to jump to?
(add-hook 'text-mode-hook   #'visual-line-mode)
(add-hook
 'prog-mode-hook
 (lambda () (setq indent-tabs-mode nil)))
(add-hook
 'emacs-startup-hook
 (lambda ()
   (message
    "Emacs ready in %s with %d garbage collections."
    (format
     "%.1f seconds"
     (float-time (time-subtract after-init-time before-init-time)))
    gcs-done)))
;; (byte-recompile-directory package-user-dir nil 'force)
