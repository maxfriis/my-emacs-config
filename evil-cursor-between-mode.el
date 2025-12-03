;;; evil-cursor-between-mode.el --- Emacs' cursor model in evil-mode -*- lexical-binding: t; -*-

;; ============================================================================
;;; License:
;; ============================================================================
;; Creative Commons Attribution-ShareAlike 4.0 International License
;; [[https://creativecommons.org/licenses/by-sa/4.0/]]

;; A special thanks to Toby Cubitt who coded the cursor model.
;; Peter Friis Jensen made it a mode and swapped three keybindings.

;; Author: Toby Cubitt
;; Maintainer: Peter Friis Jensen <maxfriis@gmail.com>
;; URL: https://github.com/maxfriis/evil-cursor-between-mode
;; Created: 2025-11-15
;; Version: 0.1.0
;; Keywords: convenience, files
;; Package-Requires: ((emacs "24.4") (evil "0"))

;; TODO: Make keybindings respect user configuration. I want a map where the
;; user can rebind "A" so it is not affected by toggling the mode.
;; The challange is that minor-mode maps override maps like `dired-mode-map'.
;; I want a map like `global-map' that get overridden by major-mode maps.
;; I don't know how. Maybe it's not even the right solution. I want it to be
;; able to handle an unknown major mode so I can't handle individual modes.
;; I can't make (use-global-map...) work and it's not recommended.
;; And the global-map is dynamic and hard to keep track of for the toggle.
;; For now bindings are hard coded and toggling the mode will rebind them.

;; ============================================================================
;;; Commentary:
;; ============================================================================
;; Emacs' cursor between characters model for cursor positioning in
;; `evil-mode' instead of Vim's normal-state cursor on characters model.

;; ============================================================================
;;; Code:
;; ============================================================================
(unless (package-installed-p 'evil)
  (package-install 'evil))
(require 'evil)

(defvar evil-cursor-between-move-cursor-back-init evil-move-cursor-back
  "For toggling the variable with `evil-cursor-between-mode'.")
(defvar evil-cursor-between-move-beyond-eol-init evil-move-beyond-eol
  "For toggling the variable with `evil-cursor-between-mode'.")
(defvar evil-cursor-between-highlight-closing-paren-at-point-states-init evil-highlight-closing-paren-at-point-states
  "For toggling the variable with `evil-cursor-between-mode'.")

;; Experimental code:
;; (defvar evil-cursor-between-mode-map (make-sparse-keymap)
;;   "Keymap for `evil-cursor-between-mode'.")
;; (add-to-list 'minor-mode-map-alist (cons 'evil-cursor-between-mode
;;                                          'evil-cursor-between-mode-map)) ; This will override major mode maps.
;; (add-to-list 'minor-mode-overriding-map-alist (cons 'evil-cursor-between-mode
;;                                                     'dired-mode-map)) ; Everything seems to be tuned to increase map priority.
;; ;; ----------------------------------------------------------------------------
;; ;; Motion commands "w"/"W", "b"/"B", "F" and "T" works out of the evil box.
;; (evil-define-key 'motion evil-cursor-between-mode-map
;;   "t"  #'evil-find-char
;;   "f"  #'evil-cursor-between-find-char-after
;;   "e"  #'evil-cursor-between-forward-after-word-end
;;   "E"  #'evil-cursor-between-forward-after-WORD-end
;;   "ge" #'evil-cursor-between-backward-after-word-end
;;   "gE" #'evil-cursor-between-backward-after-WORD-end
;;   "%"  #'evil-cursor-between-jump-after-item)
;; ;; ----------------------------------------------------------------------------
;; ;; Swap "a", "o" and "p" with their capital bindings.
;; (evil-define-key 'normal evil-cursor-between-mode-map
;;   "a"  #'evil-append-line  ; Swapped so append is used to edit from eol.
;;   "A"  #'evil-append       ; Useless in this mode. "li" makes more sense.
;;   "o"  #'evil-open-above   ; Swapped to be consistent with paste and e.g."cc".
;;   "O"  #'evil-open-below   ; "jo" and "a<RET>" does the same thing.
;;   "p"  #'evil-paste-before ; Swapped because almost only "p" is used to paste.
;;   "P"  #'evil-paste-after) ; "jp" or "lp" does the same thing.
;; (defvar evil-cursor-between-global-map (make-composed-keymap evil-cursor-between-mode-map global-map)
;;   "Global keymap for `evil-cursor-between-mode'.")
;; (use-global-map evil-cursor-between-global-map) ; I can't make working with two global maps work.

;; ============================================================================
;;; The minor mode
;; ============================================================================
(define-minor-mode evil-cursor-between-mode
  "Mode for using Emacs' cursor model in `evil-mode's normal state.
\nThe mode swap \"a\"/\"A\", \"o\"/\"O\" and \"p\"/\"P\" compared to Vim's normal state keys.
The idea is to avoid the <shift> layer when dealing with the current line.
Layers can then be replaced with a motion with equivalent efficiency.
\nEmbrace the mindset of Emacs' cursor model and motions among line nuggets.
Maybe fewer layers are better for your Emacs pinky?"
  :global t ; Without this the mode will not sync up with it's variable.
  ;; :keymap 'evil-cursor-between-mode-map
  :group 'evil
  :lighter nil
  (cond
   (evil-cursor-between-mode
    (unless evil-mode
      (evil-mode 1))
    ;; ----------------------------------------------------------------------------
    ;; Cursor related `evil-mode' settings.
    (setq
     evil-move-cursor-back nil
     evil-move-beyond-eol t
     evil-highlight-closing-paren-at-point-states nil)
    ;; ----------------------------------------------------------------------------
    ;; Motion commands "w", "b", "F" and "T" works out of the evil box.
    (evil-define-key 'motion global-map
      "t"  #'evil-find-char
      "f"  #'evil-cursor-between-find-char-after
      "e"  #'evil-cursor-between-forward-after-word-end
      "E"  #'evil-cursor-between-forward-after-WORD-end
      "ge" #'evil-cursor-between-backward-after-word-end
      "gE" #'evil-cursor-between-backward-after-WORD-end
      "%"  #'evil-cursor-between-jump-after-item)
    ;; ----------------------------------------------------------------------------
    ;; Swap "a", "o" and "p" with their capital bindings.
    (evil-define-key 'normal global-map
      "a"  #'evil-append-line   ; swapped because it's only used to edit from eol.
      "A"  #'evil-append        ; "li" does the same thing.
      "o"  #'evil-open-above    ; swapped to be consistent with paste.g. "cc".
      "O"  #'evil-open-below    ; "jo" does the same thing.
      "p"  #'evil-paste-before  ; swapped because only "p" is used to paste.
      "P"  #'evil-paste-after) ; "jp" or "lp" does the same thing.
    ;; ----------------------------------------------------------------------------
    ;; Swap "a" and "o" with their `evil-org-mode' capital bindings.
    (evil-define-key 'normal 'evil-org-mode
      "a"  #'evil-org-append-line
      "A"  nil
      "o"  #'evil-org-open-above
      "O"  #'evil-org-open-below))
   (t ; else
    ;; ----------------------------------------------------------------------------
    ;; Back to `evil-mode' defaults when `evil-cursor-between-mode' is disabled.
    (setq
     evil-move-cursor-back evil-cursor-between-move-cursor-back-init
     evil-move-beyond-eol evil-cursor-between-move-beyond-eol-init
     evil-highlight-closing-paren-at-point-states evil-cursor-between-highlight-closing-paren-at-point-states-init)
    ;; ----------------------------------------------------------------------------
    ;; Motion commands back to evil defaults.
    (evil-define-key 'motion global-map
      "t"  #'evil-find-char-to
      "f"  #'evil-find-char
      "e"  #'evil-forward-word-end
      "E"  #'evil-forward-WORD-end
      "ge" #'evil-backward-word-end
      "gE" #'evil-backward-WORD-end
      "%"  #'evil-jump-item)
    ;; ----------------------------------------------------------------------------
    ;; Swap "a", "o" and "p" back to evil defaults.
    (evil-define-key 'normal global-map
      "a"  #'evil-append
      "A"  #'evil-append-line
      "o"  #'evil-open-below
      "O"  #'evil-open-above
      "p"  #'evil-paste-after
      "P"  #'evil-paste-before)
    ;; ----------------------------------------------------------------------------
    ;; Swap "a" and "o" back to their `evil-org-mode' defaults.
    (evil-define-key 'normal 'evil-org-mode
      "a"  nil
      "A"  #'evil-org-append-line
      "o"  #'evil-org-open-below
      "O"  #'evil-org-open-above))))

;; ============================================================================
;;; Evil commands implementing Emacs' cursor model
;; ============================================================================
(evil-define-motion evil-cursor-between-find-char-after (count char)
  "Move point immediately after the next COUNT'th occurrence of CHAR.
Movement is restricted to the current line unless `evil-cross-lines' is non-nil."
  :type inclusive
  (interactive "<c><C>")
  (unless count (setq count 1))
  (if (< count 0)
      (evil-find-char-backward (- count) char)
    (when (= (char-after) char)
      (forward-char)
      (cl-decf count))
    (evil-find-char count char)
    (forward-char))
  (setq evil-last-find (list #'evil-cursor-between-find-char-after char (> count 0))))

(defun evil-cursor-between-forward-after-end (thing &optional count)
  "Move forward to end of THING.
The motion is repeated COUNT times."
  (setq count (or count 1))
  (cond
   ((> count 0)
    (forward-thing thing count))
   (t
    (unless (bobp) (forward-char -1))
    (let ((bnd (bounds-of-thing-at-point thing))
          rest)
      (when bnd
        (cond
         ((< (point) (cdr bnd)) (goto-char (car bnd)))
         ((= (point) (cdr bnd)) (cl-incf count))))
      (condition-case nil
          (when (zerop (setq rest (forward-thing thing count)))
            (end-of-thing thing))
        (error))
      rest))))

(defun evil-cursor-between-backward-after-end (thing &optional count)
  "Move backward to end of THING.
The motion is repeated COUNT times.  This is the same as calling
`evil-cursor-between-forward-after-word-end' with -COUNT."
  (evil-cursor-between-forward-after-end thing (- (or count 1))))

(evil-define-motion evil-cursor-between-forward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT'th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word))
        (count (or count 1)))
    (evil-signal-at-bob-or-eob count)
    (evil-cursor-between-forward-after-end thing count)))

(evil-define-motion evil-cursor-between-forward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT'th next WORD."
  :type inclusive
  (evil-cursor-between-forward-after-word-end count t))

(evil-define-motion evil-cursor-between-backward-after-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT'th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((thing (if bigword 'evil-WORD 'evil-word)))
    (evil-signal-at-bob-or-eob (- (or count 1)))
    (evil-cursor-between-backward-after-end thing count)))

(evil-define-motion evil-cursor-between-backward-after-WORD-end (count)
  "Move the cursor to the end of the COUNT'th previous WORD."
  :type inclusive
  (evil-cursor-between-backward-after-word-end count t))

;; ----------------------------------------------------------------------------
;;;; Redefine inclusive motion type to not include character after point.
(evil-define-type inclusive
  "Return the positions unchanged, with some exceptions.
If the end position is at the beginning of a line, then:

* If the beginning position is at or before the first non-blank
  character on the line, return `line' (expanded)."
  :expand (lambda (beg end) (evil-range beg end))
  :contract (lambda (beg end) (evil-range beg end))
  :normalize (lambda (beg end)
               (cond
                ((progn
                   (goto-char end)
                   (and (/= beg end) (bolp)))
                 (setq end (max beg (1- end)))
                 (cond
                  ((progn
                     (goto-char beg)
                     (looking-back "^[\f\s\t\v]*" (line-beginning-position)))
                   (evil-expand beg end 'line))
                  (t
                   (unless evil-cross-lines
                     (setq end (max beg (1- end))))
                   (evil-expand beg end 'inclusive))))
                (t
                 (evil-range beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

;; ----------------------------------------------------------------------------
;;;; Make "e" search offset put point after last character.
(defun evil-cursor-between-ad-evil-ex-search-adjust-offset (offset)
  "Make `evil-mode's \"e\" search OFFSET put point after last character."
  (unless (zerop (length offset))
    (save-match-data
      (string-match
       "^\\([esb]\\)?\\(\\([+-]\\)?\\([0-9]*\\)\\)$"
       offset)
      (when (and (= (aref offset (match-beginning 1)) ?e)
                 (not (bobp)))
        (forward-char 1)))))

(advice-add
 'evil-ex-search-goto-offset
 :after #'evil-cursor-between-ad-evil-ex-search-adjust-offset)

;; ----------------------------------------------------------------------------
;;;; `evil-jump-item' move point after matching delimeter if it jumps forward.
(evil-define-motion evil-cursor-between-jump-after-item (count)
  "Find the next item in this line immediately before
or somewhere after the cursor and jump to the corresponding one."
  :jump t
  :type inclusive
  (let ((pos (point)))
    (unless (or (bolp) (bobp)) (backward-char))
    (condition-case nil
        (evil-jump-item count)
      (user-error (goto-char pos)))
    (unless (< (point) pos)
      (goto-char pos)
      (evil-jump-item count)
      (when (> (point) pos) (forward-char)))))

(provide 'evil-cursor-between-mode)
;;; evil-cursor-between-mode.el ends here
