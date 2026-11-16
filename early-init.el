;;; early-init.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Variables
;; ============================================================================
(setq
 ;; The recommended max read chunk for my system.
 read-process-output-max (* 1024 1024)
 ;; Garbage collection start at 50MB rather than 800k.
 gc-cons-threshold (* 50 1024 1024)
 gc-cons-percentage .5
 ;; Trigger fewer garbage collections but use more memory.
 inhibit-compacting-font-caches t
 ;; Recalculating frame size is expencive if fonts/modeline change.
 frame-inhibit-implied-resize t
 ;; ----------------------------------------------------------------------------
 ;; Defaults I don't want to see.
 inhibit-startup-message t
 mode-line-format nil
 frame-title-format "GNU Emacs"
 initial-frame-alist
 '((fullscreen       . maximized)
   (foreground-color . "#fff")
   (background-color . "#000")
   (cursor-color     . "#0f0")))

;; ============================================================================
;;; Default modes
;; ============================================================================
(blink-cursor-mode 0)
(menu-bar-mode     0)
(scroll-bar-mode   0)
(tool-bar-mode     0)
(tooltip-mode      0)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-default-coding-systems 'utf-8)

;;; early-init.el ends here
