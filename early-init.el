;;; early-init.el --- -*- lexical-binding: t; -*-

;; ============================================================================
;;; Variables
;; ============================================================================
(setq
 ;; The recommended max read chunk for my system.
 read-process-output-max (* 1024 1024)
 ;; Garbage collection start at 50MB rather than 800k etc.
 gc-cons-threshold (* 50 1024 1024)
 gc-cons-percentage .5
 ;; Recalculating frame size is expencive if fonts/modeline change.
 frame-inhibit-implied-resize t
 ;; Trigger fewer garbage collections but use more memory.
 inhibit-compacting-font-caches t
 ;; Clean defaults I don't want.
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
(blink-cursor-mode -1)
(menu-bar-mode     -1)
(scroll-bar-mode   -1)
(tool-bar-mode     -1)
(tooltip-mode      -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-default-coding-systems 'utf-8)
;;; early-init.el ends here
