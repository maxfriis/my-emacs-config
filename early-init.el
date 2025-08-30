;; -*- lexical-binding: t; -*-
;; #+title: Emacs early-init.el

;; ============================================================================
;;; Early variables
;; ============================================================================
;; Modern computers are able to handle more than the defaults
;; Garbage collection start at 50MB rather than 800k etc.
;; ----------------------------------------------------------------------------
(setq
 gc-cons-threshold (* 50 1024 1024)
 gc-cons-percentage 0.5
 ;; The recommended max read chunk for my system:
 read-process-output-max (* 1024 1024)
 ;; Recalculating frame size is expencive if fonts/modeline change or something.
 frame-inhibit-implied-resize t
 ;; Trigger fewer garbage collections. Use more memory.
 inhibit-compacting-font-caches t)
 ;; ;; Disable some compilation warnings.
 ;; (setq native-comp-async-report-warnings-errors nil)

;; ============================================================================
;;; Vanilla defaults
;; ============================================================================
;; I clean up some defaults I don't want to see even blink during startup.
;; ----------------------------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq
 inhibit-startup-message t
 mode-line-format nil
 frame-title-format "GNU Emacs"
 default-frame-alist
 '((fullscreen       . maximized)
   (foreground-color . "#fff")
   (background-color . "#000")
   (cursor-color     . "#0f0")))
(scroll-bar-mode   -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(tooltip-mode      -1)
(blink-cursor-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
;; End of early-init.el
