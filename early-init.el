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
 ;; The recommended max for my system:
 read-process-output-max (* 1024 1024)
 ;; Recalculating frame size is expencive if fonts/modeline change or something.
 frame-inhibit-implied-resize t
 ;; Trigger fewer garbage collections. Use more memory.
 inhibit-compacting-font-caches t)

;; ============================================================================
;;; Vanilla defaults
;; ============================================================================
;; I clean up some defaults I don't want to see even blink during startup.
;; ----------------------------------------------------------------------------
(setq
 inhibit-startup-message t
 frame-title-format "GNU Emacs"
 mode-line-format nil
 default-frame-alist
 '((fullscreen       . maximized)
   (background-color . "#000")
   (cursor-color     . "#0f0")))
(defalias 'yes-or-no-p 'y-or-n-p)
(scroll-bar-mode   -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(tooltip-mode      -1)
(blink-cursor-mode -1)
(set-default-coding-systems 'utf-8)
;; End of early-init.el
