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
 gc-cons-percentage 0.5 ; Use more memory.
 read-process-output-max (* 1024 1024)
 inhibit-startup-message t
 inhibit-compacting-font-caches t
 frame-inhibit-implied-resize t) ; Resizing based on font change is expensive

;; ============================================================================
;;; Maximize the frame
;; ============================================================================
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (setq
;;  frame-resize-pixelwise t)
;; (set-frame-position nil 0 0)
;; (set-frame-size     nil (display-pixel-width) (display-pixel-height) t)

;; ============================================================================
;;; Cleanup vanilla defaults
;; ============================================================================
(blink-cursor-mode -1)
(scroll-bar-mode   -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(tooltip-mode      -1)
(set-default-coding-systems 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)

;; ============================================================================
;;; Dark background
;; ============================================================================
(if (eq (user-uid) 0) ; Different bg color as root
    (set-face-attribute
     'default
     nil :height 160  :foreground "#bba" :background "#311" :font "Ubuntu Mono")
  ;; else
  (set-face-attribute
   'default
   nil :height 160  :foreground "#bba" :background "#221" :font "Ubuntu Mono"))
