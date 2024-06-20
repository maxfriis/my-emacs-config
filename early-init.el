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
 read-process-output-max (* 16 1024 1024)
 inhibit-startup-message t
 inhibit-compacting-font-caches t
 frame-inhibit-implied-resize t
 frame-title-format "GNU Emacs"
 mode-line-format nil)

;; ============================================================================
;;; Maximize and dark background
;; ============================================================================
(setq
 default-frame-alist
 '((fullscreen       . maximized)
   (background-color . "#221")))

;; ============================================================================
;;; Cleanup vanilla defaults
;; ============================================================================
(defalias 'yes-or-no-p 'y-or-n-p)
(set-default-coding-systems 'utf-8)
(scroll-bar-mode   -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(tooltip-mode      -1)
(blink-cursor-mode -1)
