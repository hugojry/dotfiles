;;; hy-frame.el --- Frame functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs on XWayland can render fonts with subpixel aa. PGTK
;;; cannot. Resizing frames does not work on XWayland, hence these
;;; functions.
;;; Code:

(require 'general)

(defun hy/set-frame-size (rows columns)
  (interactive "nRows \nnColumns ")
  (set-frame-size (selected-frame) columns rows))

(defvar hy/frame-resize-increment 5)

(defvar-keymap hy/frame-resize-repeat-map
  :doc "Keymap of all repeatable frame resizing commands."
  :repeat t
  "u" #'hy/decrease-frame-height
  "d" #'hy/increase-frame-height
  "[" #'hy/decrease-frame-width
  "]" #'hy/increase-frame-width)

(defun hy/increase-frame-height ()
  (interactive)
  (set-frame-size (selected-frame)
                  (frame-width)
                  (+ (frame-height) hy/frame-resize-increment)))

(defun hy/decrease-frame-height ()
  (interactive)
  (set-frame-size (selected-frame)
                  (frame-width)
                  (- (frame-height) hy/frame-resize-increment)))

(defun hy/increase-frame-width ()
  (interactive)
  (set-frame-size (selected-frame)
                  (+ (frame-width) hy/frame-resize-increment)
                  (frame-height)))

(defun hy/decrease-frame-width ()
  (interactive)
  (set-frame-size (selected-frame)
                  (- (frame-width) hy/frame-resize-increment)
                  (frame-height)))

(general-def normal "C-w u" #'hy/decrease-frame-height)
(general-def normal "C-w d" #'hy/increase-frame-height)
(general-def normal "C-w [" #'hy/decrease-frame-width)
(general-def normal "C-w ]" #'hy/increase-frame-width)

(provide 'hy-frame)
