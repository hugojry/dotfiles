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

(defvar hy/frame-resize-repeat-map (make-sparse-keymap)
  "Keymap of all repeatable frame resizing commands.")

(defmacro defun-repeatable (name last-key &rest body)
  `(progn
     (defun ,name ()
       (interactive)
       ,@body
       (set-transient-map
        hy/frame-resize-repeat-map
        t
        nil
        "Repeat resizing with %k"))
     (define-key hy/frame-resize-repeat-map (kbd ,last-key) #',name)))

(setq hy/frame-resize-increment 5)

(defun-repeatable hy/increase-frame-height "d"
  (set-frame-size (selected-frame)
                  (frame-width)
                  (+ (frame-height) hy/frame-resize-increment)))

(defun-repeatable hy/decrease-frame-height "u"
  (set-frame-size (selected-frame)
                  (frame-width)
                  (- (frame-height) hy/frame-resize-increment)))

(defun-repeatable hy/increase-frame-width "]"
  (set-frame-size (selected-frame)
                  (+ (frame-width) hy/frame-resize-increment)
                  (frame-height)))

(defun-repeatable hy/decrease-frame-width "["
  (set-frame-size (selected-frame)
                  (- (frame-width) hy/frame-resize-increment)
                  (frame-height)))

(general-def normal "C-w u" #'hy/decrease-frame-height)
(general-def normal "C-w d" #'hy/increase-frame-height)
(general-def normal "C-w [" #'hy/decrease-frame-width)
(general-def normal "C-w ]" #'hy/increase-frame-width)

(provide 'hy-frame)
