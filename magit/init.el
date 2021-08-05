;;; init.el --- Strong message here  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Config for using Emacs purely for magit

;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar hy/packages '(magit evil evil-collection))

(let ((needs-refresh t))
  (dolist (package hy/packages)
    (when (not (package-installed-p package))
      (when needs-refresh
        (package-refresh-contents)
        (setq needs-refresh nil))
      (package-install package))))

(require 'evil)
(setq evil-want-integration t
      evil-want-keybinding nil ; evil-collection takes care of this
      evil-want-C-u-scroll t
      evil-search-module 'evil-search
      evil-ex-search-case 'sensitive)
(setq-default evil-symbol-word-search t)

(defalias #'forward-evil-word #'forward-evil-symbol)
(evil-mode)

(require 'evil-collection)
(evil-collection-init)

(require 'magit)
(setq magit-bury-buffer-function 'magit-mode-quit-window)

(magit-status-setup-buffer)

(provide 'init)
;;; init.el ends here
