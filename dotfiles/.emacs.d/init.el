;;; init.el --- Strong message here  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs configuration

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")

(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(savehist-mode)
(setq column-number-indicator-zero-based nil)
(column-number-mode)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(save-place-mode)
(setq-default case-fold-search nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'help-fns+)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Core packages - loaded immediately

(use-package diminish :demand t)

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil ; evil-collection takes care of this
        evil-want-C-u-scroll t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree
        evil-search-module 'evil-search
        evil-ex-search-case 'sensitive)
  (setq-default evil-symbol-word-search t)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (defun hy/nohighlight ()
    (when (memq this-command evil-motions)
      (evil-ex-nohighlight)))

  (add-hook 'post-command-hook #'hy/nohighlight)
  (evil-mode))

(use-package evil-collection
  :demand t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

(use-package general
  :demand t
  :config
  (general-evil-setup))

(general-def normal override
  "SPC u" #'universal-argument
  "SPC -" #'negative-argument)

(general-def normal "-" #'dired-jump)

(general-def normal dired-mode-map "-" #'dired-up-directory)

(general-create-definer general-buffer
  :keymaps 'override
  :states 'normal
  :prefix ", b")

(general-buffer
  "b" #'switch-to-buffer
  "k" #'kill-buffer)

(general-nmap
  :prefix "SPC n"
  "w" #'widen
  "d" #'narrow-to-defun
  "x" #'sp-narrow-to-sexp)

;; All the following packages are deferred

(use-package paren
  :init
  (setq show-paren-delay 0)
  (show-paren-mode))

(use-package recentf
  :init
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 1000)
  (recentf-mode))

(use-package dabbrev
  :config
  (setq dabbrev-case-fold-search nil))

(use-package eldoc :diminish)

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo-tree-history/")))
  (global-undo-tree-mode))

(use-package org
  :general
  (general-nmap
    :keymaps 'org-mode-map
    :prefix "SPC n"
    "s" #'org-narrow-to-subtree
    "b" #'org-narrow-to-block
    "e" #'org-narrow-to-element)
  (general-nmap
    :prefix "SPC o"
    "a" #'org-agenda
    "c" #'counsel-org-capture)
  :init
  (setq org-adapt-indentation nil
        org-src-preserve-indentation t
        org-babel-load-languages
        '((emacs-lisp . t)
          (python . t))))

(evil-define-operator hy/evil-eval (beg end type)
  :move-point nil
  (eval-region beg end t))

(use-package elisp-mode
  :ensure f
  :init
  (general-def normal emacs-lisp-mode-map
    "K" #'describe-symbol)
  :config
  (general-mmap
    :keymaps '(emacs-lisp-mode-map lisp-interaction-mode-map)
    ", e" #'hy/evil-eval
    "C-c C-c" #'eval-defun
    "C-c C-k" #'eval-buffer)

  (general-define-key
   :states '(normal insert)
   :keymaps 'lisp-interaction-mode-map
   "C-j" #'eval-print-last-sexp))

(use-package autorevert :diminish auto-revert-mode)

(use-package magit
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window))

(defconst hy/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    cider-repl-mode-hook
    fennel-mode-hook
    inferior-lisp-mode-hook
    eval-expression-minibuffer-setup-hook
    racket-mode-hook
    lisp-mode-hook))

(use-package paredit
  :ghook hy/lisp-mode-hooks
  :general
  (general-def 'normal paredit-mode-map
    ", (" #'paredit-wrap-round
    ", [" #'paredit-wrap-square
    ", {" #'paredit-wrap-curly
    ", O" #'paredit-raise-sexp
    ", @" #'paredit-splice-sexp))

(use-package lispyville
  :diminish
  :ghook 'paredit-mode-hook
  :config
  (lispyville-set-key-theme
   '(operators
     text-objects
     slurp/barf-cp
     c-w))
  (general-def 'normal lispyville-mode-map
    "M-t" #'lispyville-drag-backward)
  (general-def '(normal visual motion) lispyville-mode-map
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list
    "H" #'lispyville-backward-sexp
    "L" #'lispyville-forward-sexp))

(evil-define-operator hy/cider-eval (beg end type)
  :move-point nil
  (cider-interactive-eval nil
                          nil
                          (list beg end)
                          (cider--nrepl-pr-request-map)))

(evil-define-operator hy/cider-eval-replace (beg end type)
  :move-point nil
  (let ((form (buffer-substring-no-properties beg end)))
    ;; Only delete the form if the eval succeeds
    (cider-nrepl-sync-request:eval form)
    (delete-region beg end)
    (cider-interactive-eval form
                            (cider-eval-print-handler)
                            nil
                            (cider--nrepl-pr-request-map))))

(evil-define-operator hy/cider-eval-popup (beg end type)
  :move-point nil
  (cider--pprint-eval-form (buffer-substring-no-properties beg end)))

(use-package clojure-mode
  :general
  (general-nmap
    :keymaps 'clojure-mode-map
    :prefix ", j"
    "j" #'cider-jack-in
    "c" #'cider-jack-in-cljs
    "b" #'cider-jack-in-clj&cljs)
  :init
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :init
  (general-def 'motion cider-mode-map
    ", e" #'hy/cider-eval
    ", d" #'hy/cider-eval-popup
    ", x" #'hy/cider-eval-replace)
  (general-def 'normal cider-repl-mode-map
    "g o" #'cider-repl-switch-to-other)
  ;; Shouldn't be necessary, but it is.
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package ivy
  :diminish
  :init
  (setq ivy-extra-directories nil)
  :config
  (ivy-mode))

(general-create-definer general-file
  :keymaps 'override
  :states 'normal
  :prefix "SPC f")

(general-file "f" #'find-file)

(use-package counsel
  :diminish
  :init
  (counsel-mode)
  (general-file "r" #'counsel-recentf))

(use-package swiper :general ("C-s" #'swiper))

(use-package projectile
  :diminish
  :init
  (projectile-mode)
  (general-nmap
    :keymaps 'projectile-mode-map
    "SPC p" 'projectile-command-map))

(use-package evil-org
  :diminish
  :init
  (add-hook 'org-mode-hook #'evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package flycheck
  :diminish
  :init
  (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :init
  (require 'flycheck-clj-kondo))

;; On macOS Emacs can't find many user installed programs because GUI
;; apps are launched with a minimal set of environment variables.
(use-package exec-path-from-shell
  :ensure f
  :if (eq window-system 'ns)
  :init
  (exec-path-from-shell-initialize))

(use-package company
  :diminish
  :init
  (general-def company-active-map
    "C-n" nil
    "C-p" nil)
  (global-company-mode)
  :config
  ;; https://github.com/hlissner/doom-emacs/issues/1335#issuecomment-709691151
  (add-hook 'evil-local-mode-hook
            (lambda ()
              ;; Note:
              ;; Check if `company-emulation-alist' is in
              ;; `emulation-mode-map-alists', if true, call
              ;; `company-ensure-emulation-alist' to ensure
              ;; `company-emulation-alist' is the first item of
              ;; `emulation-mode-map-alists', thus has a higher
              ;; priority than keymaps of evil-mode.
              ;; We raise the priority of company-mode keymaps
              ;; unconditionally even when completion is not
              ;; activated. This should not cause problems,
              ;; because when completion is activated, the value of
              ;; `company-emulation-alist' is ((t . company-my-keymap)),
              ;; when completion is not activated, the value is ((t . nil)).
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist)))))

(use-package eshell
  :init
  (general-def normal
    "SPC e" #'eshell))

(provide 'init)
;;; init.el ends here
