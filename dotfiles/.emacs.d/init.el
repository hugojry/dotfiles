;;; init.el --- Strong message here  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Emacs configuration

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; Declutter
(menu-bar-mode 0)
(toggle-scroll-bar 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

;; Indentation
(setq-default indent-tabs-mode nil)
(require 'lisp-indent-function)
(setq lisp-indent-function #'Fuco1/lisp-indent-function)

(savehist-mode)

(setq column-number-indicator-zero-based nil)
(column-number-mode)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))

(save-place-mode)

(setq-default case-fold-search nil)

(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode)

(require 'recentf)
(setq recentf-max-saved-items 1000
      recentf-max-menu-items 1000)
(recentf-mode)

(require 'dabbrev)
(setq dabbrev-case-fold-search nil)

(require 'eshell)
(setq eshell-prefer-lisp-functions t)

;; Required for sudo-ing in eshell
(require 'em-tramp)

(setq password-cache t)
(setq password-cache-expiry 3600)

(require 'dired)
(setq dired-dwim-target t)

;; Provides describe-keymap
(require 'help-fns+)

(setq eldoc-echo-area-use-multiline-p nil)

(setq org-adapt-indentation nil
      org-src-preserve-indentation t
      org-babel-load-languages '((emacs-lisp . t)
                                 (python . t)))

(if (< emacs-major-version 27)
    (package-initialize)
  (require 'package))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(when (equal emacs-version "27.2")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Core packages - loaded immediately

(use-package diminish :demand t)

(diminish 'eldoc-mode)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(use-package general
  :demand t
  :config
  (general-evil-setup))

(general-create-definer general-spc
  :states 'normal
  :keymaps 'override
  :prefix "SPC")

(general-spc
  "u" #'universal-argument
  "-" #'negative-argument)

(general-def normal
  "-" #'dired-jump
  "_" #'dired-jump-other-window)

(general-def normal dired-mode-map "-" #'dired-up-directory)

(general-create-definer general-comma
  :keymaps 'override
  :states 'normal
  :prefix ",")

(general-comma
  "b b" #'switch-to-buffer
  "b k" #'kill-buffer)

(general-spc
  "n w" #'widen
  "n d" #'narrow-to-defun
  "n x" #'sp-narrow-to-sexp)

(general-spc "e" #'eshell)

(general-def normal emacs-lisp-mode-map
  "K" #'describe-symbol)

(general-def normal (emacs-lisp-mode-map lisp-interaction-mode-map)
  ", f" #'eval-defun
  ", k" #'eval-buffer)

(general-def (normal insert) lisp-interaction-mode-map
 "C-j" #'eval-print-last-sexp)

(general-def normal Info-mode-map
  "RET" #'Info-follow-nearest-node)

(use-package evil
  :demand t
  :general (general-comma "C" #'hy/bind-command)
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

  (defun hy/bind-command (command key)
    (interactive "sShell command: \nKKey: ")
    (let ((f (lambda ()
               (interactive)
               (shell-command command))))
      (define-key evil-normal-state-map key f)))

  (evil-mode))

(evil-define-operator hy/evil-eval (beg end type)
  :move-point nil
  (eval-region beg end t))

(general-def normal (emacs-lisp-mode-map lisp-interaction-mode-map)
  ", e" #'hy/evil-eval)

(use-package evil-collection
  :demand t
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init))

;; All the following packages are deferred

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo-tree-history/")))
  (global-undo-tree-mode))

(use-package magit
  :general
  (general-spc "g g" #'magit-status)
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
    lisp-mode-hook
    sly-mrepl-mode-hook))

(use-package paredit
  :ghook hy/lisp-mode-hooks
  :general
  (general-def normal paredit-mode-map
    ", (" #'paredit-wrap-round
    ", [" #'paredit-wrap-square
    ", {" #'paredit-wrap-curly
    ", O" #'paredit-raise-sexp
    ", @" #'paredit-splice-sexp))

(use-package lispyville
  :diminish
  :ghook 'paredit-mode-hook
  :general
  (general-def normal lispyville-mode-map
    ", R" #'lispyville-drag-backward
    ", r" #'lispyville-drag-forward)
  (general-def (normal visual motion) lispyville-mode-map
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list
    "H" #'lispyville-backward-sexp
    "L" #'lispyville-forward-sexp)
  :config
  (lispyville-set-key-theme
   '(operators
     text-objects
     slurp/barf-cp
     c-w)))

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
  (general-def normal clojure-mode-map
    ", j n s" #'clojure-sort-ns
    ", j j" #'cider-jack-in
    ", j c" #'cider-jack-in-cljs
    ", j b" #'cider-jack-in-clj&cljs)
  :init
  (setq clojure-toplevel-inside-comment-form t))

(use-package cider
  :general
  (general-def normal cider-mode-map
    ", e" #'hy/cider-eval
    ", d" #'hy/cider-eval-popup
    ", x" #'hy/cider-eval-replace
    ", f" #'cider-eval-defun-at-point
    ", k" #'cider-load-buffer
    ", t t" #'cider-test-run-test
    ", t a" #'cider-test-rerun-test
    ", t n" #'cider-test-run-ns-tests)
  (general-def normal cider-repl-mode-map
    "g o" #'cider-repl-switch-to-other)
  (general-def normal (cider-mode-map cider-repl-mode-map)
    ", s q" #'sesman-quit
    ", s r" #'sesman-restart)
  :init
  (setq cider-font-lock-dynamically nil)
  ;; Shouldn't be necessary, but it is.
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package ivy
  :diminish
  :init
  (setq ivy-extra-directories nil)
  :config
  (ivy-mode))

(general-spc "f f" #'find-file)

(use-package counsel
  :diminish
  :init
  (counsel-mode)
  (general-spc "f r" #'counsel-recentf))

(use-package swiper :general ("C-s" #'swiper))

(use-package projectile
  :diminish
  :general
  (general-def normal projectile-mode-map
    "SPC p" 'projectile-command-map)
  :init
  (setq projectile-use-git-grep t)
  (projectile-mode))

(use-package evil-org
  :diminish
  :init
  (add-hook 'org-mode-hook #'evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package flycheck-clj-kondo
  :init
  (require 'flycheck-clj-kondo))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package company
  :diminish
  :general
  (company-active-map
   "C-n" nil
   "C-p" nil
   "M-n" nil
   "M-p" nil)
  :init
  (global-company-mode)
  (company-tng-mode))

(evil-define-operator hy/sly-eval (beg end type)
  :move-point nil
  (sly-eval-region beg end))

(use-package sly
  :general
  (general-def normal sly-mode-map
    ", e" #'hy/sly-eval))

(provide 'init)
;;; init.el ends here
