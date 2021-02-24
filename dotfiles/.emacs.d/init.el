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
(recentf-mode)
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

;; Core packages - loaded immediately

(use-package diminish
  :demand t)

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil ; evil-collection takes care of this
        evil-want-C-u-scroll t
        evil-respect-visual-line-mode t
        evil-undo-system 'undo-tree)
  (setq-default evil-symbol-word-search t)
  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (evil-mode))

(use-package evil-collection
  :demand t
  :config
  (evil-collection-init))

(use-package general
  :demand t
  :config
  (general-evil-setup))

;; All the following packages are deferred

(use-package paren
  :init
  (setq show-paren-delay 0)
  (show-paren-mode))

(use-package eldoc
  :diminish)

(use-package undo-tree
  :diminish
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        '(("." . "~/.emacs.d/undo-tree-history/")))
  (global-undo-tree-mode))

(general-def 'normal "C-;" #'eval-expression)

(general-nmap "SPC u" #'universal-argument)

(general-nmap "-" #'dired-jump)

(general-nmap
  :keymaps 'dired-mode-map
  "-" #'dired-up-directory)

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
    "c" #'counsel-org-capture))

(evil-define-operator hy/evil-eval (beg end type)
  :move-point nil
  (eval-region beg end t))

(use-package elisp-mode
  :ensure f
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

(use-package autorevert
  :diminish auto-revert-mode)

(use-package magit
  :config
  (general-def 'magit-mode-map
    ", c c" (lambda ()
              (interactive)
              (magit-run-git "commit" "-m" "Checkpoint"))))

(use-package paredit
  :general
  (general-nmap
    :keymaps 'paredit-mode-map
    :prefix ","
    "\S-o" #'paredit-raise-sexp
    "@" #'paredit-splice-sexp)
  :ghook '(emacs-lisp-mode-hook
           clojure-mode-hook
           cider-repl-mode-hook
           fennel-mode-hook
           inferior-lisp-mode-hook
           eval-expression-minibuffer-setup-hook
           racket-mode-hook
           lisp-mode-hook))

(use-package evil-cleverparens
  :diminish
  :general
  (general-nmap
    :keymaps 'evil-cleverparens-mode-map
    :prefix ","
    "w r" #'evil-cp-wrap-next-round
    "w s" #'evil-cp-wrap-next-square
    "w c" #'evil-cp-wrap-next-curly)
  (general-define-key
   :states '(normal visual operator)
   :keymaps 'evil-cleverparens-mode-map
   "{" nil
   "}" nil
   "[" nil
   "]" nil
   "[ s" #'evil-cp-previous-opening
   "] s" #'evil-cp-next-closing
   "[ d" #'evil-cp-next-opening
   "] d" #'evil-cp-previous-closing)
  :init
  (add-hook 'paredit-mode-hook #'evil-cleverparens-mode)
  (setq evil-cleverparens-use-regular-insert t)
  :config
  (require 'evil-cleverparens-fixes)
  (add-to-list 'evil-change-commands #'evil-cp-change))

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
    "b" #'cider-jack-in-clj&cljs))

(use-package cider
  :general
  (general-mmap :keymaps 'cider-mode-map ", e" #'hy/cider-eval)
  (general-mmap
    :keymaps 'cider-mode-map
    :prefix ", c"
    "i" #'cider-inspect-last-result
    "r" #'hy/cider-eval-replace
    "b" #'hy/cider-eval-popup)
  (general-nmap
    :keymaps 'cider-repl-mode-map
    "g o" #'cider-repl-switch-to-other)
  :init
  ;; Shouldn't be necessary, but it is.
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package ivy
  :diminish
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

(use-package swiper
  :general
  ("C-s" #'swiper))

(use-package projectile
  :diminish
  :init
  (projectile-mode)
  (general-nmap
    :keymaps 'projectile-mode-map
    "SPC p" 'projectile-command-map))

(use-package counsel-projectile
  :config
  (with-eval-after-load 'projectile
    (counsel-projectile-mode)))

(use-package evil-org
  :diminish
  :init
  (add-hook 'org-mode-hook #'evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package flycheck
  :diminish
  :general
  (general-nmap
    :keymaps 'flycheck-mode-map
    "] q" #'flycheck-next-error
    "[ q" #'flycheck-previous-error)
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

(use-package rust-mode)

(use-package flycheck-rust
  :init
  (with-eval-after-load 'rust-mode
   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package company
  :diminish
  :general
  (general-imap
    "C-x C-o" #'company-manual-begin)
  :init
  (global-company-mode)
  (setq company-idle-delay nil)
  :config
  (setq company-active-map (make-sparse-keymap))
  (general-def 'company-active-map
    "C-n" #'company-select-next
    "C-p" #'company-select-previous
    "C-e" #'company-abort)
  (add-hook 'evil-local-mode-hook
            (lambda ()
              (when (memq 'company-emulation-alist emulation-mode-map-alists)
                (company-ensure-emulation-alist)))))

(use-package dabbrev
  :config
  (setq dabbrev-case-fold-search nil))

(use-package popup)

(use-package pdabbrev
  :ensure nil
  :init
  (require 'pdabbrev)

  (defun hy/padabbrev-abort-or-C-e ()
    (interactive)
    (call-interactively (if (pdabbrev-expansion-active-p)
                            #'pdabbrev-abort
                          #'evil-copy-from-below)))

  (general-imap
    "C-n" #'pdabbrev-expand-next
    "C-p" #'pdabbrev-expand-previous
    "C-e" #'hy/padabbrev-abort-or-C-e))

(provide 'init)
;;; init.el ends here
