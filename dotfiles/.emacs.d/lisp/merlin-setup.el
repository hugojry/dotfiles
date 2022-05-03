(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)))

;; Automatically load utop.el
(autoload 'utop "utop" "Toplevel for OCaml" t)

(setq utop-command "opam config exec -- utop -emacs")

(provide 'merlin-setup)
