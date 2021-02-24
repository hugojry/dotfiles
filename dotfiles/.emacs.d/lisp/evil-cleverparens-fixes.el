(require 'evil)

;; https://github.com/luxbock/evil-cleverparens/issues/59
(defun evil-cp--forward-X-begin (thing count)
  "TODO: see `forward-evil-cp-word' which is currently not
working. Could be used to implement a future
`evil-cp-forward-word-begin' the same way that
`evil-cp-forward-symbol-begin' is defined."
  (let ((orig (point)))
    (evil-signal-at-bob-or-eob count)
    (cond ((not (evil-operator-state-p))
           (evil-forward-beginning thing count))

          ((and evil-want-change-word-to-end
                ;; This is the changed line.
                (memq evil-this-operator evil-change-commands)
                (< orig (or (cdr-safe (bounds-of-thing-at-point thing)) orig)))
           (forward-thing thing count))

          (t (prog1 (evil-forward-beginning thing count)
               (when (and (> (line-beginning-position) orig)
                          (looking-back "^[[:space:]]*" (line-beginning-position)))
                 (evil-move-end-of-line 0)
                 (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                             (not (<= (line-beginning-position) orig)))
                   (evil-move-end-of-line 0))
                 (when (bolp) (forward-char))))))))

(evil-define-operator evil-cp-change (beg end type register yank-handler delete-func)
  "Call `evil-change' while keeping parentheses balanced."
  (interactive "<R><x><y>")
  (if (or (= beg end)
          (evil-cp--override)
          (and (eq type 'block) (evil-cp--balanced-block-p beg end))
          (and (sp-region-ok-p beg end) (not (eq type 'block))))
      (evil-change beg end type register yank-handler delete-func)
    (let ((delete-func (or delete-func #'evil-cp-delete))
          (nlines (1+ (- (line-number-at-pos end)
                         (line-number-at-pos beg))))
          (opoint (save-excursion
                    (goto-char beg)
                    (line-beginning-position))))
      (cond ((eq type 'line)
             (save-excursion
               (evil-cp--delete-characters
                (+ beg
                   (save-excursion
                     (beginning-of-line)
                     (sp-forward-whitespace t)))
                (1- end)))
             (evil-cp-first-non-blank-non-opening)
             (indent-according-to-mode)
             (evil-insert 1))

            ((eq type 'block)
             (evil-cp-delete beg end type register yank-handler)
             (evil-insert 1 nlines))

            (t
             (funcall delete-func beg end type register yank-handler)
             (evil-insert 1))))))

(provide 'evil-cleverparens-fixes)
