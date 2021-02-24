;;; pdabbrev.el --- dabbrev-expand with a popup menu -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Inspired by Joel Rosdahl's fancy-dabbrev, this package adds a popup
;; menu to the builtin dabbrev-expand command.

;;; Code:

(require 'cl-lib)
(require 'popup)
(require 'dabbrev)

(defgroup pdabbrev nil
  "Popup dabbrev"
  :group 'pdabbrev)

(defcustom pdabbrev-max-menu-length 30
  "Maximum number of candidates to generate.
Bigger numbers will impact performance."
  :type 'integer
  :group 'pdabbrev)

(defvar pdabbrev--candidates nil
  "The expansion candidates for the current completion.")

(defvar pdabbrev--abbrev nil
  "The current abbreviation being completed.")

(defvar pdabbrev--selection-index 0
  "The index of the currently selected candidate.")

(defvar pdabbrev--popup nil
  "The state of the popup menu.")

(defconst pdabbrev--commands
  '(pdabbrev-expand-next
    pdabbrev-expand-previous))

(defface pdabbrev-menu-face
  '((t (:inherit popup-face)))
  "Face for the pdabbrev menu.")

(defface pdabbrev-selection-face
  '((t (:inherit popup-menu-selection-face)))
  "Face for the selected item in the pdabbrev menu.")

(defface pdabbrev-preview-face
  '((t (:inherit shadow :underline t)))
  "Face for the preview.")

(defun pdabbrev--pdabbrev-command-p (command)
  "Is COMMAND a completion command?"
  (memq command pdabbrev--commands))

(defun pdabbrev--expansion-active-p ()
  "Are we are mid-completion?"
  (and (pdabbrev--pdabbrev-command-p last-command)
       pdabbrev--candidates))

(fset 'pdabbrev-expansion-active-p #'pdabbrev--expansion-active-p)

(defun pdabbrev--dabbrev-candidates (abbrev candidates n)
  "Add to a list of dabbrev CANDIDATES.

List will be of max length N and will be generated from ABBREV."
  (if (< n 1)
      (nreverse candidates)
    (let ((candidate
           (dabbrev--find-expansion abbrev 0 dabbrev-case-fold-search)))
      (if candidate
          (pdabbrev--dabbrev-candidates
           abbrev
           (cons candidate candidates)
           (1- n))
        (nreverse candidates)))))

(defun pdabbrev--insert-expansion (expansion)
  "Insert EXPANSION at point."
  (delete-region (pdabbrev--abbrev-start-location) (point))
  (insert expansion))

(defun pdabbrev--abbrev-start-location ()
  "Determine the start location of the abbreviation."
  (- dabbrev--last-abbrev-location (length pdabbrev--abbrev)))

(defun pdabbrev--get-popup-point ()
  "Determine where to place the menu.

The menu is normally placed directly under point, but if point is
near the right window edge or on a wrapped line, the menu is
placed first at the next line to avoid a misrendered menu."
  (let ((start (pdabbrev--abbrev-start-location))
        (line-width (- (line-end-position) (line-beginning-position))))
    (if (> line-width (window-width))
        (save-excursion
          (goto-char start)
          (forward-line)
          (point))
      start)))

(defun pdabbrev--init-popup (point candidates)
  "Create a popup menu at POINT containing CANDIDATES."
  (let ((popup (popup-create
                point
                (apply #'max (mapcar #'length candidates))
                10
                :around t
                :face 'pdabbrev-menu-face
                :selection-face 'pdabbrev-selection-face
                :scroll-bar t)))
    (popup-set-list popup candidates)
    (popup-draw popup)
    popup))

(defun pdabbrev--init ()
  "Prepare for a new completion."
  (dabbrev--reset-global-variables)
  (setq pdabbrev--abbrev (dabbrev--abbrev-at-point))
  (let ((candidates
         (pdabbrev--dabbrev-candidates pdabbrev--abbrev nil
                                       pdabbrev-max-menu-length)))
    (setq pdabbrev--selection-index 0)
    (cond
     ((cadr candidates)
      (setq pdabbrev--candidates candidates
            pdabbrev--popup (pdabbrev--init-popup
                             (pdabbrev--get-popup-point)
                             candidates))
      (pdabbrev--insert-expansion (car candidates))
      (add-hook 'post-command-hook #'pdabbrev--post-command-hook))

     ((car candidates)
      (pdabbrev--insert-expansion (car candidates)))

     ((error "No expansions found")))))

(defun pdabbrev--tear-down ()
  "Delete the popup."
  (popup-delete pdabbrev--popup)
  (setq pdabbrev--candidates nil
        pdabbrev--abbrev nil
        pdabbrev--selection-index 0
        pdabbrev--popup nil)
  (dabbrev--reset-global-variables))

(defun pdabbrev--select-next ()
  "Move to the next selection."
  (setq pdabbrev--selection-index
        (mod (1+ pdabbrev--selection-index)
             (length pdabbrev--candidates)))
  (pdabbrev--insert-expansion
   (nth pdabbrev--selection-index pdabbrev--candidates))
  (popup-next pdabbrev--popup))

(defun pdabbrev--select-previous ()
  "Move to the previous selection."
  (setq pdabbrev--selection-index (1- pdabbrev--selection-index))
  (when (< pdabbrev--selection-index 0)
    (setq pdabbrev--selection-index
          (+ pdabbrev--selection-index
             (length pdabbrev--candidates))))
  (pdabbrev--insert-expansion
   (nth pdabbrev--selection-index pdabbrev--candidates))
  (popup-previous pdabbrev--popup))

(defun pdabbrev-expand-next ()
  "Start the expansion menu if not open or expand the next selection."
  (interactive)
  (if (pdabbrev--expansion-active-p)
      (pdabbrev--select-next)
    (pdabbrev--init)))

(defun pdabbrev-expand-previous ()
  "Start the expansion menu if not open or expand the previous selection."
  (interactive)
  (if (pdabbrev--expansion-active-p)
      (pdabbrev--select-previous)
    (pdabbrev--init)))

(defun pdabbrev-abort ()
  "Abort the current expansion and revert."
  (interactive)
  (when (pdabbrev--pdabbrev-command-p last-command)
    (pdabbrev--insert-expansion pdabbrev--abbrev)))

(defun pdabbrev--post-command-hook ()
  "Tear down."
  (unless (pdabbrev--pdabbrev-command-p this-command)
    (pdabbrev--tear-down)
    (remove-hook 'post-command-hook #'pdabbrev--post-command-hook)))

(provide 'pdabbrev)

;;; pdabbrev.el ends here
