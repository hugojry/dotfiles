(defun wslp ()
  (and
   (eq system-type 'gnu/linux)
   (file-exists-p "/run/WSL")
   (file-directory-p "/mnt/c")))

(when (wslp)
  ;; WSLg breaks copy-paste from Emacs into Windows
  ;; see: https://www.lukas-barth.net/blog/emacs-wsl-copy-clipboard/
  (setq select-active-regions nil
		select-enable-clipboard 't
		select-enable-primary nil
		interprogram-cut-function #'gui-select-text))

(provide 'wsl)
