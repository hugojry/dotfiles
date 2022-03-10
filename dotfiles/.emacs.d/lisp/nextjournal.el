(defun clerk-show ()
  (interactive)
  (save-buffer)
  (let ((filename (buffer-file-name)))
    (when filename
      (cider-interactive-eval
       (concat "(nextjournal.clerk/show! \"" filename "\")")))))
