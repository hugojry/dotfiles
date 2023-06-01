;; -*- lexical-binding: t; -*-

;; There is a bug in the nrepl server sentinel that breaks
;; sesman-restart. As a fail safe, all cider connections are closed
;; when the nrepl server is killed, which closes the repl buffers. The
;; sesman-restart implementation tries to re-use these buffers, but it
;; can't if the buffers are closed by the sentinel. The solution is
;; simply to not close the connections in the server sentinel.

(require 'nrepl-client)

(defun nrepl-server-sentinel (process event)
  "Handle nREPL server PROCESS EVENT.
On a fatal EVENT, attempt to close any open client connections, and signal
an `error' if the nREPL PROCESS exited because it couldn't start up."
  ;; only interested on fatal signals.
  (when (not (process-live-p process))
    (emacs-bug-46284/when-27.1-windows-nt
     ;; There is a bug in emacs 27.1 (since fixed) that sets all EVENT
     ;; descriptions for signals to "unknown signal". We correct this by
     ;; resetting it back to its canonical value.
     (when (eq (process-status process) 'signal)
       (cl-case (process-exit-status process)
         ;; SIGHUP==1 emacs nt/inc/ms-w32.h
         (1 (setq event "Hangup"))
         ;; SIGINT==2 x86_64-w64-mingw32/include/signal.h
         (2 (setq event "Interrupt"))
         ;; SIGKILL==9 emacs nt/inc/ms-w32.h
         (9 (setq event "Killed")))))
    (let* ((server-buffer (process-buffer process)))
      (if (process-get process :cider--nrepl-server-ready)
          (progn
            (when server-buffer (kill-buffer server-buffer))
            (message "nREPL server exited."))
        (let ((problem (when (and server-buffer (buffer-live-p server-buffer))
                         (with-current-buffer server-buffer
                           (buffer-substring (point-min) (point-max))))))
          (error "Could not start nREPL server: %s (%S)" problem (string-trim event)))))))

(provide 'nrepl-server-sentinel-fix)
