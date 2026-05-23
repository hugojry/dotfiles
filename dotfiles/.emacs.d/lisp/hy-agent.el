;;; hy-agent.el --- Agent util functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'project)

(defvar agent--output-buffer "*agent-output*")
(defvar agent--is-already-running nil)

(defcustom claude-explain-prompt
  "Please tell me about this code block:

```
%s
```"
  "Prompt to embed the selected text in when asking Claude to explain it.")

(defcustom claude-edit-file-prompt
  "The following instructions relate to @%s. You may only edit this file.

%s"
  "Prompt harness for asking Claude to edit a file.")


(defcustom claude-edit-block-prompt
  "The following code resides in @%s:

```
%s
```

Use whatever context you need from the current project, but only edit the code above.

Here's what I would like you to do. %s"
  "Prompt harness for asking Claude to edit a specific block.")

(defun claude--call (prompt &optional allow-edits)
  "PROMPT Claude.
If ALLOW-EDITS is non-nil, then allow edits ... duh."
  (let* ((file-name buffer-file-name)
		 (source (current-buffer))
		 (snapshot (when allow-edits
					 (let ((buf (generate-new-buffer " *agent-snapshot*")))
					   (copy-to-buffer buf (point-min) (point-max))
					   buf)))
		 (proc (make-process
				:name "hy-agent-subprocess"
				:buffer agent--output-buffer
				:command (if allow-edits
							 '("claude" "-p" "--permission-mode" "acceptEdits")
						   '("claude" "-p"))
				:connection-type 'pipe
				:sentinel
				(lambda (proc event)
				  (unless (process-live-p proc)
					(setq agent--is-already-running nil))
				  (when (and allow-edits (string= event "finished\n"))
					(let ((diff-buffer (diff-no-select snapshot file-name))
						  (output-window (get-buffer-window agent--output-buffer)))
					  (kill-buffer snapshot)
					  (if output-window
						  (with-selected-window output-window
							(display-buffer diff-buffer '(nil (inhibit-same-window . t))))
						(display-buffer diff-buffer))))))))
	(with-current-buffer agent--output-buffer
	  (unless (derived-mode-p 'markdown-mode)
		(markdown-mode))
	  (erase-buffer))
	(display-buffer agent--output-buffer)
	(setq agent--is-already-running t)
	(process-send-string proc prompt)
	(process-send-eof proc)))

(defun agent--root-dir ()
  "Figure out where to root the call to the agent."
  (if (project-current)
	  (expand-file-name (project-root (project-current)))
	default-directory))

(defun claude-explain (start end)
  "Ask Claude about the region (the text between START and END)."
  (interactive "r")
  (claude--call (format claude-explain-prompt (buffer-substring-no-properties start end))))

(defun claude-edit (start end user-prompt)
  "Ask Claude to edit the region (the text between START and END).
USER-PROMPT describes the edit you want Claude to make."
  (interactive "r\nsPrompt: ")
  (unless buffer-file-name
	(user-error "This only works on a buffer visiting a file"))
  (let* ((default-directory (agent--root-dir))
		 (file-path (if (file-in-directory-p buffer-file-name default-directory)
						(file-relative-name buffer-file-name default-directory)
					  absolute-path))
		 (selected (buffer-substring-no-properties start end)))
	(claude--call (format claude-edit-prompt file-path selected user-prompt) t)))

(provide 'hy-agent)
;;; hy-agent.el ends here
