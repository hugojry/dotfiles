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

(defcustom claude-edit-prompt
  "The following code resides in @%s:

```
%s
```

Use whatever context you need from the current project, but only edit the code above.

Here's what I would like you to do. %s"
  "Prompt harness for a request to Claude to edit a specific block.")

(defun claude--call (prompt &optional allow-edits)
  "PROMPT Claude.
If ALLOW-EDITS is non-nil, then allow edits ... duh."
  (let ((proc (make-process
			   :name "hy-agent-subprocess"
			   :buffer agent--output-buffer
			   :command (if allow-edits
							'("claude" "-p" "--permission-mode" "acceptEdits")
						  '("claude" "-p"))
			   :connection-type 'pipe
			   :sentinel (lambda (proc _event)
						   (unless (process-live-p proc)
							 (setq agent--is-already-running nil))))))
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

(defun claude--validate-args (start end)
  "Can we start a new prompt given START, END and the state of a previous prompt?"
  (when (= start end)
	(user-error "Region is empty"))
  (when agent--is-already-running
	(user-error "One agent request at a time")))

(defun claude-explain (start end)
  "Ask Claude about the region (the text between START and END)."
  (interactive "r")
  (claude--validate-args start end)
  (claude--call (format claude-explain-prompt (buffer-substring-no-properties start end))))

(defun claude-edit (start end user-prompt)
  "Ask Claude to edit the region (the text between START and END).
USER-PROMPT describes the edit you want Claude to make."
  (interactive "r\nsPrompt: ")
  (unless (buffer-file-name)
	(user-error "This only works on a buffer visiting a file"))
  (claude--validate-args start end)
  (let* ((default-directory (agent--root-dir))
		 (absolute-path (buffer-file-name))
		 (file-path (if (file-in-directory-p absolute-path default-directory)
						(file-relative-name absolute-path default-directory)
					  absolute-path))
		 (selected (buffer-substring-no-properties start end)))
	(claude--call (format claude-edit-prompt file-path selected user-prompt) t)))

(provide 'hy-agent)
;;; hy-agent.el ends here
