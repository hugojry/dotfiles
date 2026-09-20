;;; hy-agent.el --- Agent util functions -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'project)
(require 'diff)

(defgroup hy-agent nil
  "Customization options for agent commands."
  :group 'applications)

(defcustom hy-agent-type 'codex
  "Which agent to use."
  :type '(choice
		  (const :tag "Codex" codex)
		  (const :tag "Claude Code" claude))
  :group 'hy-agent)

(defconst hy-agent--commands
  '((codex "codex" "exec")
	(claude "claude" "-p")))

(defconst hy-agent--allow-edit-commands
  '((codex "codex" "exec" "--sandbox" "workspace-write" "--ask-for-approval" "never")
	(claude "claude" "-p" "--permission-mode" "acceptEdits")))

(defvar agent--output-buffer "*agent-output*")
(defvar agent--is-already-running nil)

(defvar explain-block-prompt
  "Please tell me about this code block:

```
%s
```"
  "Prompt to embed the selected text in when asking the agent to explain it.")

(defvar explain-file-prompt
  "Read the code at @%s and then tell me about it."
  "Prompt to embed the selected text in when asking the agent to explain it.")


(defvar edit-file-prompt
  "The following instructions relate to @%s. You may only edit this file.

%s"
  "Prompt harness for asking the agent to edit a file.")


(defvar edit-block-prompt
  "The following code resides in @%s:

```
%s
```

Use whatever context you need from the current project, but only edit the code above.

Here's what I would like you to do. %s"
  "Prompt harness for asking the agent to edit a specific block.")

(defun agent--call (prompt &optional allow-edits)
  "PROMPT the agent.
If ALLOW-EDITS is non-nil, then allow edits ... duh."
  (let* ((file-name buffer-file-name)
		 (snapshot (when allow-edits
					 (let ((buf (generate-new-buffer " *agent-snapshot*")))
					   (copy-to-buffer buf (point-min) (point-max))
					   buf)))
		 (proc (make-process
				:name "hy-agent-subprocess"
				:buffer agent--output-buffer
				:command (alist-get hy-agent-type
									(if allow-edits
										hy-agent--allow-edit-commands
									  hy-agent--commands))
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

(defun agent--file-path ()
  "Figure out what file path, absolute or relative, to use in the prompt."
  (if (file-in-directory-p buffer-file-name default-directory)
	  (file-relative-name buffer-file-name default-directory)
	buffer-file-name))

(defun hy-agent-explain ()
  "Ask the agent about something."
  (interactive)
  (if (use-region-p)
	  (agent--call
	   (format explain-block-prompt
			   (buffer-substring-no-properties
				(region-beginning)
				(region-end))))
	(let ((prompt (read-string "Prompt: "))
		  (default-directory (agent--root-dir)))
	  (agent--call (if (string= "" prompt)
					   (format explain-file-prompt (agent--file-path))
					 prompt)))))

(defun hy-agent--make-edit-prompt (file-path user-prompt)
  "Choose the right harness for USER-PROMPT given the region and FILE-PATH."
  (if (use-region-p)
	  (format edit-file-prompt file-path user-prompt)
	(format edit-block-prompt
			file-path
			(buffer-substring-no-properties (region-beginning) (region-end))
			user-prompt)))

(defun hy-agent-edit ()
  "Ask the agent to edit the region (the text between START and END).
USER-PROMPT describes the edit you want the agent to make."
  (interactive)
  (unless buffer-file-name
	(user-error "This only works on a buffer visiting a file"))
  (let ((prompt (read-string "Prompt: "))
		(default-directory (agent--root-dir)))
	(agent--call (hy-agent--make-edit-prompt (agent--file-path) prompt) t)))

(provide 'hy-agent)
;;; hy-agent.el ends here
