;;; package --- Summary

;;; Commentary:

;;; Code:

(require 's)

(defconst telecommand-process-name "telecommand")
(defconst telecommand-buffer-name "*telecommand*")
(defconst telecommand-output-buffer-name "*telecommand-output*")

(defconst telecommand-list-format
  [("Marked" 6 t :right-align t)
   ("Name" 25 t)
   ("Host" 25 t)
   ("Command" 25 nil)]
  "List format.")

(defconst telecommand-list-sort-key
  '("Name" . nil)
  "Sort table on this key.")

(defvar telecommand-commands '())
(defvar telecommand-run-async nil)

(defvar telecommand-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'telecommand-run-line)
    map)
  "Keymap for `telecommand-mode'.")

(defun telecommand--buffer ()
  "Return telecommand buffer if it exists."
  (get-buffer telecommand-buffer-name))

(defun telecommand--list-hosts ()
  "List all hosts."
  (mapcar 'car telecommand-commands))

(defun telecommand--list-commands-names (host)
  "List all commands for a given host.

HOST is a host"
  (mapcar 'car (assoc-default host telecommand-commands)))


(defun telecommand--find-command (host name)
  "For a given host and name, find matching command.

HOST is the host
NAME is the command's name"
  (assoc-default name (assoc-default host telecommand-commands)))


(defun telecommand--run-command (host command)
  "Run a command on remote host and return output buffer.

HOST is the remote host of type /ssh:host:
COMMAND is the comamnd to run"
  (let ((default-directory host)
        (process telecommand-process-name)
        (buffer telecommand-output-buffer-name))
    (if telecommand-run-async
        (async-shell-command command buffer)
      (shell-command command buffer))
    buffer))

(defun telecommand--show-result (buffer)
  "Display the result buffer.

BUFFER is the result buffer"
  (pop-to-buffer buffer)
  (goto-char (point-min)))

(defun telecommand-run-line ()
  "Run the current line in telecommand buffer."
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (telecommand--show-result
     (telecommand--run-command (elt entry 2) (elt entry 3)))))

(defun telecommand--format-host (host)
  "Format the host.

HOST is the host string"
  (if (not (s-prefix? "/ssh:" host))
      (if (not (s-suffix? ":" host))
          (format "/ssh:%s:" host)
        (format "/ssh:%s" host))
    (if (not (s-suffix? ":" host))
        (format "%s:" host)
      host)))

(defun telecommand--add-command (host name command)
  "Add command to the data structure.

HOST is the host
NAME is the command name
COMMAND is the actual command"
  (if (not (assoc-default host telecommand-commands))
      (push (cons host (list (cons name command))) telecommand-commands)
    (let ((dic (assoc-default host telecommand-commands)))
      (setcdr (assoc host telecommand-commands)
              (append dic (list (cons name command)))))))

(defun telecommand-add-command (&rest cl-keys)
  "Add a command.

CL-KEYS arguments"
  (cl--parsing-keywords ((:host nil) (:command nil) (:name nil)) nil
    (let ((host (telecommand--format-host cl-host))
          (name cl-name)
          (command cl-command))
      (telecommand--add-command host name command))))

(defun telecommand-run ()
  "Quick run of a command."
  (interactive)
  (let ((host (completing-read "Pick a host:" (telecommand--list-hosts))) )
    (let ((name (completing-read "Pick a command: " (telecommand--list-commands-names host))))
      (let ((command (telecommand--find-command host name)))
        (telecommand--show-result (telecommand--run-command host command))))))

(defun telecommand--command-id (host name)
  "Turn the command's host and name into an id.

HOST is the host.
NAME is the name."
  (intern (concat
           (replace-regexp-in-string "\/ssh:" "" host)
           (replace-regexp-in-string "[^a-zA-Z0-9]" "" name))))

(defun telecommand-list-entries ()
  "Create the entries for the service list."
  (apply #'append
         (mapcar (lambda (host)
                   (mapcar (lambda (command)
                             (list (telecommand--command-id (car host) (car command))
                                   (vector "" (car command) (car host) (cdr command))))
                           (cdr host)))
                 telecommand-commands)))

(defun telecommand--imenu-prev-index-position-function ()
  "Move point to previous line in telecommand buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

(defun telecommand--imenu-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (elt (tabulated-list-get-entry) 1))

(define-derived-mode telecommand-mode tabulated-list-mode "Telecommand"
  "Special mode for telecommand buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "Telecommand")
  (setq major-mode 'telecommand-mode)
  (use-local-map telecommand-mode-map)
  (setq tabulated-list-format telecommand-list-format)
  (setq tabulated-list-entries 'telecommand-list-entries)
  (setq tabulated-list-sort-key telecommand-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (setq imenu-prev-index-position-function
        #'telecommand--imenu-prev-index-position-function)
  (setq imenu-extract-index-name-function
        #'telecommand--imenu-extract-index-name-function)
  (run-mode-hooks 'telecommand-mode-hook))

(defun telecommand ()
  "Invoke the telecommand buffer."
  (interactive)
  (let ((buffer-p (telecommand--buffer))
        (buffer (get-buffer-create telecommand-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (telecommand-mode))))

(provide 'telecommand)
;;; telecommand.el ends here
