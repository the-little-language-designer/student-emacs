;; copy form gforth.el, which copy from cmuscheme.el

(require 'cicada-nymph-mode)
(require 'comint)

(provide 'inferior-cicada-nymph-mode)


(defvar cicada-nymph-program-name "cn"
  "*Program invoked by the `run-cicada-nymph' command, including program arguments")

(defcustom inferior-cicada-nymph-mode-hook nil
  "*Hook for customising inferior-cicada-nymph-mode."
  :type 'hook
  :group 'cicada-nymph)

(defvar inferior-cicada-nymph-mode-map)
(defvar cicada-nymph-process-buffer)

(define-derived-mode inferior-cicada-nymph-mode comint-mode
  "Inferior cicada-nymph"
  "Major mode for interacting with an inferior cicada-nymph process.

The following commands are available:
\\{inferior-cicada-nymph-mode-map}

A cicada-nymph process can be fired up with M-x run-cicada-nymph.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inferior-cicada-nymph-mode-hook (in that order).

You can send text to the inferior cicada-nymph process from other buffers containing
cicada-nymph source.
    cicada-nymph-switch-to-interactive switches the current buffer to the cicada-nymph
        process buffer.
    cicada-nymph-send-paragraph sends the current paragraph to the cicada-nymph process.
    cicada-nymph-send-region sends the current region to the cicada-nymph process.
    cicada-nymph-send-buffer sends the current buffer to the cicada-nymph process.

    cicada-nymph-send-paragraph-and-go, cicada-nymph-send-region-and-go,
        cicada-nymph-send-buffer-and-go switch to the cicada-nymph process buffer after
        sending their text.
For information on running multiple processes in multiple buffers, see
documentation for variable `cicada-nymph-process-buffer'.

Commands:
Return after the end of the process' output sends the text from the
end of process to point. If you accidentally suspend your process, use
\\[comint-continue-subjob] to continue it. "
  ;; Customise in inferior-cicada-nymph-mode-hook
  (set-syntax-table cicada-nymph-mode-syntax-table)
  (setq comint-prompt-regexp "^")
  (setq mode-line-process '(":%s")))

(defun cicada-nymph-args-to-list (string)
  (let ((where (string-match "[ \t]" string)))
    (cond ((null where) (list string))
          ((not (= where 0))
           (cons (substring string 0 where)
                 (cicada-nymph-args-to-list (substring string (+ 1 where)
                                                       (length string)))))
          (t (let ((pos (string-match "[^ \t]" string)))
               (if (null pos)
                   nil
                   (cicada-nymph-args-to-list (substring string pos
                                                         (length string)))))))))


;;;###autoload
(defun run-cicada-nymph (cmd)
  "Run an inferior cicada-nymph process, input and output via buffer *cicada-nymph*.
If there is a process already running in `*cicada-nymph*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `cicada-nymph-program-name').  Runs the hooks `inferior-cicada-nymph-mode-hook'
\(after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (interactive
   (list (read-string "Run cicada-nymph: " cicada-nymph-program-name)))
  (if (not (comint-check-proc "*cicada-nymph*"))
      (let ((cmdlist (cicada-nymph-args-to-list cmd)))
        (set-buffer (apply 'make-comint "cicada-nymph" (car cmdlist)
                           nil (cdr cmdlist)))
        (inferior-cicada-nymph-mode)))
  (setq cicada-nymph-program-name cmd)
  (setq cicada-nymph-process-buffer "*cicada-nymph*")
  ;; (pop-to-buffer "*cicada-nymph*")
  (split-window-for-cicada-nymph-with-named-buffer "*cicada-nymph*")
  (other-window 1))


(defun split-window-for-cicada-nymph-with-named-buffer (buffer-name-string)
  (interactive)
  (cond
    ((= 1 (count-windows))
     (progn
       (split-window-horizontally (floor (- (* 0.38 (window-width)))))
       (other-window 1)
       (switch-to-buffer buffer-name-string)
       (other-window -1)))
    ((not (cl-find buffer-name-string
                   (mapcar (lambda (w) (buffer-name (window-buffer w)))
                           (window-list))
                   :test 'equal))
     (progn
       (other-window 1)
       (switch-to-buffer buffer-name-string)
       (other-window -1)))))


(defun cicada-nymph-send-line ()
  (interactive)
  (move-beginning-of-line nil)
  (cua-set-mark)
  (move-end-of-line nil)
  (cicada-nymph-send-region (region-beginning) (region-end))
  (cua-set-mark))

(defun cicada-nymph-send-line-and-split-window-and-goto-next-line ()
  (interactive)
  (split-window-for-cicada-nymph-with-named-buffer "*cicada-nymph*")
  (cicada-nymph-send-line)
  (cond
    ((looking-at (rx buffer-end)) (insert 10))
    (:else
     (let ()
       (next-line)
       (move-beginning-of-line nil)))))


(defun cicada-nymph-send-region (start end)
  "Send the current region to the inferior cicada-nymph process."
  (interactive "r")
  (comint-send-region (cicada-nymph-proc) start end)
  (comint-send-string (cicada-nymph-proc) "\n"))

(defun cicada-nymph-send-region-and-split-window (start end)
  "Send the current region to the inferior cicada-nymph process."
  (interactive "r")
  (split-window-for-cicada-nymph-with-named-buffer "*cicada-nymph*")
  (comint-send-region (cicada-nymph-proc) start end)
  (comint-send-string (cicada-nymph-proc) "\n"))

(defun cicada-nymph-switch-to-interactive (eob-p)
  "Switch to the cicada-nymph process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (get-buffer cicada-nymph-process-buffer)
      (pop-to-buffer cicada-nymph-process-buffer)
      (error "No current process buffer.  See variable `cicada-nymph-process-buffer'"))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))


(defvar cicada-nymph-process-buffer nil "*The current scheme process buffer.

MULTIPLE PROCESS SUPPORT
===========================================================================
Cmuscheme.el supports, in a fairly simple fashion, running multiple Scheme
processes.  To run multiple Scheme processes, you start the first up with
\\[run-scheme].  It will be in a buffer named *scheme*.  Rename this buffer
with \\[rename-buffer].  You may now start up a new process with another
\\[run-scheme].  It will be in a new buffer, named *scheme*.  You can
switch between the different process buffers with \\[switch-to-buffer].

Commands that send text from source buffers to Scheme processes --
like `scheme-send-definition' or `scheme-compile-region' -- have to choose a
process to send to, when you have more than one Scheme process around.  This
is determined by the global variable `scheme-buffer'.  Suppose you
have three inferior Schemes running:
    Buffer      Process
    foo         scheme
    bar         scheme<2>
    *scheme*    scheme<3>
If you do a \\[scheme-send-definition-and-go] command on some Scheme source
code, what process do you send it to?

- If you're in a process buffer (foo, bar, or *scheme*),
  you send it to that process.
- If you're in some other buffer (e.g., a source file), you
  send it to the process attached to buffer `scheme-buffer'.
This process selection is performed by function `scheme-proc'.

Whenever \\[run-scheme] fires up a new process, it resets `scheme-buffer'
to be the new process's buffer.  If you only run one process, this will
do the right thing.  If you run multiple processes, you can change
`scheme-buffer' to another process buffer with \\[set-variable].

More sophisticated approaches are, of course, possible.  If you find yourself
needing to switch back and cicada-nymph between multiple processes frequently,
you may wish to consider ilisp.el, a larger, more sophisticated package
for running inferior Lisp and Scheme processes.  The approach taken here is
for a minimal, simple implementation.  Feel free to extend it.")

(defun cicada-nymph-proc ()
  "Return the current cicada-nymph process.  See variable `cicada-nymph-process-buffer'."
  (let ((proc (get-buffer-process (if (eq major-mode 'inferior-cicada-nymph-mode)
                                      (current-buffer)
                                      cicada-nymph-process-buffer))))
    (or proc
        (error "No current process.  See variable `cicada-nymph-process-buffer'"))))
