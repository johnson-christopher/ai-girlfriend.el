;;; ai-girlfriend --- ai-girlfriend-git.el --- copilot chat git operation -*- lexical-binding: t; -*-

;; Copyright (C) 2024  ai-girlfriend maintainers

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;;; Code:

(require 'aio)

(require 'ai-girlfriend-copilot)
(require 'ai-girlfriend-frontend)
(require 'ai-girlfriend-spinner)
(require 'ai-girlfriend-prompts)

(defcustom ai-girlfriend-commit-model nil
  "The model to use specifically for commit message generation.
When nil, falls back to `ai-girlfriend-default-model`.
Set via `ai-girlfriend-set-commit-model'."
  :type '(choice (const :tag "Use default model" nil) (string :tag "Specific model"))
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-ignored-commit-files
  '("pnpm-lock.yaml"
    "package-lock.json"
    "yarn.lock"
    "poetry.lock"
    "Cargo.lock"
    "go.sum"
    "composer.lock"
    "Gemfile.lock"
    "requirements.txt"
    "*.pyc"
    "*.pyo"
    "*.pyd"
    "*.so"
    "*.dylib"
    "*.dll"
    "*.exe"
    "*.jar"
    "*.war"
    "*.ear"
    "node_modules/"
    "vendor/"
    "dist/"
    "build/")
  "List of file patterns to ignore when generating commit messages.
These are typically large generated files like lock files or build artifacts
that don't need to be included in commit message generation.
Supports glob patterns like `*.lock' or `node_modules/'."
  :type '(repeat string)
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-use-difftastic nil
  "Whether to use difftastic for generating diffs when available.
Difftastic provides syntax-aware diffs that are often more readable.
Requires the `difft` command to be installed.

Note: Difftastic is experimental here.  It is designed for human reviewers;
LLMs may understand standard git diff output better."
  :type 'boolean
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-git-wait-message-format
  "# [copilot:%s] Generating commit message..."
  "Format string for the message displayed while generating a commit message.
The %s placeholder will be replaced by the model name."
  :type 'string
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-git-regenerate-wait-message-format
  "# [copilot:%s] Regenerating commit message..."
  "Format string for the message displayed while regenerating a commit message.
The %s placeholder will be replaced by the model name."
  :type 'string
  :group 'ai-girlfriend)

(defvar ai-girlfriend--git-commit-instance nil
  "Persistent instance for Git commit message generation.")

(aio-defun
  ai-girlfriend--exec
  (&rest command)
  "Asynchronously execute command COMMAND and return its output string."
  (let ((promise (aio-promise))
        (buf (generate-new-buffer " *ai-girlfriend-shell-command*")))
    (set-process-sentinel
     (apply #'start-process "ai-girlfriend-shell-command" buf command)
     (lambda (proc _signal)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer buf
           (let ((data (buffer-string)))
             (aio-resolve
              promise
              (lambda ()
                (if (> (length data) 0)
                    (substring data 0 -1)
                  "")))))
         (kill-buffer buf))))
    (aio-await promise)))

(aio-defun
  ai-girlfriend--git-top-level () "Get top folder of current git repo."
  (when (executable-find "git")
    (let* ((git-dir
            (aio-await
             (ai-girlfriend--exec "git" "rev-parse" "--absolute-git-dir")))
           (default-directory git-dir)
           cdup)
      (cond
       ((file-exists-p "gitdir")
        (with-temp-buffer
          (insert-file-contents "gitdir")
          (file-name-directory (buffer-string))))
       ((and (setq cdup
                   (aio-await
                    (ai-girlfriend--exec "git" "rev-parse" "--show-cdup")))
             (not (string-empty-p cdup)))
        cdup)
       (t
        (file-name-directory git-dir))))))


(aio-defun
  ai-girlfriend--git-ls-files (repo-root)
  "Return a list of git managed files in REPO-ROOT.
Uses `git ls-files` to retrieve files that are tracked or not ignored by
Git.  REPO-ROOT must be git top directory."
  (let* ((default-directory repo-root)
         (ls-output
          (aio-await
           (ai-girlfriend--exec
            "git"
            "--no-pager"
            "ls-files"
            "--full-name"
            "--cached"
            "--others"
            "--exclude-standard")))
         (all-files (split-string ls-output "\n" t)))
    (mapcar (lambda (file) (expand-file-name file repo-root)) all-files)))

(defun ai-girlfriend--format-git-context (status diff)
  "Format git context information for commit message generation.
STATUS is the output of git status command.
DIFF is the output of git diff command."
  (let ((commented-status
         (if (string-empty-p status)
             ""
           (replace-regexp-in-string "^" "# " status))))
    (concat
     "<git_context>\n" "# Git Status Summary:\n" commented-status
     (if (string-empty-p commented-status)
         "\n"
       "\n\n")
     "<git_diff>\n" diff
     (if (or (string-empty-p diff) (string-suffix-p "\n" diff))
         ""
       "\n")
     "</git_diff>\n" "</git_context>")))

(defun ai-girlfriend--difftastic-available-p ()
  "Check if difftastic is available on the system."
  (and ai-girlfriend-use-difftastic (executable-find "difft")))

(defun ai-girlfriend--get-diff-command (files-to-include use-difftastic)
  "Get the appropriate diff command based on configuration.
FILES-TO-INCLUDE is the list of files to include in the diff.
USE-DIFFTASTIC is non-nil to use difftastic if available."
  (if (and use-difftastic (ai-girlfriend--difftastic-available-p))
      (append
       (list
        "git"
        "-c"
        "diff.external=difft --display=inline --color=never --syntax-highlight=off"
        "--no-pager"
        "diff"
        "--cached"
        "--ext-diff"
        "--no-color"
        "--")
       files-to-include)
    (append
     (list "git" "--no-pager" "diff" "--cached" "--no-color" "--")
     files-to-include)))

(aio-defun
  ai-girlfriend--get-diff-content ()
  "Get the diff content of staged changes.

Returns a string containing the diff content, formatted by
`ai-girlfriend--format-git-context`."
  (let* ((default-directory
          (or (aio-await (ai-girlfriend--git-top-level))
              (user-error "Not inside a Git repository")))
         (staged-files
          (split-string (aio-await
                         (ai-girlfriend--exec
                          "git" "--no-pager" "diff" "--cached" "--name-only"))
                        "\n" t))
         (files-to-include
          (cl-remove-if
           (lambda (file)
             (cl-some
              (lambda (pattern)
                (or (string-match-p
                     (wildcard-to-regexp pattern) file)
                    (and (string-suffix-p "/" pattern)
                         (string-prefix-p pattern file))))
              ai-girlfriend-ignored-commit-files))
           staged-files)))
    (when files-to-include
      (let* ((status
              (aio-await
               (ai-girlfriend--exec
                "git" "status" "--short" "--branch" "--untracked-files=no")))
             (diff-output
              (aio-await
               (apply #'ai-girlfriend--exec
                      (ai-girlfriend--get-diff-command
                       files-to-include ai-girlfriend-use-difftastic)))))
        (ai-girlfriend--format-git-context status diff-output)))))

(defun ai-girlfriend--ensure-commit-instance (&optional repo-root)
  "Ensure the commit INSTANCE exists, creating it if necessary.

Optional REPO-ROOT specifies the Git repository's top-level directory."
  (let ((recreate-instance t)
        (instance-dir
         (or repo-root
             (file-name-directory (or (buffer-file-name) default-directory)))))
    (if (and ai-girlfriend--git-commit-instance
             (ai-girlfriend-p ai-girlfriend--git-commit-instance)
             (eq (ai-girlfriend-type ai-girlfriend--git-commit-instance) 'commit))
        (if (equal
             (ai-girlfriend-directory ai-girlfriend--git-commit-instance)
             instance-dir)
            (setq recreate-instance nil)
          (progn
            (ai-girlfriend--debug
             'commit
             "Commit instance exists for %s, but current context is %s. Recreating."
             (ai-girlfriend-directory ai-girlfriend--git-commit-instance)
             instance-dir)
            (ai-girlfriend-clear-git-commit-instance)))
      (setq recreate-instance t))

    (when recreate-instance
      (ai-girlfriend--debug
       'commit
       "Creating/recreating commit instance for directory: %s"
       instance-dir)
      (let ((instance
             (ai-girlfriend--create
              instance-dir ai-girlfriend-commit-model 'commit)))
        (setq ai-girlfriend--git-commit-instance instance)
        (unless (memq instance ai-girlfriend--instances)
          (push instance ai-girlfriend--instances))))
    ai-girlfriend--git-commit-instance))

(defun ai-girlfriend--get-git-commit-template-comments ()
  "Extract comments (lines starting with #) from the commit buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((comments ""))
      (while (re-search-forward "^#.*$" nil t)
        (setq comments (concat comments (match-string 0) "\n")))
      comments)))

(cl-defstruct
    ai-girlfriend--commit-callback-params
  "Parameters for commit message generation callback.
All fields are required."
  instance ; The commit instance
  current-buf ; Buffer where commit message will be inserted
  start-pos ; Starting position in current-buf
  accumulated-content ; Accumulated content so far
  template-comments ; Original commit template comments
  wait-prompt ; Temporary prompt shown while generating
  user-prompt-for-this-turn ; The prompt that led to this assistant response
  out-of-context-for-ask) ; Whether ai-girlfriend--ask was called with out-of-context

(defun ai-girlfriend--commit-callback (params)
  "Callback function for handling commit message generation stream.
PARAMS is a `ai-girlfriend--commit-callback-params' struct containing:
- instance: The commit instance
- current-buf: Buffer where the commit message will be inserted
- start-pos: Starting position in current-buf
- accumulated: Content accumulated so far
- template-comments: Original commit template comments
- wait-prompt: Temporary prompt shown while generating
- user-prompt: The prompt that led to this assistant response
- out-of-context: Whether `ai-girlfriend--ask' was called with out-of-context"
  (lambda (_cb-instance content)
    (with-current-buffer (ai-girlfriend--commit-callback-params-current-buf
                          params)
      (save-excursion
        (if (string= content ai-girlfriend--magic)
            (progn
              (ai-girlfriend--spinner-stop
               (ai-girlfriend--commit-callback-params-instance params))
              (with-current-buffer
                  (ai-girlfriend--commit-callback-params-current-buf params)
                (goto-char
                 (ai-girlfriend--commit-callback-params-start-pos params))
                (when (looking-at
                       (ai-girlfriend--commit-callback-params-wait-prompt
                        params))
                  (delete-region
                   (ai-girlfriend--commit-callback-params-start-pos params)
                   (+ (ai-girlfriend--commit-callback-params-start-pos params)
                      (length
                       (ai-girlfriend--commit-callback-params-wait-prompt
                        params))))))
              (goto-char (point-max))
              (delete-region (point-min) (point-max))
              (insert
               (ai-girlfriend--commit-callback-params-accumulated-content params)
               "\n\n"
               (ai-girlfriend--commit-callback-params-template-comments params))
              (if (ai-girlfriend--commit-callback-params-out-of-context-for-ask
                   params)
                  (setf
                   (ai-girlfriend-history
                    (ai-girlfriend--commit-callback-params-instance params))
                   `((:content
                      ,(ai-girlfriend--commit-callback-params-accumulated-content
                        params)
                      :role "assistant")
                     (:content
                      ,(ai-girlfriend--commit-callback-params-user-prompt-for-this-turn
                        params)
                      :role "user")))
                (setf
                 (ai-girlfriend-history
                  (ai-girlfriend--commit-callback-params-instance params))
                 (cons
                  `(:content
                    ,(ai-girlfriend--commit-callback-params-accumulated-content
                      params)
                    :role "assistant")
                  (ai-girlfriend-history
                   (ai-girlfriend--commit-callback-params-instance params))))))
          (progn
            (when (string=
                   (ai-girlfriend--commit-callback-params-accumulated-content
                    params)
                   "")
              (goto-char
               (ai-girlfriend--commit-callback-params-start-pos params))
              (when (looking-at
                     (ai-girlfriend--commit-callback-params-wait-prompt params))
                (delete-region
                 (ai-girlfriend--commit-callback-params-start-pos params)
                 (+ (ai-girlfriend--commit-callback-params-start-pos params)
                    (length
                     (ai-girlfriend--commit-callback-params-wait-prompt
                      params))))))
            (goto-char (ai-girlfriend--commit-callback-params-start-pos params))
            (delete-region
             (ai-girlfriend--commit-callback-params-start-pos params)
             (min (+ (ai-girlfriend--commit-callback-params-start-pos params)
                     (length
                      (ai-girlfriend--commit-callback-params-accumulated-content
                       params)))
                  (point-max)))
            (setf (ai-girlfriend--commit-callback-params-accumulated-content
                   params)
                  (concat
                   (ai-girlfriend--commit-callback-params-accumulated-content
                    params)
                   content))
            (insert
             (ai-girlfriend--commit-callback-params-accumulated-content
              params))))))))

(defmacro ai-girlfriend--with-commit-context (&rest body)
  "Execute BODY with commit-specific context.
This involves setting `ai-girlfriend-prompt` to `ai-girlfriend-commit-prompt`
and temporarily disabling the org frontend's `create-req-fn` if active."
  `(let ((ai-girlfriend-prompt ai-girlfriend-commit-prompt)
         (frontend (ai-girlfriend--get-frontend))
         (original-org-create-req-fn nil))
     (when (and frontend (eq (ai-girlfriend-frontend-id frontend) 'org))
       (setq original-org-create-req-fn
             (ai-girlfriend-frontend-create-req-fn frontend))
       (setf (ai-girlfriend-frontend-create-req-fn frontend) nil)
       (ai-girlfriend--debug
        'commit "Temporarily disabled org frontend create-req-fn for commit."))
     (unwind-protect
         (progn
           ,@body)
       (when original-org-create-req-fn
         (setf (ai-girlfriend-frontend-create-req-fn frontend)
               original-org-create-req-fn)))))

;;;###autoload (autoload 'ai-girlfriend-insert-commit-message-when-ready "ai-girlfriend" nil t)
(defun ai-girlfriend-insert-commit-message-when-ready ()
  "Generate and insert a commit message using GitHub Copilot."
  (interactive)
  (when buffer-read-only
    (signal
     'buffer-read-only (format "Buffer `%s' is read-only" (buffer-name))))
  (aio-with-async
    (let* ((instance (ai-girlfriend--ensure-commit-instance))
           (current-buf (current-buffer))
           (start-pos (point))
           (diff-content (aio-await (ai-girlfriend--get-diff-content)))
           (template-comments (ai-girlfriend--get-git-commit-template-comments))
           (wait-prompt
            (format ai-girlfriend-git-wait-message-format
                    (ai-girlfriend-model instance)))
           (accumulated-content ""))

      (setf (ai-girlfriend-history instance) nil)
      (ai-girlfriend--debug
       'commit "Commit instance history cleared for new generation.")
      (ai-girlfriend--debug 'commit "Starting initial commit message generation.")
      (ai-girlfriend--debug
       'commit "Diff content size: %d bytes, Model: %s"
       (if diff-content
           (length diff-content)
         0)
       (ai-girlfriend-model instance))

      (cond
       ((or (null diff-content) (string-empty-p diff-content))
        (ai-girlfriend--debug 'commit "No changes found in staging area.")
        (user-error
         "No staged changes found or diff content is empty.  Please stage some changes first"))
       (t
        (message "Generating commit message...")
        (insert wait-prompt "\n\n")
        (goto-char start-pos)
        (ai-girlfriend--debug
         'commit "User diff content to be sent:\n%s" diff-content)

        (condition-case err
            (ai-girlfriend--with-commit-context
             (ai-girlfriend--ask
              instance diff-content
              (ai-girlfriend--commit-callback
               (make-ai-girlfriend--commit-callback-params
                :instance instance
                :current-buf current-buf
                :start-pos start-pos
                :accumulated-content accumulated-content
                :template-comments template-comments
                :wait-prompt wait-prompt
                :user-prompt-for-this-turn diff-content
                :out-of-context-for-ask t))
              t))
          (error
           (ai-girlfriend--spinner-stop instance)
           (signal (car err) (cdr err)))))))))

;;;###autoload (autoload 'ai-girlfriend-insert-commit-message "ai-girlfriend" nil t)
(defun ai-girlfriend-insert-commit-message ()
  "Generate and insert a commit message using Copilot.
Uses the current staged changes in git to
generate an appropriate commit message.
Requires the repository to have staged changes.
This function is expected to be safe to open via magit when added to
`git-commit-setup-hook'.
Unlike `ai-girlfriend-insert-commit-message-when-ready', this function
delays invocation by 1 second to allow the buffer to be fully initialized."
  (interactive)
  ;;TODO: I really don't want to do anything delayed by time,
  ;; but I had to in order to make it work anyway.
  ;; In fact, we would like to get rid of this kind of messy control.
  (run-with-timer 1 nil #'ai-girlfriend-insert-commit-message-when-ready))

(defun ai-girlfriend--commit-buffer-has-message-p ()
  "Return non-nil if the current buffer has a non-comment, non-empty line.
Lines starting with `#' (git comment lines) and blank lines are ignored.
Content after the scissor line (`# --- >8 ---') is also ignored,
as it contains the verbose diff from `git commit -v'."
  (save-excursion
    (goto-char (point-min))
    (let ((bound
           (save-excursion
             (if (re-search-forward "^# -+ >8 -+$" nil t)
                 (line-beginning-position)
               (point-max)))))
      (catch 'found
        (while (< (point) bound)
          (let ((line
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
            (unless (or (string-empty-p (string-trim line))
                        (string-prefix-p "#" (string-trim-left line)))
              (throw 'found t)))
          (forward-line 1))
        nil))))

;;;###autoload (autoload 'ai-girlfriend-insert-commit-message-no-clobber "ai-girlfriend" nil t)
(defun ai-girlfriend-insert-commit-message-no-clobber ()
  "Generate commit message, but if has no existing message.
Like `ai-girlfriend-insert-commit-message',
but skip generation
if the commit buffer already contains a non-comment, non-blank line.
This is useful for `git-commit-setup-hook'
to avoid overwriting existing messages during
amend, rebase, or squash operations."
  (interactive)
  (if (ai-girlfriend--commit-buffer-has-message-p)
      (message "Commit buffer already has a message, skipping generation.")
    (ai-girlfriend-insert-commit-message)))

;;;###autoload (autoload 'ai-girlfriend-regenerate-commit-message "ai-girlfriend" nil t)
(defun ai-girlfriend-regenerate-commit-message ()
  "Regenerate and insert a new commit message using GitHub Copilot."
  (interactive)
  (aio-with-async
    (let* ((instance (ai-girlfriend--ensure-commit-instance))
           (current-buf (current-buffer))
           (start-pos (point))
           (template-comments (ai-girlfriend--get-git-commit-template-comments))
           (additional-instructions "")
           (wait-prompt "")
           (accumulated-content ""))

      (unless (ai-girlfriend-history instance)
        (message
         "No previous commit message generated in this session. Calling 'ai-girlfriend-insert-commit-message'.")
        (ai-girlfriend-insert-commit-message)
        (cl-return-from ai-girlfriend-regenerate-commit-message))

      (setq
       additional-instructions
       (let
           ((instr
             (read-string
              "Additional instructions for regeneration (or RET for default 'regenerate'): ")))
         (if (string-empty-p instr)
             "Please regenerate a new one, taking the previous attempt and my request into account."
           instr)))
      (setq wait-prompt
            (format ai-girlfriend-git-regenerate-wait-message-format
                    (ai-girlfriend-model instance)))
      (ai-girlfriend--debug 'commit "Starting commit message regeneration.")
      (ai-girlfriend--debug
       'commit "Additional instructions: %s" additional-instructions)
      (ai-girlfriend--debug
       'commit
       "Current history for regeneration: %S"
       (ai-girlfriend-history instance))

      (message "Regenerating commit message...")
      (insert wait-prompt "\n\n")
      (goto-char start-pos)

      (condition-case err
          (ai-girlfriend--with-commit-context
           (ai-girlfriend--ask
            instance additional-instructions
            (ai-girlfriend--commit-callback
             (make-ai-girlfriend--commit-callback-params
              :instance instance
              :current-buf current-buf
              :start-pos start-pos
              :accumulated-content accumulated-content
              :template-comments template-comments
              :wait-prompt wait-prompt
              :user-prompt-for-this-turn additional-instructions
              :out-of-context-for-ask nil))
            nil))
        (error
         (ai-girlfriend--spinner-stop instance)
         (signal (car err) (cdr err)))))))

;;;###autoload
(defun ai-girlfriend-clear-git-commit-instance ()
  "Clear and remove the persistent Git commit instance."
  (interactive)
  (when ai-girlfriend--git-commit-instance
    (ai-girlfriend--debug 'commit "Clearing persistent Git commit instance.")
    (setq ai-girlfriend--instances
          (delq ai-girlfriend--git-commit-instance ai-girlfriend--instances))
    (when (ai-girlfriend-spinner-timer ai-girlfriend--git-commit-instance)
      (cancel-timer
       (ai-girlfriend-spinner-timer ai-girlfriend--git-commit-instance)))
    (setq ai-girlfriend--git-commit-instance nil)
    (message "Persistent Git commit instance cleared."))
  (unless ai-girlfriend--git-commit-instance
    (message "No active persistent Git commit instance to clear.")))

(provide 'ai-girlfriend-git)
;;; ai-girlfriend-git.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
