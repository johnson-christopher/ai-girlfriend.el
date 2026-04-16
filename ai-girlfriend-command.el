;;; ai-girlfriend --- ai-girlfriend-command.el --- copilot chat command -*- lexical-binding: t; -*-

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

(require 'polymode)

(require 'ai-girlfriend-copilot)
(require 'ai-girlfriend-git)
(require 'ai-girlfriend-model)
(require 'ai-girlfriend-prompt-mode)
(require 'ai-girlfriend-request)

;; customs
(defcustom ai-girlfriend-list-added-buffers-only nil
  "If non-nil, only show buffers that have been added to the Copilot chat list."
  :type 'boolean
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-list-show-path t
  "If t, show path of files in the Copilot chat list.
If nil, show only file names."
  :type 'boolean
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-list-show-relative-path t
  "If t, show relative path of buffers in the Copilot chat list.
If nil, show absolute path."
  :type 'boolean
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-default-save-dir
  (concat user-emacs-directory "ai-girlfriend")
  "Default directory to save chats."
  :type 'string
  :group 'ai-girlfriend)

;; Faces
(defface ai-girlfriend-list-selected-buffer-face
  '((t :inherit font-lock-keyword-face))
  "Face used for selected buffers in `ai-girlfriend' buffer list."
  :group 'ai-girlfriend)
(defface ai-girlfriend-list-default-face '((t :inherit default))
  "Face used for unselected buffers in `ai-girlfriend' buffer list."
  :group 'ai-girlfriend)

;; Variables
(defvar ai-girlfriend-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'ai-girlfriend-list-add-or-remove-buffer)
    (define-key map (kbd "SPC") 'ai-girlfriend-list-add-or-remove-buffer)
    (define-key map (kbd "C-c C-c") 'ai-girlfriend-list-clear-buffers)
    (define-key
     map (kbd "g")
     (lambda ()
       (interactive)
       (ai-girlfriend-list-refresh (ai-girlfriend--current-instance))))
    (define-key
     map (kbd "q")
     (lambda ()
       (interactive)
       (bury-buffer)
       (delete-window)))
    map)
  "Keymap for `ai-girlfriend-list-mode'.")

;; Functions
(define-derived-mode
  ai-girlfriend-list-mode
  special-mode
  "Copilot Chat List"
  "Major mode for listing and managing buffers in Copilot chat."
  (setq buffer-read-only t))

(defun ai-girlfriend-prompt-send ()
  "Send the prompt content to Copilot.
Retrieves the current prompt, displays it in the chat buffer, and sends it
to Copilot for processing."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (when instance
      (ai-girlfriend--display instance)
      (let ((prompt (ai-girlfriend--pop-current-prompt instance)))
        (ai-girlfriend--write-buffer
         instance
         (ai-girlfriend--format-data instance prompt 'prompt) nil)
        (with-current-buffer (ai-girlfriend--get-buffer instance)
          (recenter-top-bottom))
        (setf (ai-girlfriend-prompt-history-position instance) nil)
        (ai-girlfriend--ask instance prompt 'ai-girlfriend-prompt-cb)))))

;;;###autoload (autoload 'ai-girlfriend-ask-and-insert "ai-girlfriend" nil t)
(defun ai-girlfriend-ask-and-insert ()
  "Send to Copilot a custom prompt and insert answer in current buffer at point."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (prompt (read-from-minibuffer "Copilot prompt: "))
         (current-buf (current-buffer)))
    (ai-girlfriend--ask
     instance prompt
     (lambda (instance content)
       (ai-girlfriend-prompt-cb instance content current-buf)))))

(defun ai-girlfriend--ask-region (prompt beg end)
  "Send to Copilot a prompt followed by the current selected code.
Argument PROMPT is the prompt to send to Copilot.
BEG and END are the beginning and end positions of the region to explain."
  (let ((instance (ai-girlfriend--current-instance))
        (code (buffer-substring-no-properties beg end)))
    (ai-girlfriend--insert-and-send-prompt
     instance
     (concat
      (cdr (assoc prompt (ai-girlfriend--prompts)))
      (ai-girlfriend--format-code code)))))

;;;###autoload (autoload 'ai-girlfriend-explain "ai-girlfriend" nil t)
(defun ai-girlfriend-explain (beg end)
  "Ask Copilot to explain the current selected code.
BEG and END are the beginning and end positions of the region to explain."
  (interactive "r")
  (ai-girlfriend--ask-region 'explain beg end))

;;;###autoload (autoload 'ai-girlfriend-review "ai-girlfriend" nil t)
(defun ai-girlfriend-review (beg end)
  "Ask Copilot to review the current selected code.
BEG and END are the beginning and end positions of the region to review."
  (interactive "r")
  (ai-girlfriend--ask-region 'review beg end))

;;;###autoload (autoload 'ai-girlfriend-doc "ai-girlfriend" nil t)
(defun ai-girlfriend-doc (beg end)
  "Ask Copilot to write documentation for the current selected code.
BEG and END are the beginning and end positions of the region to document."
  (interactive "r")
  (ai-girlfriend--ask-region 'doc beg end))

;;;###autoload (autoload 'ai-girlfriend-fix "ai-girlfriend" nil t)
(defun ai-girlfriend-fix (beg end)
  "Ask Copilot to fix the current selected code.
BEG and END are the beginning and end positions of the region to fix."
  (interactive "r")
  (ai-girlfriend--ask-region 'fix beg end))

;;;###autoload (autoload 'ai-girlfriend-optimize "ai-girlfriend" nil t)
(defun ai-girlfriend-optimize (beg end)
  "Ask Copilot to optimize the current selected code.
BEG and END are the beginning and end positions of the region to optimize."
  (interactive "r")
  (ai-girlfriend--ask-region 'optimize beg end))

;;;###autoload (autoload 'ai-girlfriend-test "ai-girlfriend" nil t)
(defun ai-girlfriend-test (beg end)
  "Ask Copilot to generate test for the current selected code.
BEG and END are the beginning and end positions of the region to test."
  (interactive "r")
  (ai-girlfriend--ask-region 'test beg end))

(defun ai-girlfriend--insert-prompt (instance prompt)
  "Insert PROMPT in the Copilot Chat prompt region.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the text to insert in the prompt region."
  (let ((prompt-fn
         (ai-girlfriend-frontend-insert-prompt-fn (ai-girlfriend--get-frontend))))
    (when prompt-fn
      (funcall prompt-fn instance prompt))))

(defun ai-girlfriend--insert-and-send-prompt (instance prompt)
  "Helper function to prepare buffer and send PROMPT to Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the text to send to Copilot."
  (ai-girlfriend--insert-prompt instance prompt)
  (ai-girlfriend-prompt-send))

(defun ai-girlfriend--get-language ()
  "Get the current language of the buffer.
Derives language name from the major mode of the current buffer."
  (if (derived-mode-p 'prog-mode) ; current buffer is a programming language buffer
      (let* ((major-mode-str (symbol-name major-mode))
             (lang
              (replace-regexp-in-string
               "\\(?:-ts\\)?-mode$" "" major-mode-str)))
        lang)
    nil))

(defun ai-girlfriend--format-code (code)
  "Format code according to the frontend.
Argument CODE is the code to be formatted."
  (let ((format-fn
         (ai-girlfriend-frontend-format-code-fn (ai-girlfriend--get-frontend))))
    (if format-fn
        (funcall format-fn code (ai-girlfriend--get-language))
      code)))

;;;###autoload (autoload 'ai-girlfriend-explain-symbol-at-line "ai-girlfriend" nil t)
(defun ai-girlfriend-explain-symbol-at-line ()
  "Ask Copilot to explain symbol under point.
Given the code line as background info."
  (interactive)
  (let*
      ((instance (ai-girlfriend--current-instance))
       (symbol (thing-at-point 'symbol))
       (line
        (buffer-substring-no-properties
         (line-beginning-position) (line-end-position)))
       (prompt
        (format
         "Please explain what '%s' means in the context of this code line:\n%s"
         symbol (ai-girlfriend--format-code line))))
    (ai-girlfriend--insert-and-send-prompt instance prompt)))

;;;###autoload (autoload 'ai-girlfriend-explain-defun "ai-girlfriend" nil t)
(defun ai-girlfriend-explain-defun ()
  "Mark current function definition and ask Copilot to explain it, then unmark."
  (interactive)
  (save-excursion
    (mark-defun)
    (call-interactively 'ai-girlfriend-explain)
    (deactivate-mark)))

;;;###autoload (autoload 'ai-girlfriend-custom-prompt-function "ai-girlfriend" nil t)
(defun ai-girlfriend-custom-prompt-function ()
  "Mark current function and ask `ai-girlfriend' with custom prompt."
  (interactive)
  (save-excursion
    (mark-defun)
    (ai-girlfriend-custom-prompt-selection)
    (deactivate-mark)))

;;;###autoload (autoload 'ai-girlfriend-review-whole-buffer "ai-girlfriend" nil t)
(defun ai-girlfriend-review-whole-buffer ()
  "Mark whole buffer, ask Copilot to review it, then unmark.
It can be used to review the magit diff for my change, or other people's"
  (interactive)
  (save-excursion (ai-girlfriend-review (point-min) (point-max))))

;;;###autoload (autoload 'ai-girlfriend-switch-to-buffer "ai-girlfriend" nil t)
(defun ai-girlfriend-switch-to-buffer ()
  "Switch to Copilot Chat buffer.
Side by side with the current code editing buffer."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (unless (equal (pm-base-buffer) (ai-girlfriend--get-buffer instance))
      (switch-to-buffer-other-window (ai-girlfriend--get-buffer instance))))
  (ai-girlfriend-goto-input))

;;;###autoload (autoload 'ai-girlfriend-custom-prompt-selection "ai-girlfriend" nil t)
(defun ai-girlfriend-custom-prompt-selection (&optional custom-prompt)
  "Send to Copilot a custom prompt followed by the current selected code/buffer.
If CUSTOM-PROMPT is provided, use it instead of reading from the mini-buffer."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (prompt (or custom-prompt (read-from-minibuffer "Copilot prompt: ")))
         (code
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         (formatted-prompt
          (concat prompt "\n" (ai-girlfriend--format-code code))))
    (ai-girlfriend--insert-and-send-prompt instance formatted-prompt)))

;;;###autoload (autoload 'ai-girlfriend-custom-prompt-mini-buffer "ai-girlfriend" nil t)
(defun ai-girlfriend-custom-prompt-mini-buffer ()
  "Read a string with Helm completion, showing historical inputs."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (prompt "Question for ai-girlfriend: "))
    (setq ai-girlfriend--prompt-history
          (mapcar
           (lambda (msg) (plist-get msg :content))
           (seq-filter
            (lambda (msg)
              (equal (plist-get msg :role) "user"))
            (ai-girlfriend-history instance))))
    (let ((input (read-string prompt nil 'ai-girlfriend--prompt-history 0)))
      (ai-girlfriend--insert-and-send-prompt instance input))))

;;;###autoload (autoload 'ai-girlfriend-list "ai-girlfriend" nil t)
(defun ai-girlfriend-list ()
  "Open Copilot Chat list buffer."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (buffer (ai-girlfriend--get-list-buffer-create instance)))
    (with-current-buffer buffer
      (ai-girlfriend-list-mode))
    (ai-girlfriend-list-refresh instance)
    (switch-to-buffer buffer)))

;;;###autoload (autoload 'ai-girlfriend-display "ai-girlfriend" nil t)
(defun ai-girlfriend-display (&optional arg)
  "Display copilot chat buffer.
With prefix argument, explicitly ask for which instance to use.
Optional argument ARG if non-nil, force instance selection."
  (interactive "P")
  (let ((instance
         (if arg
             (ai-girlfriend--ask-for-instance)
           (ai-girlfriend--current-instance))))
    (ai-girlfriend--display instance)))

;;;###autoload (autoload 'ai-girlfriend "ai-girlfriend" nil t)
(defalias 'ai-girlfriend 'ai-girlfriend-display)

;;;###autoload (autoload 'ai-girlfriend-hide "ai-girlfriend" nil t)
(defun ai-girlfriend-hide ()
  "Hide copilot chat buffer."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (base-buffer (ai-girlfriend--get-buffer instance)))
    (dolist (window (window-list))
      (let ((buf (window-buffer window)))
        (when (or (eq buf base-buffer)
                  (eq
                   (with-current-buffer buf
                     (pm-base-buffer))
                   base-buffer))
          (delete-window window))))))

(defun ai-girlfriend-add-current-buffer ()
  "Add current buffer in sent buffers list."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (ai-girlfriend--add-buffer instance (current-buffer))
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-del-current-buffer ()
  "Remove current buffer from sent buffers list."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (ai-girlfriend--del-buffer instance (current-buffer))
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-add-buffers (buffers)
  "Add BUFFERS to sent buffers list."
  (interactive (list
                (completing-read-multiple
                 "Buffers: "
                 (mapcar #'buffer-name (buffer-list))
                 nil
                 t
                 (buffer-name (current-buffer)))))
  (let ((instance (ai-girlfriend--current-instance)))
    (mapc (lambda (buf) (ai-girlfriend--add-buffer instance buf)) buffers)
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-del-buffers (buffers)
  "Remove BUFFERS from sent buffers list."
  (interactive (list
                (completing-read-multiple
                 "Buffers: "
                 (mapcar #'buffer-name (buffer-list))
                 nil
                 t
                 (buffer-name (current-buffer)))))
  (let ((instance (ai-girlfriend--current-instance)))
    (mapc (lambda (buf) (ai-girlfriend--del-buffer instance buf)) buffers)
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-add-file (file-path)
  "Add FILE-PATH to `ai-girlfriend' buffers without changing current window layout."
  (interactive "fFile to add: ")
  (save-window-excursion
    (let ((current-buf (current-buffer)))
      (find-file file-path)
      (ai-girlfriend-add-current-buffer)
      (switch-to-buffer current-buf))))

(defun ai-girlfriend-add-buffers-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Copilot chat."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list)))
        (added-buffers '()))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when buffer-file-name
          (ai-girlfriend-add-current-buffer)
          (push (buffer-name buffer) added-buffers))))
    (message "Added buffers: %s" (string-join added-buffers ", "))))

(defun ai-girlfriend-add-files-under-dir ()
  "Add all files with same suffix as current file under current directory.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files
            (directory-files dir
                             t (concat "\\." current-suffix "$")
                             t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files)
                   max-files
                   current-suffix)
        (dolist (file files)
          (ai-girlfriend-add-file file))
        (message "Added %d files with suffix .%s"
                 (length files)
                 current-suffix)))))

(defun ai-girlfriend--buffer-list (instance)
  "Return a list of buffer with files in INSTANCE directory."
  (let* ((dir (ai-girlfriend-directory instance))
         (bufs-under-dir
          (cl-remove-if-not
           (lambda (buf)
             (with-current-buffer buf
               (and buffer-file-name
                    (file-in-directory-p buffer-file-name dir))))
           (buffer-list))))
    (cl-union bufs-under-dir (ai-girlfriend-buffers instance) :test #'eq)))

(defun ai-girlfriend-list-refresh (&optional instance)
  "Refresh the list of buffers in the current Copilot chat list buffer.
Optional argument INSTANCE specifies which instance to refresh the list for."
  (interactive)
  (unless instance
    (setq instance (ai-girlfriend--current-instance)))
  (setf (ai-girlfriend-buffers instance)
        (cl-remove-if-not #'buffer-live-p (ai-girlfriend-buffers instance)))
  (with-current-buffer (ai-girlfriend--get-list-buffer-create instance)
    (let* ((pt (point))
           (inhibit-read-only t)
           (buffers
            (if ai-girlfriend-list-added-buffers-only
                (ai-girlfriend-buffers instance)
              (ai-girlfriend--buffer-list instance)))
           (sorted-buffers
            (sort buffers
                  (lambda (a b)
                    (string<
                     (symbol-name (buffer-local-value 'major-mode a))
                     (symbol-name (buffer-local-value 'major-mode b)))))))
      (erase-buffer)
      (setq-local header-line-format
                  (concat "Copilot chat " (ai-girlfriend-directory instance)))
      (dolist (buffer sorted-buffers)
        (let* ((file-name (buffer-file-name buffer))
               (buffer-name
                (if (and file-name ai-girlfriend-list-show-path)
                    (if ai-girlfriend-list-show-relative-path
                        (file-relative-name file-name
                                            (ai-girlfriend-directory instance))
                      file-name)
                  (buffer-name buffer)))
               (cop-bufs (ai-girlfriend--get-buffers instance)))
          (when (and (not (string-prefix-p " " buffer-name))
                     (not (string-prefix-p "*" buffer-name)))
            (insert
             (propertize buffer-name
                         'face
                         (if (member buffer cop-bufs)
                             'ai-girlfriend-list-selected-buffer-face
                           'ai-girlfriend-list-default-face))
             "\n"))))
      (goto-char pt))))


(defun ai-girlfriend-list-add-or-remove-buffer ()
  "Add or remove the buffer at point from the Copilot chat list."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (buffer-name
          (buffer-substring (line-beginning-position) (line-end-position)))
         (buffer
          (if ai-girlfriend-list-show-path
              (or (get-file-buffer
                   (if ai-girlfriend-list-show-relative-path
                       (concat
                        (ai-girlfriend-directory instance) "/" buffer-name)
                     buffer-name))
                  (get-buffer buffer-name))
            (get-buffer buffer-name)))
         (cop-bufs (ai-girlfriend--get-buffers instance)))
    (when buffer
      (if (member buffer cop-bufs)
          (progn
            (ai-girlfriend--del-buffer instance buffer)
            (message "Buffer '%s' removed from Copilot chat list." buffer-name))
        (ai-girlfriend--add-buffer instance buffer)
        (message "Buffer '%s' added to Copilot chat list." buffer-name)))
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-list-clear-buffers ()
  "Clear all buffers from the Copilot chat list."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (ai-girlfriend--clear-buffers instance)
    (message "Cleared all buffers from Copilot chat list.")
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-prompt-split-and-list ()
  "Split prompt window and display buffer list."
  (interactive)
  (let ((split-window-preferred-function nil)
        (split-height-threshold nil)
        (split-width-threshold nil))
    (split-window-right (floor (* 0.8 (window-total-width)))))
  (other-window 1)
  (ai-girlfriend-list))

(defun ai-girlfriend-prompt-history-previous ()
  "Insert previous prompt in prompt buffer."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (user-messages
          (seq-filter
           (lambda (msg)
             (equal (plist-get msg :role) "user"))
           (ai-girlfriend-history instance)))
         (index (ai-girlfriend-prompt-history-position instance))
         (prompt
          (when (ai-girlfriend-history instance)
            (if (null index)
                (progn
                  (setf (ai-girlfriend-prompt-history-position instance) 0)
                  (plist-get (car user-messages) :content))
              (if (= index (1- (length user-messages)))
                  (plist-get (car (last user-messages)) :content)
                (setf (ai-girlfriend-prompt-history-position instance)
                      (1+ index))
                (plist-get (nth index user-messages) :content))))))
    (when prompt
      (ai-girlfriend--insert-prompt instance prompt))))

(defun ai-girlfriend-prompt-history-next ()
  "Insert next prompt in prompt buffer."
  (interactive)
  (let* ((instance (ai-girlfriend--current-instance))
         (user-messages
          (seq-filter
           (lambda (msg)
             (equal (plist-get msg :role) "user"))
           (ai-girlfriend-history instance)))
         (index (ai-girlfriend-prompt-history-position instance))
         (prompt
          (when (and (ai-girlfriend-history instance) index)
            (if (= 0 index)
                ""
              (setf
               index (1- index)
               (ai-girlfriend-prompt-history-position instance) index)
              (plist-get (nth index user-messages) :content)))))
    (when prompt
      (ai-girlfriend--insert-prompt instance prompt))))

(defun ai-girlfriend-reset (&optional keep-buffers)
  "Reset copilot chat session.
When called interactively with prefix argument, preserve the buffer list.
Optional argument KEEP-BUFFERS if non-nil, preserve the current buffer list."
  (interactive "P")
  (let* ((instance (ai-girlfriend--current-instance))
         (old-buffers
          (when keep-buffers
            (ai-girlfriend-buffers instance)))
         (buf (ai-girlfriend--get-buffer instance)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (setf
     (ai-girlfriend-history instance) nil
     (ai-girlfriend-prompt-history-position instance) nil
     (ai-girlfriend-yank-index instance) 1
     (ai-girlfriend-last-yank-start instance) nil
     (ai-girlfriend-last-yank-end instance) nil
     (ai-girlfriend-spinner-timer instance) nil
     (ai-girlfriend-spinner-index instance) 0
     (ai-girlfriend-spinner-status instance) nil)
    (unless old-buffers
      (setf (ai-girlfriend-buffers instance) nil))
    (ai-girlfriend--display instance)
    (ai-girlfriend-list-refresh instance)))

(defun ai-girlfriend-frontend-clean ()
  "Cleaning function."
  (interactive)
  (let ((clean-fn
         (ai-girlfriend-frontend-clean-fn (ai-girlfriend--get-frontend))))
    (when clean-fn
      (funcall clean-fn))
    (setq ai-girlfriend--frontend-init-p nil)))


(defun ai-girlfriend-send-to-buffer ()
  "Send the code block at point to buffer.
Replace selection if any."
  (interactive)
  (let ((send-fn
         (ai-girlfriend-frontend-send-to-buffer-fn
          (ai-girlfriend--get-frontend))))
    (when send-fn
      (funcall send-fn))))

(defun ai-girlfriend-copy-code-at-point ()
  "Copy the code block at point into kill ring."
  (interactive)
  (let ((copy-fn (ai-girlfriend-frontend-copy-fn (ai-girlfriend--get-frontend))))
    (when copy-fn
      (funcall copy-fn))))

(defun ai-girlfriend-goto-input ()
  "Go to the input area."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (when (equal (pm-base-buffer) (ai-girlfriend--get-buffer instance))
      (let ((goto-fn
             (ai-girlfriend-frontend-goto-input-fn
              (ai-girlfriend--get-frontend))))
        (when goto-fn
          (funcall goto-fn))))))

(defun ai-girlfriend--get-model-choices-with-wait ()
  "Get the list of available models for Copilot Chat.
waiting for fetch if needed.
If models haven't been fetched yet and no cache exists,
wait for the fetch to complete."
  (let ((models
         (seq-filter
          #'ai-girlfriend--model-enabled-p
          (if ai-girlfriend-model-ignore-picker
              (ai-girlfriend-connection-models ai-girlfriend--connection)
            (seq-filter
             #'ai-girlfriend--model-picker-enabled
             (ai-girlfriend-connection-models ai-girlfriend--connection))))))
    (if models
        (let* ((model-info-list
                (mapcar
                 (lambda (model)
                   (let* ((id (alist-get 'id model))
                          (name (alist-get 'name model))
                          (clean-name
                           (replace-regexp-in-string " (Preview)$" "" name))
                          (vendor (alist-get 'vendor model))
                          (capabilities (alist-get 'capabilities model))
                          (limits (alist-get 'limits capabilities))
                          (preview-p (eq t (alist-get 'preview model)))
                          (preview-text
                           (if preview-p
                               " (Preview)"
                             ""))
                          (prompt-tokens (alist-get 'max_prompt_tokens limits))
                          (output-tokens
                           (or (alist-get 'max_output_tokens limits)
                               (when (string-prefix-p "o1" id)
                                 100000)))
                          (prompt-text
                           (if prompt-tokens
                               (format "%dk" (round (/ prompt-tokens 1000)))
                             "?"))
                          (output-text
                           (if output-tokens
                               (format "%dk" (round (/ output-tokens 1000)))
                             "?")))
                     (list
                      :id id
                      :vendor vendor
                      :clean-name clean-name
                      :preview-text preview-text
                      :prompt-text prompt-text
                      :output-text output-text)))
                 models))
               (max-vendor-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :vendor)))
                        model-info-list)))
               (max-name-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :clean-name)))
                        model-info-list)))
               (max-preview-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :preview-text)))
                        model-info-list)))
               (max-prompt-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :prompt-text)))
                        model-info-list)))
               (max-output-width
                (apply #'max
                       (mapcar
                        (lambda (info)
                          (length (plist-get info :output-text)))
                        model-info-list)))
               (format-str
                (format "[%%-%ds] %%-%ds%%-%ds (Tokens in/out: %%%ds/%%%ds)"
                        max-vendor-width
                        max-name-width
                        max-preview-width
                        max-prompt-width
                        max-output-width)))
          ;; Return list of (name . id) pairs from fetched models, sorted by ID
          (sort (mapcar
                 (lambda (info)
                   (cons
                    (format format-str
                            (plist-get info :vendor)
                            (plist-get info :clean-name)
                            (plist-get info :preview-text)
                            (plist-get info :prompt-text)
                            (plist-get info :output-text))
                    (plist-get info :id)))
                 model-info-list)
                (lambda (a b) (string< (cdr a) (cdr b)))))
      ;; No models available - fetch and wait
      (progn
        ;; Try loading from cache first
        (let ((cached-models (ai-girlfriend--load-models-from-cache)))
          (if cached-models
              (progn
                (setf (ai-girlfriend-connection-models ai-girlfriend--connection)
                      cached-models)
                (ai-girlfriend--get-model-choices-with-wait))
            ;; No cache - need to fetch
            (message "No models available. Fetching from API...")
            (ai-girlfriend--auth)
            (let ((inhibit-quit t)) ; Prevent C-g during fetch
              (ai-girlfriend--request-models t)
              ;; Wait for models to be fetched (with timeout)
              (with-timeout (10 (error
                                 "Timeout waiting for models to be fetched"))
                (while (not
                        (ai-girlfriend-connection-models
                         ai-girlfriend--connection))
                  (sit-for 0.1)))
              (ai-girlfriend--get-model-choices-with-wait))))))))

;;;###autoload (autoload 'ai-girlfriend-set-model "ai-girlfriend" nil t)
(defun ai-girlfriend-set-model (model)
  "Set the Copilot Chat model to MODEL for the current instance.
Fetches available models from the API if not already fetched."
  (interactive
   (let*
       ((choices (ai-girlfriend--get-model-choices-with-wait))
        (max-id-width
         (apply #'max (mapcar (lambda (choics) (length (cdr choics))) choices)))
        ;; Create completion list with ID as prefix for unique identification
        (completion-choices
         (mapcar
          (lambda (choice)
            (let ((name (car choice))
                  (id (cdr choice)))
              (cons (format (format "[%%-%ds] %%s" max-id-width) id name) id)))
          choices))
        (choice
         (completing-read
          "Select Copilot Chat model: " (mapcar 'car completion-choices)
          nil t)))
     ;; Extract model ID from the selected choice
     (let ((model-value (cdr (assoc choice completion-choices))))
       (when ai-girlfriend-debug
         (message "Setting model to: %s" model-value))
       (list model-value))))

  ;; Set the model value only for current instance
  (let ((instance (ai-girlfriend--current-instance)))
    (setf (ai-girlfriend-model instance) model)
    (message "Copilot Chat model set to %s for current instance" model)))

(defun ai-girlfriend-yank ()
  "Insert last code block given by `ai-girlfriend'."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (setf
     (ai-girlfriend-yank-index instance) 1
     (ai-girlfriend-last-yank-start instance) nil
     (ai-girlfriend-last-yank-end instance) nil)
    (ai-girlfriend--yank instance)))

(defun ai-girlfriend-yank-pop (&optional inc)
  "Replace just-yanked code block with a different block.
INC is the number to use as increment for index in block ring."
  (interactive "*p")
  (let ((instance (ai-girlfriend--current-instance)))
    (if (not (eq last-command 'ai-girlfriend-yank-pop))
        (unless (eq last-command 'ai-girlfriend-yank)
          (error "Previous command was not a yank")))
    (if inc
        (setf (ai-girlfriend-yank-index instance)
              (+ (ai-girlfriend-yank-index instance) inc))
      (setf (ai-girlfriend-yank-index instance)
            (1+ (ai-girlfriend-yank-index instance))))
    (ai-girlfriend--yank instance)
    (setq this-command 'ai-girlfriend-yank-pop)))

(defun ai-girlfriend--yank (instance)
  "Insert at point the code block at the current index in the block ring.
Argument INSTANCE is the copilot chat instance to use."
  (when-let* ((yank-fn
               (ai-girlfriend-frontend-yank-fn (ai-girlfriend--get-frontend))))
    (funcall yank-fn instance)))

;;;###autoload (autoload 'ai-girlfriend-clear-auth-cache "ai-girlfriend" nil t)
(defun ai-girlfriend-clear-auth-cache ()
  "Clear the auth cache for Copilot Chat."
  (interactive)
  (ai-girlfriend-reset)
  ;; remove ai-girlfriend-token-cache file
  (let ((token-cache-file (expand-file-name ai-girlfriend-token-cache))
        (github-token-file (expand-file-name ai-girlfriend-github-token-file)))
    (when (file-exists-p token-cache-file)
      (delete-file token-cache-file))
    (when (file-exists-p github-token-file)
      (delete-file github-token-file)))
  (message "Auth cache cleared.")
  (setq ai-girlfriend--connection (ai-girlfriend-connection--make)))

;;;###autoload (autoload 'ai-girlfriend-reset-models "ai-girlfriend" nil t)
(defun ai-girlfriend-reset-models ()
  "Reset model cache and fetch models again.
This is useful when GitHub adds new models or updates model capabilities.
Clears model cache from memory and disk, then triggers background fetch."
  (interactive)
  ;; Clear models
  (setf (ai-girlfriend-connection-models ai-girlfriend--connection) nil)
  (setf (ai-girlfriend-connection-last-models-fetch-time
         ai-girlfriend--connection)
        0)

  ;; Remove cached models file if it exists
  (let ((models-cache-file (expand-file-name ai-girlfriend-models-cache-file)))
    (when (file-exists-p models-cache-file)
      (delete-file models-cache-file)
      (when ai-girlfriend-debug
        (message "Removed models cache file: %s" models-cache-file))))

  ;; Trigger a background fetch
  (message "Models cache cleared. Fetching updated models...")
  (ai-girlfriend--fetch-models-async)

  ;; Return nil for programmatic usage
  nil)

(aio-defun
  ai-girlfriend--add-workspace (instance only-open)
  "Add all files matching an instance to buffer list.
INSTANCE is the copilot chat instance to use.  If ONLY-OPEN is non-nil,
add only files already visited by a buffer."
  (ai-girlfriend--clear-buffers instance)
  (let* ((default-directory (ai-girlfriend-directory instance))
         (repo-root (aio-await (ai-girlfriend--git-top-level)))
         (files
          (if (null repo-root)
              (directory-files-recursively default-directory ".*" nil)
            (aio-await (ai-girlfriend--git-ls-files repo-root))))
         (file-count (length files)))
    (if (and (> file-count 50)
             (not only-open)
             (not
              (yes-or-no-p
               (format "Found %d files, add them all? " file-count))))
        (message "Workspace file addition cancelled.")
      (if only-open
          (mapcar
           (lambda (buffer)
             (let ((file (buffer-file-name buffer)))
               (when (and file (member (expand-file-name file) files))
                 (ai-girlfriend--add-buffer instance buffer))))
           (buffer-list))
        (mapcar (lambda (file) (ai-girlfriend-add-file file)) files)))))

(defun ai-girlfriend-add-workspace-buffers ()
  "Add all open files matching an instance.
If a gitignore file is present in the instance directory, it will be used to
filter files.  Buffer list is cleared and all buffer displaying a file in the
instance directory will be added."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (aio-with-async
      (aio-await (ai-girlfriend--add-workspace instance t))
      (ai-girlfriend-list-refresh instance))))

(defun ai-girlfriend-add-workspace ()
  "Add all files in instance's directory.
All files in instance's directory and its subdirectories are added to
context.  If a gitignore file is present in the instance directory, it
will be used to filter files.  Buffer list is cleared and all buffer
displaying a file in the instance directory will be added."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (aio-with-async
      (aio-await (ai-girlfriend--add-workspace instance nil))
      (ai-girlfriend-list-refresh instance))))

;;;###autoload (autoload 'ai-girlfriend-kill-instance "ai-girlfriend" nil t)
(defun ai-girlfriend-kill-instance ()
  "Interactively kill a selected copilot chat instance.
All its associated buffers are killed."
  (interactive)
  (let* ((instance (ai-girlfriend--choose-instance)))
    (when instance
      (ai-girlfriend--kill-instance instance))))

;;;###autoload (autoload 'ai-girlfriend-set-commit-model "ai-girlfriend" nil t)
(defun ai-girlfriend-set-commit-model (model)
  "Set the model to use specifically for commit message generation to MODEL."
  (interactive (let* ((choices (ai-girlfriend--get-model-choices-with-wait))
                      (max-id-width
                       (apply #'max
                              (mapcar
                               (lambda (choics)
                                 (length (cdr choics)))
                               choices)))
                      (completion-choices
                       (mapcar
                        (lambda (choice)
                          (let ((name (car choice))
                                (id (cdr choice)))
                            (cons
                             (format (format "[%%-%ds] %%s" max-id-width)
                                     id
                                     name)
                             id)))
                        choices))
                      (choice
                       (completing-read "Select commit message model: "
                                        (mapcar 'car completion-choices)
                                        nil
                                        t)))
                 (let ((model-value (cdr (assoc choice completion-choices))))
                   (when ai-girlfriend-debug
                     (message "Setting commit model to: %s" model-value))
                   (list model-value))))

  (setq ai-girlfriend-commit-model model)
  (when ai-girlfriend--git-commit-instance
    (setf (ai-girlfriend-model ai-girlfriend--git-commit-instance)
          (or model ai-girlfriend-default-model)))
  (message "Commit message model set to %s" model))

;;;###autoload (autoload 'ai-girlfriend-save "ai-girlfriend" nil t)
(defun ai-girlfriend-save ()
  "Save an instance to a file."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance))
        (current-date (format-time-string "%Y_%m_%d_%H%M%S")))
    (when instance
      (let* ((default-path
              (or (ai-girlfriend-file-path instance)
                  (format "%s/%s_%s.el"
                          ai-girlfriend-default-save-dir
                          (replace-regexp-in-string
                           "/" "_" (ai-girlfriend-directory instance))
                          current-date)))
             (default-dir (file-name-directory default-path))
             (default-file (file-name-nondirectory default-path))
             (file
              (read-file-name "Save instance to file: "
                              default-dir
                              nil
                              nil
                              default-file)))
        (ai-girlfriend--save-instance instance file)
        (setf (ai-girlfriend-file-path instance) file)
        (message "Saved instance to %s" file)))))

;;;###autoload (autoload 'ai-girlfriend-load "ai-girlfriend" nil t)
(defun ai-girlfriend-load ()
  "Load an instance from a file."
  (interactive)
  (let ((file
         (read-file-name "File to load: " ai-girlfriend-default-save-dir nil t)))
    (ai-girlfriend--load-instance file)
    (message "Loaded instance from %s" file)))

;;;###autoload (autoload 'ai-girlfriend-quotas "ai-girlfriend" nil t)
(defun ai-girlfriend-quotas ()
  "Display the current Copilot Chat quotas."
  (interactive)
  (ai-girlfriend--quotas))

;;;###autoload (autoload 'ai-girlfriend-cancel "ai-girlfriend" nil t)
(defun ai-girlfriend-cancel ()
  "Cancel the current Copilot Chat request."
  (interactive)
  (let ((instance (ai-girlfriend--current-instance)))
    (ai-girlfriend--cancel instance)))

(provide 'ai-girlfriend-command)
;;; ai-girlfriend-command.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
