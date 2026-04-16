;;; ai-girlfriend --- ai-girlfriend-markdown.el --- copilot chat interface, markdown frontend -*- lexical-binding: t; -*-

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

(require 'markdown-mode)
(require 'polymode)

(require 'ai-girlfriend-common)
(require 'ai-girlfriend-instance)
(require 'ai-girlfriend-prompt-mode)
(require 'ai-girlfriend-prompts)

;;; Constants
(defconst ai-girlfriend--markdown-delimiter (concat "# ╭──── Chat Input ────╮")
  "The delimiter used to identify copilot chat input.")

;;; Polymode
(define-derived-mode
  ai-girlfriend-markdown-prompt-mode
  markdown-mode
  "Copilot Chat markdown Prompt"
  "Major mode for the Copilot Chat Prompt region."
  (setq
   major-mode 'ai-girlfriend-markdown-prompt-mode
   mode-name "Copilot Chat markdown prompt")
  (ai-girlfriend-prompt-mode))

(define-hostmode
  poly-copilot-markdown-hostmode
  :mode 'ai-girlfriend-markdown-prompt-mode)

(define-innermode
  poly-copilot-markdown-innermode
  :mode 'markdown-view-mode
  :head-matcher "\\`" ; Match beginning of buffer
  :tail-matcher (concat ai-girlfriend--markdown-delimiter "\n")
  :head-mode 'inner
  :tail-mode 'host)

(declare-function ai-girlfriend-markdown-poly-mode "ai-girlfriend-markdown"
                  "Polymode for Copilot Chat Markdown.")

(define-polymode
  ai-girlfriend-markdown-poly-mode
  :hostmode 'poly-copilot-markdown-hostmode
  :innermodes '(poly-copilot-markdown-innermode))


;;; Functions
(defun ai-girlfriend--markdown-format-data (instance content type)
  "Format the CONTENT according to the frontend.
INSTANCE is `ai-girlfriend' instance to use.
Argument TYPE is the type of data to format: `answer` or `prompt`."
  (let ((data ""))
    (if (eq type 'prompt)
        (progn
          (setf (ai-girlfriend-first-word-answer instance) t)
          (setq data
                (concat
                 "\n# "
                 (format-time-string "*[%T]* You\n")
                 (format "%s\n" content))))
      (when (ai-girlfriend-first-word-answer instance)
        (setf (ai-girlfriend-first-word-answer instance) nil)
        (setq data
              (concat
               "\n## "
               (concat
                (format-time-string "*[%T]* ")
                (format "Copilot(%s):\n" (ai-girlfriend-model instance))))))
      (setq data (concat data content)))
    data))

(defun ai-girlfriend--markdown-format-code (code language)
  "Format code for markdown frontend.
Argument CODE is the code to format.
Argument LANGUAGE is the language of the code."
  (if language
      (format "\n```%s\n%s\n```\n" language code)
    code))

(defun ai-girlfriend--markdown-format-buffer (buffer instance)
  "Format the content of a buffer into a Markdown-compatible string.
This function extracts the content of the specified BUFFER, determines
its file name, relative path, and programming language, and formats the
content as a Markdown code block.
INSTANCE is `ai-girlfriend' instance, used to retrieve relative file path."
  (with-current-buffer buffer
    (let* ((file-name (buffer-file-name))
           (relative-path
            (if file-name
                (file-relative-name file-name (ai-girlfriend-directory instance))
              (buffer-name)))
           (content
            (ai-girlfriend--markdown-format-code
             (buffer-substring-no-properties (point-min) (point-max))
             relative-path)))
      content)))

(defun ai-girlfriend--get-markdown-block-content-at-point ()
  "Get the content of the markdown block at point."
  (let* ((props (text-properties-at (point)))
         (face (plist-get props 'face)))
    (when (and (listp face)
               (or (memq 'markdown-pre-face face)
                   (memq 'markdown-code-face face)))
      (let* ((begin-block (previous-single-property-change (point) 'face))
             (end-block (next-single-property-change (point) 'face))
             (content
              (when (and begin-block end-block)
                (buffer-substring-no-properties begin-block end-block)))
             ;; Try to get language from previous text properties
             (lang-props
              (text-properties-at (max (- begin-block 1) (point-min))))
             (lang (plist-get lang-props 'markdown-language)))
        (when content
          (list :content content :language lang))))))

(defun ai-girlfriend--markdown-send-to-buffer ()
  "Send the code block at point to buffer.
Replace selection if any."
  (let ((buffer
         (completing-read "Choose buffer: " (mapcar #'buffer-name (buffer-list))
                          nil ; PREDICATE
                          t ; REQUIRE-MATCH
                          nil ; INITIAL-INPUT
                          'buffer-name-history (buffer-name (current-buffer))))
        (content (ai-girlfriend--get-markdown-block-content-at-point)))
    (when content
      (with-current-buffer buffer
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert (plist-get content :content))))))

(defun ai-girlfriend--markdown-copy ()
  "Copy the code block at point into kill ring."
  (let ((content (ai-girlfriend--get-markdown-block-content-at-point)))
    (when content
      (kill-new (plist-get content :content)))))

(defun ai-girlfriend--markdown-write (data)
  "Write DATA at the end of the chat part of the buffer."
  (ai-girlfriend--markdown-goto-input)
  (forward-line -3)
  (end-of-line)
  (insert data))

(defun ai-girlfriend--markdown-goto-input ()
  "Go to the input part of the chat buffer.
The input is created if not found."
  (goto-char (point-max))
  (if (re-search-backward ai-girlfriend--markdown-delimiter nil t)
      (forward-line 1)
    (insert "\n\n")
    (let ((start (point))
          (inhibit-read-only t))
      (insert ai-girlfriend--markdown-delimiter "\n\n")
      ;; Create overlay for read-only section
      (let ((overlay (make-overlay start (1- (point)))))
        (overlay-put overlay 'read-only t)
        (overlay-put overlay 'evaporate t)))))

(defun ai-girlfriend--markdown-get-buffer (instance)
  "Create `ai-girlfriend' buffers for INSTANCE."
  (unless (buffer-live-p (ai-girlfriend-chat-buffer instance))
    (setf (ai-girlfriend-chat-buffer instance)
          (get-buffer-create
           (ai-girlfriend--get-buffer-name (ai-girlfriend-directory instance))))
    (with-current-buffer (ai-girlfriend-chat-buffer instance)
      (ai-girlfriend-markdown-poly-mode)
      (ai-girlfriend--markdown-goto-input)
      (setq-local default-directory (ai-girlfriend-directory instance))))
  (ai-girlfriend-chat-buffer instance))


(defun ai-girlfriend--markdown-get-spinner-buffers (instance)
  "Get markdown spinner buffers for INSTANCE."
  (let ((buffer (ai-girlfriend--markdown-get-buffer instance)))
    (with-current-buffer buffer
      (list (pm-get-buffer-of-mode 'markdown-view-mode) buffer))))

(defun ai-girlfriend--markdown-insert-prompt (instance prompt)
  "Insert PROMPT in the chat buffer corresponding to INSTANCE."
  (with-current-buffer (ai-girlfriend--markdown-get-buffer instance)
    (ai-girlfriend--markdown-goto-input)
    (unless (eobp)
      (delete-region (point) (point-max)))
    (insert prompt)))

(defun ai-girlfriend--markdown-pop-prompt (instance)
  "Get current prompt to send and clean it.
INSTANCE is `ai-girlfriend' instance to use."
  (with-current-buffer (ai-girlfriend--markdown-get-buffer instance)
    (ai-girlfriend--markdown-goto-input)
    (let ((prompt (buffer-substring-no-properties (point) (point-max))))
      (delete-region (point) (point-max))
      prompt)))

(defun ai-girlfriend--markdown-init ()
  "Initialize the copilot chat markdown frontend."
  (setq ai-girlfriend-prompt ai-girlfriend-markdown-prompt))

;; Top-level execute code.

(cl-pushnew
 (make-ai-girlfriend-frontend
  :id 'markdown
  :init-fn #'ai-girlfriend--markdown-init
  :clean-fn nil
  :instance-init-fn nil
  :instance-clean-fn nil
  :save-fn nil
  :load-fn nil
  :format-fn #'ai-girlfriend--markdown-format-data
  :format-code-fn #'ai-girlfriend--markdown-format-code
  :format-buffer-fn #'ai-girlfriend--markdown-format-buffer
  :create-req-fn nil
  :send-to-buffer-fn #'ai-girlfriend--markdown-send-to-buffer
  :copy-fn #'ai-girlfriend--markdown-copy
  :yank-fn nil
  :write-fn #'ai-girlfriend--markdown-write
  :get-buffer-fn #'ai-girlfriend--markdown-get-buffer
  :insert-prompt-fn #'ai-girlfriend--markdown-insert-prompt
  :pop-prompt-fn #'ai-girlfriend--markdown-pop-prompt
  :goto-input-fn #'ai-girlfriend--markdown-goto-input
  :get-spinner-buffers-fn #'ai-girlfriend--markdown-get-spinner-buffers)
 ai-girlfriend--frontend-list
 :test #'equal)

(provide 'ai-girlfriend-markdown)
;;; ai-girlfriend-markdown.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
