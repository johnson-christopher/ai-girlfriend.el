;;; ai-girlfriend-chat --- ai-girlfriend-chat-shell-maker.el --- copilot chat interface, shell-maker frontend -*- lexical-binding: t; -*-

;; Copyright (C) 2024  ai-girlfriend-chat maintainers

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

(require 'shell-maker)

(require 'ai-girlfriend-chat-command)
(require 'ai-girlfriend-chat-copilot)
(require 'ai-girlfriend-chat-markdown)

;; Constants
(defconst ai-girlfriend-chat--shell-maker-temp-buffer-prefix
  "*ai-girlfriend-chat-shell-maker-temp "
  "Temporary buffer prefix for Copilot Chat shell-maker.")

;; Structures
(cl-defstruct
    (ai-girlfriend-chat-shell-maker
     (:constructor ai-girlfriend-chat-shell-maker--make) (:copier nil))
  "Struct for Copilot Chat shell-maker frontend."
  (tmp-buf nil :type (or null buffer)) ; Temporary buffer for shell-maker
  (answer-point 0 :type int) ; Point in the temporary buffer for the answer
  (cb-fn nil :type function) ; Callback function for shell-maker output
  (history nil :type list)) ; History of shell-maker commands

;; Functions
(defun ai-girlfriend-chat--shell-maker-prompt-send ()
  "Function to send the prompt content."
  (let ((instance (ai-girlfriend-chat--current-instance)))
    (with-current-buffer (ai-girlfriend-chat--shell-maker-get-buffer instance)
      (shell-maker-submit)
      (display-buffer (current-buffer)))))

(defun ai-girlfriend-chat--shell-maker-get-buffer-name (directory)
  "Get the corresponding shell-maker buffer name for DIRECTORY."
  (format "Ai-Girlfriend-Chat%s" directory))

(defun ai-girlfriend-chat--shell-maker-temp-buffer-name (instance)
  "Return the temporary buffer name for the Copilot Chat shell-maker.
INSTANCE is used to get directory"
  (concat
   ai-girlfriend-chat--shell-maker-temp-buffer-prefix
   (ai-girlfriend-chat--shell-maker-get-buffer-name (ai-girlfriend-chat-directory instance))
   "*"))

(defun ai-girlfriend-chat--shell-maker-tmp-buf (instance)
  "Get or create the temporary buffer for syntax highlighting for INSTANCE."
  (let* ((private (ai-girlfriend-chat--frontend instance))
         (tempb (ai-girlfriend-chat-shell-maker-tmp-buf private)))
    (unless (buffer-live-p tempb)
      (setq tempb
            (get-buffer-create
             (ai-girlfriend-chat--shell-maker-temp-buffer-name instance)))
      (setf (ai-girlfriend-chat-shell-maker-tmp-buf private) tempb))
    tempb))

(defun ai-girlfriend-chat--shell-maker-get-buffer (instance)
  "Create or retrieve the Copilot Chat shell-maker buffer for INSTANCE."
  (unless (buffer-live-p (ai-girlfriend-chat-chat-buffer instance))
    (setf (ai-girlfriend-chat-chat-buffer instance) (ai-girlfriend-chat--shell instance)))
  (let ((tempb (ai-girlfriend-chat--shell-maker-tmp-buf instance)))
    (with-current-buffer tempb
      (let ((inhibit-read-only t))
        (markdown-view-mode)))
    (ai-girlfriend-chat-chat-buffer instance)))

(defun ai-girlfriend-chat--shell-maker-font-lock-faces (instance)
  "Replace faces by font-lock-faces in INSTANCE buffer."
  (with-current-buffer (ai-girlfriend-chat--shell-maker-tmp-buf instance)
    (let ((inhibit-read-only t))
      (font-lock-ensure)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((next-change
               (or (next-property-change (point) nil (point-max)) (point-max)))
              (face (get-text-property (point) 'face)))
          (when face
            (font-lock-append-text-property
             (point) next-change 'font-lock-face face))
          (goto-char next-change))))))

(defun ai-girlfriend-chat--shell-maker-copy-faces (instance)
  "Apply faces to the copilot chat buffer corresponding to INSTANCE."
  (with-current-buffer (ai-girlfriend-chat--shell-maker-tmp-buf instance)
    (save-restriction
      (widen)
      (font-lock-ensure)
      (ai-girlfriend-chat--shell-maker-font-lock-faces instance)
      (let ((content (buffer-substring (point-min) (point-max))))
        (with-current-buffer (ai-girlfriend-chat--shell-maker-get-buffer instance)
          (goto-char
           (1+ (ai-girlfriend-chat-shell-maker-answer-point
                (ai-girlfriend-chat--frontend instance))))
          (insert content)
          (delete-region (point) (+ (point) (1- (length content))))
          (goto-char (point-max)))))))

(defun ai-girlfriend-chat--shell-cb-prompt (instance shell content)
  "Callback for Copilot Chat `shell-maker'.
Argument INSTANCE is `ai-girlfriend-chat' instance.
Argument SHELL is the `shell-maker' instance.
Argument CONTENT is copilot chat answer."
  (with-current-buffer (ai-girlfriend-chat--shell-maker-get-buffer instance)
    (goto-char (point-max))
    (when (ai-girlfriend-chat-first-word-answer instance)
      (setf (ai-girlfriend-chat-first-word-answer instance) nil)
      (let ((str
             (concat
              (format-time-string "# [%T] ")
              (format "Copilot(%s):\n" (ai-girlfriend-chat-model instance))))
            (inhibit-read-only t))
        (with-current-buffer (ai-girlfriend-chat-shell-maker-tmp-buf
                              (ai-girlfriend-chat--frontend instance))
          (insert str))
        (funcall (map-elt shell :write-output) str)))
    (if (string= content ai-girlfriend-chat--magic)
        (progn
          (funcall (map-elt shell :finish-output) t) ; the end
          (ai-girlfriend-chat--shell-maker-copy-faces instance)
          (setf (ai-girlfriend-chat-first-word-answer instance) t))
      (progn
        (with-current-buffer (ai-girlfriend-chat--shell-maker-tmp-buf instance)
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert content)))
        (funcall (map-elt shell :write-output) content)))))

(defun ai-girlfriend-chat--shell-cb-prompt-wrapper (shell instance content)
  "Wrapper around `ai-girlfriend-chat--shell-cb-prompt'.
Argument SHELL is the `shell-maker' instance.
Argument INSTANCE is `ai-girlfriend-chat' instance.
Argument CONTENT is copilot chat answer."
  (if ai-girlfriend-chat-follow
      (ai-girlfriend-chat--shell-cb-prompt instance shell content)
    (save-excursion (ai-girlfriend-chat--shell-cb-prompt instance shell content))))

(defun ai-girlfriend-chat--shell-cb (instance command shell)
  "Callback for Copilot Chat `shell-maker'.
Argument INSTANCE is `ai-girlfriend-chat' instance.
Argument COMMAND is the command to send to Copilot.
Argument SHELL is the `shell-maker' instance."
  (setf
   (ai-girlfriend-chat-shell-maker-cb-fn (ai-girlfriend-chat--frontend instance))
   (apply-partially #'ai-girlfriend-chat--shell-cb-prompt-wrapper shell)
   (ai-girlfriend-chat-shell-maker-answer-point (ai-girlfriend-chat--frontend instance)) (point))
  (let ((inhibit-read-only t))
    (with-current-buffer (ai-girlfriend-chat--shell-maker-tmp-buf instance)
      (erase-buffer)))
  (ai-girlfriend-chat--ask
   instance
   command
   (ai-girlfriend-chat-shell-maker-cb-fn (ai-girlfriend-chat--frontend instance))))

(defun ai-girlfriend-chat--shell (instance)
  "Start a Copilot Chat shell for INSTANCE."
  (let ((buf
         (shell-maker-start
          (make-shell-maker-config
           :name
           (ai-girlfriend-chat--shell-maker-get-buffer-name
            (ai-girlfriend-chat-directory instance))
           :execute-command
           (lambda (command shell)
             (ai-girlfriend-chat--shell-cb instance command shell)))
          t
          nil
          t
          (ai-girlfriend-chat--get-buffer-name (ai-girlfriend-chat-directory instance)))))
    (with-current-buffer buf
      (setq-local default-directory (ai-girlfriend-chat-directory instance))
      (local-set-key [remap comint-send-input] #'ai-girlfriend-chat-prompt-send)
      (local-set-key [remap shell-maker-submit] #'ai-girlfriend-chat-prompt-send))
    buf))

(defun ai-girlfriend-chat--shell-maker-insert-prompt (instance prompt)
  "Insert PROMPT in the chat buffer corresponding to INSTANCE."
  (with-current-buffer (ai-girlfriend-chat--shell-maker-get-buffer instance)
    (goto-char (point-max))
    (insert prompt)))

(defun ai-girlfriend-chat--shell-maker-clean ()
  "Clean the copilot chat `shell-maker' frontend."
  (advice-remove
   'ai-girlfriend-chat-prompt-send #'ai-girlfriend-chat--shell-maker-prompt-send))

(defun ai-girlfriend-chat-shell-maker-init ()
  "Initialize the copilot chat `shell-maker' frontend."
  (setq ai-girlfriend-chat-prompt ai-girlfriend-chat-markdown-prompt)
  (advice-add
   'ai-girlfriend-chat-prompt-send
   :override #'ai-girlfriend-chat--shell-maker-prompt-send))

(defun ai-girlfriend-chat--shell-maker-get-spinner-buffers (instance)
  "Get the spinner buffers for the copilot chat `shell-maker' frontend.
INSTANCE is the copilot chat instance."
  (list (ai-girlfriend-chat--shell-maker-get-buffer instance)))

(defun ai-girlfriend-chat--shell-maker-init-instance (instance)
  "Initialize the copilot chat `shell-maker' INSTANCE."
  (setf (ai-girlfriend-chat--frontend instance) (ai-girlfriend-chat-shell-maker--make)))

(defun ai-girlfriend-chat--shell-maker-clean-instance (instance)
  "Initialize the copilot chat `shell-maker' INSTANCE."
  (let ((tmp-buf
         (ai-girlfriend-chat-shell-maker-tmp-buf (ai-girlfriend-chat--frontend instance))))
    (when (buffer-live-p tmp-buf)
      (kill-buffer tmp-buf)))
  (setf (ai-girlfriend-chat--frontend instance) nil))

(defun ai-girlfriend-chat--shell-maker-save (instance)
  "Save shell-maker history of INSTANCE."
  (setf
   (ai-girlfriend-chat-shell-maker-tmp-buf (ai-girlfriend-chat--frontend instance)) nil
   (ai-girlfriend-chat-shell-maker-cb-fn (ai-girlfriend-chat--frontend instance)) nil)
  (with-current-buffer (ai-girlfriend-chat-chat-buffer instance)
    (setf (ai-girlfriend-chat-shell-maker-history (ai-girlfriend-chat--frontend instance))
          (shell-maker--extract-history
           (buffer-string) (shell-maker-prompt-regexp shell-maker--config)))))

(defun ai-girlfriend-chat--shell-maker-load (instance)
  "Load shell-maker history of INSTANCE."
  (let ((history
         (ai-girlfriend-chat-shell-maker-history (ai-girlfriend-chat--frontend instance)))
        (buf (ai-girlfriend-chat-chat-buffer instance)))
    (when (and history (buffer-live-p buf))
      (with-current-buffer buf
        (shell-maker-restore-session-from-transcript history)))))

;; Top-level execute code.

(cl-pushnew
 (make-ai-girlfriend-chat-frontend
  :id 'shell-maker
  :init-fn #'ai-girlfriend-chat-shell-maker-init
  :clean-fn #'ai-girlfriend-chat--shell-maker-clean
  :instance-init-fn #'ai-girlfriend-chat--shell-maker-init-instance
  :instance-clean-fn #'ai-girlfriend-chat--shell-maker-clean-instance
  :save-fn #'ai-girlfriend-chat--shell-maker-save
  :load-fn #'ai-girlfriend-chat--shell-maker-load
  :format-fn nil
  :format-code-fn #'ai-girlfriend-chat--markdown-format-code
  :format-buffer-fn #'ai-girlfriend-chat--markdown-format-buffer
  :create-req-fn nil
  :send-to-buffer-fn nil
  :copy-fn nil
  :yank-fn nil
  :write-fn nil
  :get-buffer-fn #'ai-girlfriend-chat--shell-maker-get-buffer
  :insert-prompt-fn #'ai-girlfriend-chat--shell-maker-insert-prompt
  :pop-prompt-fn nil
  :goto-input-fn #'nil
  :get-spinner-buffers-fn #'ai-girlfriend-chat--shell-maker-get-spinner-buffers)
 ai-girlfriend-chat--frontend-list
 :test #'equal)

(provide 'ai-girlfriend-chat-shell-maker)
;;; ai-girlfriend-chat-shell-maker.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
