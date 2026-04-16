;;; ai-girlfriend-chat --- ai-girlfriend-chat-spinner.el --- copilot chat spinner -*- lexical-binding: t; -*-

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

(require 'ai-girlfriend-chat-instance)
(require 'ai-girlfriend-chat-frontend)

(defcustom ai-girlfriend-chat-spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Frames used for the spinner animation during streaming."
  :type '(repeat string)
  :group 'ai-girlfriend-chat)

(defcustom ai-girlfriend-chat-spinner-interval 0.1
  "Interval in seconds between spinner frame updates."
  :type 'number
  :group 'ai-girlfriend-chat)

(defface ai-girlfriend-chat-spinner-face '((t :inherit font-lock-keyword-face))
  "Face used for the spinner during streaming."
  :group 'ai-girlfriend-chat)


(defun ai-girlfriend-chat--get-spinner-buffers (instance)
  "Get Spinner buffer from the active frontend.
Argument INSTANCE is the copilot chat instance to get the buffer for."
  (condition-case err
      (let ((get-buffer-fn
             (ai-girlfriend-chat-frontend-get-spinner-buffers-fn
              (ai-girlfriend-chat--get-frontend))))
        (when (and get-buffer-fn
                   (buffer-live-p (ai-girlfriend-chat-chat-buffer instance)))
          (funcall get-buffer-fn instance)))
    (error
     (when ai-girlfriend-chat-debug
       (message "Error getting spinner buffer: %S" err))
     nil)))

(defun ai-girlfriend-chat--spinner-start (instance)
  "Start the spinner animation in the Copilot Chat buffer.
Argument INSTANCE is the copilot chat instance to use."
  (when (ai-girlfriend-chat-spinner-timer instance)
    (cancel-timer (ai-girlfriend-chat-spinner-timer instance)))

  (setf
   (ai-girlfriend-chat-spinner-index instance) 0
   (ai-girlfriend-chat-spinner-status instance) "Thinking"
   (ai-girlfriend-chat-spinner-timer instance)
   (run-with-timer
    0 ai-girlfriend-chat-spinner-interval #'ai-girlfriend-chat--spinner-update
    instance)))

(defun ai-girlfriend-chat--spinner-update (instance)
  "Update the spinner animation in the Copilot Chat buffer.
Argument INSTANCE is the copilot chat instance to use."
  (let ((buffers (ai-girlfriend-chat--get-spinner-buffers instance)))
    (dolist (buffer buffers)
      (when (and buffer (buffer-live-p buffer))
        (let ((frame
               (nth
                (ai-girlfriend-chat-spinner-index instance)
                ai-girlfriend-chat-spinner-frames))
              (status-text
               (if (ai-girlfriend-chat-spinner-status instance)
                   (concat (ai-girlfriend-chat-spinner-status instance) " ")
                 "")))
          (with-current-buffer buffer
            (save-excursion
              ;; Remove existing spinner overlay if any
              (remove-overlays (point-min) (point-max) 'ai-girlfriend-chat-spinner t)
              ;; Create new spinner overlay at the end of buffer
              (goto-char (point-max))
              (let ((ov (make-overlay (point) (point))))
                (overlay-put ov 'ai-girlfriend-chat-spinner t)
                (overlay-put
                 ov 'after-string
                 (propertize (concat status-text frame)
                             'face
                             'ai-girlfriend-chat-spinner-face))))))

        ;; Update spinner index
        (setf (ai-girlfriend-chat-spinner-index instance)
              (% (1+ (ai-girlfriend-chat-spinner-index instance))
                 (length ai-girlfriend-chat-spinner-frames)))))))

(defun ai-girlfriend-chat--spinner-stop (instance)
  "Stop the spinner animation.
Argument INSTANCE is the copilot chat instance to use."
  (when (ai-girlfriend-chat-spinner-timer instance)
    (cancel-timer (ai-girlfriend-chat-spinner-timer instance))
    (setf (ai-girlfriend-chat-spinner-timer instance) nil))

  ;; Remove spinner overlay - with robust error handling
  (condition-case err
      (let ((buffers (ai-girlfriend-chat--get-spinner-buffers instance)))
        (dolist (buffer buffers)
          (when (and buffer (buffer-live-p buffer))
            (with-current-buffer buffer
              (remove-overlays
               (point-min) (point-max) 'ai-girlfriend-chat-spinner t)))))
    (error
     (when ai-girlfriend-chat-debug
       (message "Error stopping spinner: %S" err)))))

(defun ai-girlfriend-chat--spinner-set-status (instance status)
  "Set the status message to display with the spinner.
Argument INSTANCE is the copilot chat instance to use.
Argument STATUS is the status message to display."
  (setf (ai-girlfriend-chat-spinner-status instance) status)
  (when (ai-girlfriend-chat-spinner-timer instance)
    (ai-girlfriend-chat--spinner-update instance)))

(provide 'ai-girlfriend-chat-spinner)
;;; ai-girlfriend-chat-spinner.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
