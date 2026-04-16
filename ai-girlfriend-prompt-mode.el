;;; ai-girlfriend --- ai-girlfriend-prompt-mode.el --- copilot chat prompt mode -*- lexical-binding: t; -*-

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

(require 'ai-girlfriend-common)
(require 'ai-girlfriend-spinner)

(defvar ai-girlfriend-prompt-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c RET") 'ai-girlfriend-prompt-send)
    (define-key map (kbd "C-c C-c") 'ai-girlfriend-prompt-send)
    (define-key
     map (kbd "C-c C-q")
     (lambda ()
       (interactive)
       (bury-buffer)
       (delete-window)))
    (define-key map (kbd "C-c C-l") 'ai-girlfriend-prompt-split-and-list)
    (define-key map (kbd "C-c C-t") 'ai-girlfriend-transient)
    (define-key map (kbd "M-p") 'ai-girlfriend-prompt-history-previous)
    (define-key map (kbd "M-n") 'ai-girlfriend-prompt-history-next)
    map)
  "Keymap for Copilot Chat Prompt mode.")
(defvar ai-girlfriend--prompt-history nil
  "ai-girlfriend prompt history.")

(define-minor-mode ai-girlfriend-prompt-mode
  "Minor mode for the Copilot Chat Prompt region."
  :init-value nil
  :lighter " Copilot Chat Prompt"
  :keymap ai-girlfriend-prompt-mode-map)

(defun ai-girlfriend--write-buffer (instance data save &optional buffer)
  "Write content to the Copilot Chat BUFFER.
Argument INSTANCE is the copilot chat instance to use.
Argument DATA data to be inserted in buffer.
If argument SAVE is t and BUFFER nil, `save-excursion' is used.
Optional argument BUFFER is the buffer to write to,
defaults to instance's chat buffer."
  (if buffer
      (with-current-buffer buffer
        (insert data))
    (with-current-buffer (ai-girlfriend--get-buffer instance)
      (let ((write-fn
             (ai-girlfriend-frontend-write-fn (ai-girlfriend--get-frontend))))
        (when write-fn
          (if save
              (save-excursion (funcall write-fn data))
            (funcall write-fn data)))))))

(defun ai-girlfriend--format-data (instance content type)
  "Format the CONTENT according to the frontend.
Argument INSTANCE is the copilot chat instance to use.
Argument CONTENT is the data to format.
Argument TYPE is the type of data to format: `answer` or `prompt`."
  (let ((format-fn
         (ai-girlfriend-frontend-format-fn (ai-girlfriend--get-frontend))))
    (if format-fn
        (funcall format-fn instance content type)
      content)))

(defun ai-girlfriend-prompt-cb (instance content &optional buffer)
  "Function called by backend when data is received.
Argument INSTANCE is the copilot chat instance to use.
Argument CONTENT is data received from backend.
Optional argument BUFFER is the buffer to write data in."
  (if (string= content ai-girlfriend--magic)
      (progn
        (when (boundp 'ai-girlfriend--spinner-timer)
          (ai-girlfriend--spinner-stop instance))
        (ai-girlfriend--write-buffer instance
                                     (ai-girlfriend--format-data
                                      instance "\n\n" 'answer)
                                     (not ai-girlfriend-follow)
                                     buffer))
    (ai-girlfriend--write-buffer instance
                                 (ai-girlfriend--format-data
                                  instance content 'answer)
                                 (not ai-girlfriend-follow)
                                 buffer)))

(defun ai-girlfriend--pop-current-prompt (instance)
  "Get current prompt to send and clean it.
Argument INSTANCE is the copilot chat instance to use."
  (let ((pop-prompt-fn
         (ai-girlfriend-frontend-pop-prompt-fn (ai-girlfriend--get-frontend))))
    (when pop-prompt-fn
      (funcall pop-prompt-fn instance))))

(provide 'ai-girlfriend-prompt-mode)
;;; ai-girlfriend-prompt-mode.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
