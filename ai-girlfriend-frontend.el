;;; ai-girlfriend --- ai-girlfriend-frontend.el --- define copilot frontend interface -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(defvar ai-girlfriend-frontend)

(cl-defstruct
    ai-girlfriend-frontend
  id
  init-fn
  clean-fn
  instance-init-fn
  instance-clean-fn
  save-fn
  load-fn
  format-fn
  format-code-fn
  format-buffer-fn
  create-req-fn
  send-to-buffer-fn
  copy-fn
  yank-fn
  write-fn
  get-buffer-fn
  insert-prompt-fn
  pop-prompt-fn
  goto-input-fn
  get-spinner-buffers-fn)

(defvar ai-girlfriend--frontend-list '()
  "ai-girlfriend frontends and functions list.
Each element must be a `ai-girlfriend-frontend' struct instance.
Elements are added in the module that defines each front end.")

(defvar ai-girlfriend--frontend-init-p nil
  "Flag to indicate if the frontend has been initialized.")

(cl-declaim (type (list-of ai-girlfriend-frontend) ai-girlfriend--frontend-list))

(defun ai-girlfriend--get-frontend ()
  "Get frontend from custom."
  (cl-find
   ai-girlfriend-frontend
   ai-girlfriend--frontend-list
   :key #'ai-girlfriend-frontend-id
   :test #'eq))

(defun ai-girlfriend--get-buffer (instance)
  "Get Copilot Chat buffer from the active frontend.
Argument INSTANCE is the copilot chat instance to get the buffer for."
  (let ((get-buffer-fn
         (ai-girlfriend-frontend-get-buffer-fn (ai-girlfriend--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn instance))))

(provide 'ai-girlfriend-frontend)
;;; ai-girlfriend-frontend.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
