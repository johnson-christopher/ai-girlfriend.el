;;; ai-girlfriend-chat --- ai-girlfriend-chat-common.el --- copilot chat variables and const -*- lexical-binding: t; -*-

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
;; The shared variables and constants

;;; Code:

(require 'cl-lib)

;; constants
(defconst ai-girlfriend-chat--magic "#cc#done#!$")
(defconst ai-girlfriend-chat--buffer-name "*Copilot Chat*"
  "Name of the Copilot Chat buffer.")

;; customs
(defgroup ai-girlfriend-chat nil
  "GitHub Copilot chat."
  :group 'tools)

(defcustom ai-girlfriend-chat-follow nil
  "Follow the chat buffer."
  :type 'boolean
  :group 'ai-girlfriend-chat)

(defcustom ai-girlfriend-chat-github-token-file "~/.config/github-copilot/apps.json"
  "The file where to find GitHub token."
  :type 'string
  :group 'ai-girlfriend-chat)

(defcustom ai-girlfriend-chat-token-cache "~/.cache/ai-girlfriend-chat/token"
  "The file where the GitHub token is cached."
  :type 'string
  :group 'ai-girlfriend-chat)

;; Functions
(defun ai-girlfriend-chat--uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536)
          (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 4096) 32768)
          (random 65536)
          (random 65536)
          (random 65536)))

(defun ai-girlfriend-chat--machine-id ()
  "Generate a machine ID."
  (let ((hex-chars "0123456789abcdef")
        (length 65)
        (hex ""))
    (dotimes (_ length)
      (setq hex (concat hex (string (aref hex-chars (random 16))))))
    hex))

(defun ai-girlfriend-chat--get-buffer-name (directory)
  "Get the corresponding chat buffer name for DIRECTORY."
  (format "*Copilot Chat [%s]*" directory))

(provide 'ai-girlfriend-chat-common)
;;; ai-girlfriend-chat-common.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
