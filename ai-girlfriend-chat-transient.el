;;; ai-girlfriend-chat --- ai-girlfriend-chat-transient.el  --- copilot chat transient functions -*- lexical-binding: t; -*-

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

(require 'transient)

(require 'ai-girlfriend-chat-command)
(require 'ai-girlfriend-chat-mcp)

;;;###autoload (autoload 'ai-girlfriend-chat-transient "ai-girlfriend-chat" nil t)
(transient-define-prefix
  ai-girlfriend-chat-transient () "Copilot chat command menu."
  [["Commands"
    ("d" "Display chat" ai-girlfriend-chat-display)
    ("h" "Hide chat" ai-girlfriend-chat-hide)
    ("x" "Reset" ai-girlfriend-chat-reset)
    ("g" "Go to buffer" ai-girlfriend-chat-switch-to-buffer)
    ("q" "Quit" transient-quit-one)]
   ["Instance"
    ("M" "Set model" ai-girlfriend-chat-set-model)
    ("C" "Set commit model" ai-girlfriend-chat-set-commit-model)
    ("S" "Save chat" ai-girlfriend-chat-save)
    ("L" "Load chat" ai-girlfriend-chat-load)
    ("k" "Kill instance" ai-girlfriend-chat-kill-instance)]
   ["Actions"
    ("p" "Custom prompt" ai-girlfriend-chat-custom-prompt-selection)
    ("i" "Ask and insert" ai-girlfriend-chat-ask-and-insert)
    ("m" "Insert commit message" ai-girlfriend-chat-insert-commit-message)]
   ["Data"
    ("y" "Yank last code block" ai-girlfriend-chat-yank)
    ("s" "Send code to buffer" ai-girlfriend-chat-send-to-buffer)]
   ["Tools"
    ("b" "Buffers" ai-girlfriend-chat-transient-buffers)
    ("c" "Code helpers" ai-girlfriend-chat-transient-code)]])

;;;###autoload (autoload 'ai-girlfriend-chat-transient-buffers "ai-girlfriend-chat" nil t)
(transient-define-prefix
  ai-girlfriend-chat-transient-buffers () "Copilot chat buffers menu."
  [["Buffers"
    ("a" "Add buffers" ai-girlfriend-chat-add-buffers)
    ("A"
     "Add all buffers in current frame"
     ai-girlfriend-chat-add-buffers-in-current-window)
    ("d" "Delete buffers" ai-girlfriend-chat-del-buffers)
    ("D" "Delete all buffers" ai-girlfriend-chat-list-clear-buffers)
    ("f" "Add files under current directory" ai-girlfriend-chat-add-files-under-dir)
    ("l" "Display buffer list" ai-girlfriend-chat-list)
    ("c" "Clear buffers" ai-girlfriend-chat-list-clear-buffers)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ai-girlfriend-chat-transient-code "ai-girlfriend-chat" nil t)
(transient-define-prefix
  ai-girlfriend-chat-transient-code () "Copilot chat code helpers menu."
  [["Code helpers"
    ("e" "Explain" ai-girlfriend-chat-explain)
    ("E" "Explain symbol" ai-girlfriend-chat-explain-symbol-at-line)
    ("r" "Review" ai-girlfriend-chat-review)
    ("d" "Doc" ai-girlfriend-chat-doc)
    ("f" "Fix" ai-girlfriend-chat-fix)
    ("o" "Optimize" ai-girlfriend-chat-optimize)
    ("t" "Test" ai-girlfriend-chat-test)
    ("F" "Explain function" ai-girlfriend-chat-explain-defun)
    ("c" "Custom prompt function" ai-girlfriend-chat-custom-prompt-function)
    ("R" "Review whole buffer" ai-girlfriend-chat-review-whole-buffer)
    ("q" "Quit" transient-quit-one)]])


(defun ai-girlfriend-chat--index-to-key (index)
  "Convert INDEX to a string key for transient suffixes."
  (cond
   ((< index 10)
    (format "%d" index))
   ((< index 36)
    (char-to-string (+ ?a (- index 10))))
   ((< index 62)
    (char-to-string (+ ?A (- index 10))))
   (t
    (error "Index %d is out of range for transient suffixes" index))))

(defun ai-girlfriend-chat--mcp-generate-server-suffixes ()
  "Generate dynamic switches for servers."
  (let ((suffixes '())
        (index 0)
        (instance (ai-girlfriend-chat--current-instance)))
    ;; Add each server as switch
    (dolist (server (mapcar 'car mcp-hub-servers))
      (push (list
             (ai-girlfriend-chat--index-to-key index)
             (format "Add %s" server)
             (format "%s" server)
             :init-value
             (lambda (obj)
               (when (member
                      (slot-value obj 'argument)
                      (ai-girlfriend-chat-mcp-servers instance))
                 (setf (slot-value obj 'value) (slot-value obj 'argument)))))
            suffixes)
      (setq index (1+ index)))

    (push (list (ai-girlfriend-chat--index-to-key index) "Add All" "ALL") suffixes)
    (push (list (ai-girlfriend-chat--index-to-key (1+ index)) "Clear All" "CLEAR")
          suffixes)

    (nreverse suffixes)))

(defun ai-girlfriend-chat--mcp-handle-selection (servers)
  "Handle selected SERVERS from arguments."
  (interactive (list (transient-args 'ai-girlfriend-chat-mcp-servers-transient)))
  (let ((instance (ai-girlfriend-chat--current-instance)))
    (cond
     ((member "ALL" servers)
      (setf (ai-girlfriend-chat-mcp-servers instance) (mapcar 'car mcp-hub-servers)))
     ((member "CLEAR" servers)
      (setf (ai-girlfriend-chat-mcp-servers instance) nil))
     (t
      (setf (ai-girlfriend-chat-mcp-servers instance) servers)))
    (ai-girlfriend-chat--activate-mcp-servers instance)))

;;;###autoload (autoload 'ai-girlfriend-chat-mcp-servers-transient "ai-girlfriend-chat" nil t)
(transient-define-prefix
  ai-girlfriend-chat-mcp-servers-transient () "Copilot chat MCP servers menu."
  ["MCP servers:"
   :class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      transient--prefix (ai-girlfriend-chat--mcp-generate-server-suffixes)))]
  [["Actions"
    ("RET" "Validate" ai-girlfriend-chat--mcp-handle-selection)
    ("q" "Cancel" transient-quit-one)]])

;;;###autoload (autoload 'ai-girlfriend-chat-set-mcp-servers "ai-girlfriend-chat" nil t)
(defalias 'ai-girlfriend-chat-set-mcp-servers 'ai-girlfriend-chat-mcp-servers-transient)

(provide 'ai-girlfriend-chat-transient)
;;; ai-girlfriend-chat-transient.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
