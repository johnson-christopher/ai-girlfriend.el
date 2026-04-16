;;; ai-girlfriend --- ai-girlfriend-transient.el  --- copilot chat transient functions -*- lexical-binding: t; -*-

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

(require 'transient)

(require 'ai-girlfriend-command)
(require 'ai-girlfriend-mcp)

;;;###autoload (autoload 'ai-girlfriend-transient "ai-girlfriend" nil t)
(transient-define-prefix
  ai-girlfriend-transient () "Copilot chat command menu."
  [["Commands"
    ("d" "Display chat" ai-girlfriend-display)
    ("h" "Hide chat" ai-girlfriend-hide)
    ("x" "Reset" ai-girlfriend-reset)
    ("g" "Go to buffer" ai-girlfriend-switch-to-buffer)
    ("q" "Quit" transient-quit-one)]
   ["Instance"
    ("M" "Set model" ai-girlfriend-set-model)
    ("C" "Set commit model" ai-girlfriend-set-commit-model)
    ("S" "Save chat" ai-girlfriend-save)
    ("L" "Load chat" ai-girlfriend-load)
    ("k" "Kill instance" ai-girlfriend-kill-instance)]
   ["Actions"
    ("p" "Custom prompt" ai-girlfriend-custom-prompt-selection)
    ("i" "Ask and insert" ai-girlfriend-ask-and-insert)
    ("m" "Insert commit message" ai-girlfriend-insert-commit-message)]
   ["Data"
    ("y" "Yank last code block" ai-girlfriend-yank)
    ("s" "Send code to buffer" ai-girlfriend-send-to-buffer)]
   ["Tools"
    ("b" "Buffers" ai-girlfriend-transient-buffers)
    ("c" "Code helpers" ai-girlfriend-transient-code)]])

;;;###autoload (autoload 'ai-girlfriend-transient-buffers "ai-girlfriend" nil t)
(transient-define-prefix
  ai-girlfriend-transient-buffers () "Copilot chat buffers menu."
  [["Buffers"
    ("a" "Add buffers" ai-girlfriend-add-buffers)
    ("A"
     "Add all buffers in current frame"
     ai-girlfriend-add-buffers-in-current-window)
    ("d" "Delete buffers" ai-girlfriend-del-buffers)
    ("D" "Delete all buffers" ai-girlfriend-list-clear-buffers)
    ("f" "Add files under current directory" ai-girlfriend-add-files-under-dir)
    ("l" "Display buffer list" ai-girlfriend-list)
    ("c" "Clear buffers" ai-girlfriend-list-clear-buffers)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'ai-girlfriend-transient-code "ai-girlfriend" nil t)
(transient-define-prefix
  ai-girlfriend-transient-code () "Copilot chat code helpers menu."
  [["Code helpers"
    ("e" "Explain" ai-girlfriend-explain)
    ("E" "Explain symbol" ai-girlfriend-explain-symbol-at-line)
    ("r" "Review" ai-girlfriend-review)
    ("d" "Doc" ai-girlfriend-doc)
    ("f" "Fix" ai-girlfriend-fix)
    ("o" "Optimize" ai-girlfriend-optimize)
    ("t" "Test" ai-girlfriend-test)
    ("F" "Explain function" ai-girlfriend-explain-defun)
    ("c" "Custom prompt function" ai-girlfriend-custom-prompt-function)
    ("R" "Review whole buffer" ai-girlfriend-review-whole-buffer)
    ("q" "Quit" transient-quit-one)]])


(defun ai-girlfriend--index-to-key (index)
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

(defun ai-girlfriend--mcp-generate-server-suffixes ()
  "Generate dynamic switches for servers."
  (let ((suffixes '())
        (index 0)
        (instance (ai-girlfriend--current-instance)))
    ;; Add each server as switch
    (dolist (server (mapcar 'car mcp-hub-servers))
      (push (list
             (ai-girlfriend--index-to-key index)
             (format "Add %s" server)
             (format "%s" server)
             :init-value
             (lambda (obj)
               (when (member
                      (slot-value obj 'argument)
                      (ai-girlfriend-mcp-servers instance))
                 (setf (slot-value obj 'value) (slot-value obj 'argument)))))
            suffixes)
      (setq index (1+ index)))

    (push (list (ai-girlfriend--index-to-key index) "Add All" "ALL") suffixes)
    (push (list (ai-girlfriend--index-to-key (1+ index)) "Clear All" "CLEAR")
          suffixes)

    (nreverse suffixes)))

(defun ai-girlfriend--mcp-handle-selection (servers)
  "Handle selected SERVERS from arguments."
  (interactive (list (transient-args 'ai-girlfriend-mcp-servers-transient)))
  (let ((instance (ai-girlfriend--current-instance)))
    (cond
     ((member "ALL" servers)
      (setf (ai-girlfriend-mcp-servers instance) (mapcar 'car mcp-hub-servers)))
     ((member "CLEAR" servers)
      (setf (ai-girlfriend-mcp-servers instance) nil))
     (t
      (setf (ai-girlfriend-mcp-servers instance) servers)))
    (ai-girlfriend--activate-mcp-servers instance)))

;;;###autoload (autoload 'ai-girlfriend-mcp-servers-transient "ai-girlfriend" nil t)
(transient-define-prefix
  ai-girlfriend-mcp-servers-transient () "Copilot chat MCP servers menu."
  ["MCP servers:"
   :class transient-column
   :setup-children
   (lambda (_)
     (transient-parse-suffixes
      transient--prefix (ai-girlfriend--mcp-generate-server-suffixes)))]
  [["Actions"
    ("RET" "Validate" ai-girlfriend--mcp-handle-selection)
    ("q" "Cancel" transient-quit-one)]])

;;;###autoload (autoload 'ai-girlfriend-set-mcp-servers "ai-girlfriend" nil t)
(defalias 'ai-girlfriend-set-mcp-servers 'ai-girlfriend-mcp-servers-transient)

(provide 'ai-girlfriend-transient)
;;; ai-girlfriend-transient.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
