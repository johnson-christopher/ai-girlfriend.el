;;; ai-girlfriend --- ai-girlfriend-backend.el --- define copilot backend interface -*- lexical-binding: t; -*-

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

;; Forward declaration of custom variables
(defvar ai-girlfriend-backend)

;; Struct
(cl-defstruct
    ai-girlfriend-backend
  "Struct for Copilot chat backend."
  id
  init-fn
  clean-fn
  login-fn
  renew-token-fn
  ask-fn
  cancel-fn
  quotas-fn)

(cl-declaim (type (list-of ai-girlfriend-backend) ai-girlfriend--backend-list))

(defvar ai-girlfriend--backend-list '()
  "ai-girlfriend backends and functions list.
Each element must be a `ai-girlfriend-backend' struct instance.
Elements are added in the module that defines each backend.")

(defun ai-girlfriend--get-backend ()
  "Get backend from custom."
  (cl-find
   ai-girlfriend-backend
   ai-girlfriend--backend-list
   :key #'ai-girlfriend-backend-id
   :test #'eq))


(provide 'ai-girlfriend-backend)
;;; ai-girlfriend-backend.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
