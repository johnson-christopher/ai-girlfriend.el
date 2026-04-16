;;; ai-girlfriend --- ai-girlfriend-curl.el --- copilot chat curl backend -*- lexical-binding: t; -*-

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
;; This is curl backend for ai-girlfriend code

;;; Code:

(require 'ai-girlfriend-body)
(require 'ai-girlfriend-common)
(require 'ai-girlfriend-connection)
(require 'ai-girlfriend-spinner)
(require 'ai-girlfriend-backend)
(require 'ai-girlfriend-mcp)
(require 'ai-girlfriend-responses)
(require 'ai-girlfriend-completions)

;; customs
(defcustom ai-girlfriend-curl-program "curl"
  "Curl program to use if `ai-girlfriend-use-curl' is set."
  :type 'string
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-curl-proxy nil
  "Curl will use this proxy if defined.
The proxy string can be specified with a protocol:// prefix.  No protocol
specified or http:// it is treated as an HTTP proxy.  Use socks4://,
socks4a://, socks5:// or socks5h:// to request a specific SOCKS version
to be used.

Unix domain sockets are supported for socks proxy.  Set localhost for the
host part.  e.g. socks5h://localhost/path/to/socket.sock

HTTPS proxy support works set with the https:// protocol prefix for
OpenSSL and GnuTLS.  It also works for BearSSL, mbedTLS, rustls,
Schannel, Secure Transport and wolfSSL (added in 7.87.0).

Unrecognized and unsupported proxy protocols cause an error.  Ancient
curl versions ignored unknown schemes and used http:// instead.

If the port number is not specified in the proxy string, it is assumed
to be 1080.

This option overrides existing environment variables that set the proxy
to use.  If there is an environment variable setting a proxy, you can set
proxy to \"\" to override it.

User and password that might be provided in the proxy string are URL
decoded by curl. This allows you to pass in special characters such as @
by using %40 or pass in a colon with %3a."
  :type 'string
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-curl-proxy-insecure nil
  "Insecure flag for `ai-girlfriend' proxy with curl backend.
Every secure connection curl makes is verified to be secure before the
transfer takes place.  This option makes curl skip the verification step
with a proxy and proceed without checking."
  :type 'boolean
  :group 'ai-girlfriend)

(defcustom ai-girlfriend-curl-proxy-user-pass nil
  "User password for `ai-girlfriend' proxy with curl backend.
Specify the username and password <user:password> to use for proxy
authentication."
  :type 'boolean
  :group 'ai-girlfriend)

;; structures
(cl-defstruct
    ai-girlfriend-curl
  "Private data for Copilot chat curl backend."
  (file nil :type (or null file))
  (process nil :type (or null process))
  (responses (make-ai-girlfriend-responses) :type ai-girlfriend-responses)
  (completions (make-ai-girlfriend-completions) :type ai-girlfriend-completions))


;; functions
(defun ai-girlfriend--curl-call-process (address method data &rest args)
  "Call curl synchronously.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Arguments ARGS are additional arguments to pass to curl."
  (let ((curl-args
         (append
          (list
           address
           "-s"
           "-X"
           (if (eq method 'post)
               "POST"
             "GET")
           "-A"
           "user-agent: CopilotChat.nvim/2.0.0"
           "-H"
           "content-type: application/json"
           "-H"
           "accept: application/json"
           "-H"
           "editor-plugin-version: CopilotChat.nvim/2.0.0"
           "-H"
           "editor-version: Neovim/0.10.0")
          (when data
            (list "-d" data))
          (when ai-girlfriend-curl-proxy
            (list "-x" ai-girlfriend-curl-proxy))
          (when ai-girlfriend-curl-proxy-insecure
            (list "--proxy-insecure"))
          (when ai-girlfriend-curl-proxy-user-pass
            (list "-U" ai-girlfriend-curl-proxy-user-pass))
          args)))
    (let ((result
           (apply #'call-process
                  ai-girlfriend-curl-program
                  nil
                  t
                  nil
                  curl-args)))
      (when (/= result 0)
        (error (format "curl returned non-zero result: %d" result))))))

(defun ai-girlfriend--curl-make-process
    (instance address method data filter vision callback &rest args)
  "Call curl asynchronously for INSTANCE.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Argument FILTER is the function called to parse data.
If VISION is t, add vision header.
Argument CALLBACK is the function to call with analysed data.
Optional argument ARGS are additional arguments to pass to curl."
  (let ((command
         (append
          (list
           ai-girlfriend-curl-program
           address
           "-s"
           "-X"
           (if (eq method 'post)
               "POST"
             "GET")
           "-A"
           "user-agent: CopilotChat.nvim/2.0.0"
           "-H"
           "content-type: application/json"
           "-H"
           "accept: application/json"
           "-H"
           "editor-plugin-version: CopilotChat.nvim/2.0.0"
           "-H"
           "editor-version: Neovim/0.10.0"
           "-H"
           "copilot-integration-id: vscode-chat")
          (when vision
            (list "-H" "Copilot-Vision-Request: true"))
          (when data
            (list "-d" data))
          (when ai-girlfriend-curl-proxy
            (list "-x" ai-girlfriend-curl-proxy))
          (when ai-girlfriend-curl-proxy-insecure
            (list "--proxy-insecure"))
          (when ai-girlfriend-curl-proxy-user-pass
            (list "-U" ai-girlfriend-curl-proxy-user-pass))
          args)))
    (setf (ai-girlfriend-curl-process (ai-girlfriend--backend instance))
          (make-process
           :name "ai-girlfriend-curl"
           :buffer nil
           :filter filter
           :sentinel
           (lambda (proc _exit)
             (when (/= (process-exit-status proc) 0)
               (let ((error-msg
                      (format "Curl interrupted: %d"
                              (process-exit-status proc))))
                 (funcall callback instance error-msg)
                 (funcall callback instance ai-girlfriend--magic)))
             (setf (ai-girlfriend-curl-process (ai-girlfriend--backend instance))
                   nil)
             (ai-girlfriend--spinner-stop instance))
           :stderr (get-buffer-create "*ai-girlfriend-curl-stderr*")
           :command command))))

(defun ai-girlfriend--curl-parse-github-token ()
  "Curl github token request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer :false-object :json-false))
         (token (gethash "access_token" json-data)))
    (setf (ai-girlfriend-connection-github-token ai-girlfriend--connection) token)
    (ai-girlfriend--write-cached-token token)))

(defun ai-girlfriend--curl-parse-login ()
  "Curl login request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer :false-object :json-false))
         (device-code (gethash "device_code" json-data))
         (user-code (gethash "user_code" json-data))
         (verification-uri (gethash "verification_uri" json-data)))
    (gui-set-selection 'CLIPBOARD user-code)
    (message
     (format
      "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
      user-code verification-uri))
    (read-from-minibuffer
     (format
      "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
      user-code verification-uri))
    (browse-url verification-uri)
    (read-from-minibuffer "Press ENTER after authorizing.")
    (with-temp-buffer
      (ai-girlfriend--curl-call-process
       "https://github.com/login/oauth/access_token" 'post
       (format
        "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}"
        device-code))
      (ai-girlfriend--curl-parse-github-token))))


(defun ai-girlfriend--curl-login ()
  "Manage github login."
  (with-temp-buffer
    (ai-girlfriend--curl-call-process
     "https://github.com/login/device/code"
     'post
     "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}")
    (ai-girlfriend--curl-parse-login)))


(defun ai-girlfriend--curl-parse-renew-token ()
  "Curl renew token request parsing."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let ((json-data
         (json-parse-buffer
          :object-type 'alist ;need alist to be compatible with
                                        ;ai-girlfriend-token format
          :false-object
          :json-false))
        (cache-dir
         (file-name-directory (expand-file-name ai-girlfriend-token-cache))))
    (setf (ai-girlfriend-connection-token ai-girlfriend--connection) json-data)
    ;; save token in ai-girlfriend-token-cache file after creating
    ;; folders if needed
    (when (not (file-directory-p cache-dir))
      (make-directory cache-dir t))
    (with-temp-file ai-girlfriend-token-cache
      (insert (json-serialize json-data :false-object :json-false)))))


(defun ai-girlfriend--curl-renew-token ()
  "Renew session token."
  (with-temp-buffer
    (ai-girlfriend--curl-call-process
     "https://api.github.com/copilot_internal/v2/token" 'get nil
     "-H"
     (format "authorization: token %s"
             (ai-girlfriend-connection-github-token ai-girlfriend--connection)))
    (ai-girlfriend--curl-parse-renew-token)))


(defun ai-girlfriend--curl-analyze-answer (instance string callback no-history)
  "Analyse curl response.
Argument INSTANCE is the copilot chat instance to use.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
if the response should be added to history."
  (if (ai-girlfriend--instance-support-responses-endpoint instance)
      (ai-girlfriend--responses-analyze
       instance
       (ai-girlfriend-curl-responses
        (ai-girlfriend--backend instance))
       string callback no-history)
    (ai-girlfriend--completions-analyze
     instance
     (ai-girlfriend-curl-completions
      (ai-girlfriend--backend instance))
     string callback no-history)))

(defun ai-girlfriend--curl-ask (instance prompt callback out-of-context)
  "Ask a question to Copilot using curl backend.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.  It can be a string or a list
of json objects.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  (setf
   (ai-girlfriend-curl-responses (ai-girlfriend--backend instance))
   (make-ai-girlfriend-responses)
   (ai-girlfriend-curl-completions (ai-girlfriend--backend instance)) (make-ai-girlfriend-completions))

  ;; Start the spinner animation only for instances with chat buffers
  (when (buffer-live-p (ai-girlfriend-chat-buffer instance))
    (ai-girlfriend--spinner-start instance))

  (let ((file (ai-girlfriend-curl-file (ai-girlfriend--backend instance))))
    (when (and file (file-exists-p file))
      (delete-file file)))
  (setf (ai-girlfriend-curl-file (ai-girlfriend--backend instance))
        (make-temp-file "ai-girlfriend"))
  (let ((coding-system-for-write 'raw-text))
    (with-temp-file (ai-girlfriend-curl-file (ai-girlfriend--backend instance))
      (insert
       (if (ai-girlfriend--instance-support-responses-endpoint instance)
           (ai-girlfriend--responses-create-req instance prompt out-of-context)
         (ai-girlfriend--completions-create-req
          instance prompt out-of-context)))))

  (unless out-of-context
    (let* ((history (ai-girlfriend-history instance))
           (new-history
            (if (stringp prompt)
                ;; classic prompt
                (cons `(:content ,prompt :role "user") history)
              ;; tool answer
              (append prompt history))))
      (setf (ai-girlfriend-history instance) new-history)))

  (ai-girlfriend--curl-make-process
   instance
   (if (ai-girlfriend--instance-support-responses-endpoint instance)
       "https://api.githubcopilot.com/responses"
     "https://api.githubcopilot.com/chat/completions")
   'post
   (concat "@" (ai-girlfriend-curl-file (ai-girlfriend--backend instance)))
   (lambda (proc string)
     (ai-girlfriend--debug 'curl "ai-girlfriend--curl-ask: %s" string)
     (if (not
          (string= string "quota exceeded\n"))
         (if (ai-girlfriend--instance-support-streaming instance)
             (ai-girlfriend--curl-analyze-answer
              instance string callback out-of-context)
           (ai-girlfriend--completions-analyze-nonstream
            instance
            (ai-girlfriend-curl-completions (ai-girlfriend--backend instance))
            proc
            string
            callback
            out-of-context))
       (ai-girlfriend--spinner-stop instance)
       (funcall callback instance "Quota exceeded.")))
   (ai-girlfriend-uses-vision instance)
   callback
   "-H"
   "openai-intent: conversation-panel"
   "-H"
   (concat
    "authorization: Bearer "
    (alist-get 'token (ai-girlfriend-connection-token ai-girlfriend--connection)))
   "-H"
   (concat "x-request-id: " (ai-girlfriend--uuid))
   "-H"
   (concat
    "vscode-sessionid: "
    (ai-girlfriend-connection-sessionid ai-girlfriend--connection))
   "-H"
   (concat
    "vscode-machineid: "
    (ai-girlfriend-connection-machineid ai-girlfriend--connection))))

(defun ai-girlfriend--curl-cancel (instance)
  "Cancel the current request for INSTANCE."
  (ai-girlfriend--spinner-stop instance)
  (let ((proc (ai-girlfriend-curl-process (ai-girlfriend--backend instance))))
    (when (process-live-p proc)
      (delete-process proc))))

(defun ai-girlfriend--curl-quotas ()
  "Get the current GitHub Copilot quotas."
  (with-temp-buffer
    (let* ((curl-args
            (append
             (list
              "https://api.github.com/rate_limit" "-s" "-X" "GET" "-H"
              (concat
               "authorization: Bearer "
               (ai-girlfriend-connection-github-token ai-girlfriend--connection))
              "-H" "Accept: application/vnd.github+json")))
           (result
            (apply #'call-process
                   ai-girlfriend-curl-program
                   nil
                   t
                   nil
                   curl-args)))
      (when (/= result 0)
        (error (format "curl returned non-zero result: %d" result))))
    (goto-char (point-min))
    (let* ((json-data
            (json-parse-buffer :object-type 'alist :false-object :json-false))
           (resources (alist-get 'resources json-data))
           (result '()))
      (dolist (resource resources)
        (let* ((name
                (capitalize
                 (replace-regexp-in-string
                  "_" " "
                  (symbol-name (car resource)))))
               (data (cdr resource))
               (limit (alist-get 'limit data))
               (used (alist-get 'used data))
               (remaining (alist-get 'remaining data))
               (reset (alist-get 'reset data)))
          (push (list name limit used remaining reset) result)))
      (nreverse result))))

(defun ai-girlfriend--curl-init (instance)
  "Initialize Copilot chat curl backend for INSTANCE."
  (setf (ai-girlfriend--backend instance) (make-ai-girlfriend-curl)))


;; Top-level execute code.
(cl-pushnew
 (make-ai-girlfriend-backend
  :id 'curl
  :init-fn #'ai-girlfriend--curl-init
  :clean-fn nil
  :login-fn #'ai-girlfriend--curl-login
  :renew-token-fn #'ai-girlfriend--curl-renew-token
  :ask-fn #'ai-girlfriend--curl-ask
  :cancel-fn #'ai-girlfriend--curl-cancel
  :quotas-fn #'ai-girlfriend--curl-quotas)
 ai-girlfriend--backend-list
 :test #'equal)

(provide 'ai-girlfriend-curl)
;;; ai-girlfriend-curl.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
