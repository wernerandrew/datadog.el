;;; dogapi.el --- emacs interface to the Datadog api

;; Copyright (C) 2014 Drew Werner

;; Author: Drew Werner <wernerandrew at gmail.com>
;; Package-Requires:
;; Version: 0.0.1alpha

;; LICENSE GOES HERE

;; Commentary:

;; Code

(require 'url)
(require 'json)

;; Configuration variables

(defvar dog/api-endpoint "https://app.datadoghq.com/"
  "URL to which API requests should be sent")

(defconst dog/api-version 1)

(defvar dog/api-key nil
  "API key for Datadog API.")

(defvar dog/application-key nil
  "Application key for Datadog API")

(defconst dog/url-endpoints
  (list '(dog/get-dash-list . "dash")
        '(dog/get-dash . "dash/%d")
        '(dog/get-metric-series . "series/query")))

;; External functions

(defun dog/metric-query (query from-ts to-ts)
  (let ((response (dog/request 'dog/get-metric-series
                               (list (cons "q" query)
                                     (cons "from" from-ts)
                                     (cons "to" to-ts))
                               nil)))
    response
    ))

;; Helper functions

(defun dog/request (request-name url-param extra-args)
  "docstring here"
  (let* ((url (dog/request-url request-name url-param extra-args))
          (url-request-method "GET"))

    (dog/json-from-buffer (url-retrieve-synchronously url))
  ))

(defun dog/json-from-buffer (buffer)
  (let ((json-response nil))
    (save-excursion
      (switch-to-buffer buffer)
      (widen)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move) ;; skip header
      (setq json-response (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))
    (json-read-from-string json-response)
    ))

(defun dog/request-url (request-name url-param extra-args)
  "Generates the actual URL to be used"
  (message url-param)
  (let* ((request-base (cdr (assoc request-name dog/url-endpoints)))
         (request-path (if url-param
                           (format request-base url-param)
                         request-base)))

    (concat (dog/join-url-segments (list dog/api-endpoint
                                         (dog/get-api-base)
                                         request-path))
            (format "?api_key=%s&application_key=%s"
                    dog/api-key dog/application-key)
            (mapconcat
             (lambda (x)
               (format "&%s=%s"
                       (url-hexify-string (car x))
                       (url-hexify-string (cdr x))))
             extra-args ""))
    ))

(defun dog/get-api-base ()
  (format "api/v%d" dog/api-version))

(defun dog/join-url-segments (segments)
  "Joins url-segments with at most 1 '/' character"
  (replace-regexp-in-string
   "[^:]/+"
   "/"
   (mapconcat 'identity segments "/")))
