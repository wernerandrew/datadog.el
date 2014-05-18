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

(defvar dogapi-api-endpoint "https://app.datadoghq.com/"
  "URL to which API requests should be sent")

(defconst dogapi-api-version 1)

(defvar dogapi-api-key nil
  "API key for Datadog API.")

(defvar dogapi-application-key nil
  "Application key for Datadog API")

(defconst dogapi-url-endpoints
  (list '(dogapi-get-dash-list . "dash")
        '(dogapi-get-dash . "dash/%d")
        '(dogapi-get-metric-series . "series/query")))

;; External functions

(defun dogapi-metric-query (query from-ts to-ts)
  "Main metric query function.
Currently only a single query supported per call."
  (let* ((response (dogapi--request
                    'dogapi-get-metric-series
                    nil
                    (list (cons "q" query)
                          (cons "from" from-ts)
                          (cons "to" to-ts))))
         (series (cdr (assoc 'series response))))
    series
    ))

;; Helper functions

(defun dogapi--request (request-name url-param extra-args)
  "docstring here"
  (let* ((url (dogapi--request-url request-name url-param extra-args))
          (url-request-method "GET"))

    (dogapi--json-from-buffer (url-retrieve-synchronously url))
  ))

(defun dogapi--json-from-buffer (buffer)
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

(defun dogapi--request-url (request-name url-param extra-args)
  "Generates the actual URL to be used"
  (message url-param)
  (let* ((request-base (cdr (assoc request-name dogapi-url-endpoints)))
         (request-path (if url-param
                           (format request-base url-param)
                         request-base)))

    (concat (dogapi--join-url-segments (list dogapi-api-endpoint
                                         (dogapi--get-api-base)
                                         request-path))
            (format "?api_key=%s&application_key=%s"
                    dogapi-api-key dogapi-application-key)
            (mapconcat
             (lambda (x)
               (format "&%s=%s"
                       (url-hexify-string (car x))
                       (url-hexify-string (cdr x))))
             extra-args ""))
    ))

(defun dogapi--get-api-base ()
  (format "api/v%d" dogapi-api-version))

(defun dogapi--join-url-segments (segments)
  "Joins url-segments with at most 1 '/' character"
  (replace-regexp-in-string
   "([^\:])/+"
   "\\1/"
   (mapconcat 'identity segments "/")))

(provide 'dogapi)
