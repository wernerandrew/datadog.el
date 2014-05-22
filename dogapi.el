;;; dogapi.el --- emacs interface to the Datadog api

;; Copyright (C) 2014 Drew Werner <wernerandrew at gmail dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Package-Requires: url json
;; URL: http://github.com/wernerandrew/datadog.el
;; Version: 0.0.1alpha

;; See LICENSE file for licensing details.

;; Commentary:

;; Code

(require 'url)
(require 'json)

;; Configuration variables

(defvar dogapi-api-endpoint "https://app.datadoghq.com"
  "URL to which API requests should be sent")

(defconst dogapi-api-version 1)

(defvar dogapi-api-key nil
  "API key for Datadog API.")

(defvar dogapi-application-key nil
  "Application key for Datadog API")

(defvar dogapi-credentials-path "~/.emacs.d"
  "Directory to persist the file described in
`dogapi-credentials-file'")

(defvar dogapi-credentials-file "dogapi-credentials"
  "Name of file to store dogapi credentials")

(defvar dogapi-save-credentials t
  "Set to non-nil to persist api credentials to a file")

(defconst dogapi-url-endpoints
  (list '(dogapi-get-dash-list . "dash")
        '(dogapi-get-dash . "dash/%d")
        '(dogapi-get-metric-series . "series/query")))

;; Credentials management

(defun dogapi--credentials-file-full-path ()
  (expand-file-name
   (concat (file-name-as-directory dogapi-credentials-path)
           dogapi-credentials-file)))

(defun dogapi--set-credentials-from-file ()
  (let ((cred-file (dogapi--credentials-file-full-path)))
    (if (file-exists-p cred-file)
        (let ((cred-data (json-read-from-string
                          (with-temp-buffer
                            (insert-file-contents cred-file)
                            (buffer-string)))))
          (setq dogapi-api-key (cdr (assoc 'dogapi-api-key cred-data)))
          (setq dogapi-application-key
                (cdr (assoc 'dogapi-application-key cred-data)))
          t)
      nil)
    ))

(defun dogapi--save-credentials-to-file ()
  (let* ((cred-file (dogapi--credentials-file-full-path))
         (cred-data (list (cons 'dogapi-api-key dogapi-api-key)
                          (cons 'dogapi-application-key dogapi-application-key))))
    ;; lots must go right to save this
    ;; credentials path must be a directory
    (if (and (file-directory-p (expand-file-name dogapi-credentials-path))
             (or (not (file-exists-p cred-file))
                 (yes-or-no-p (format "Overwrite credentials file: %s "
                                      cred-file))))
        (with-temp-file cred-file
          (insert (json-encode cred-data)))
      (message "Error writing credentials to file."))))

(defun dogapi--has-credentials ()
  (not (or (null dogapi-api-key)
           (null dogapi-application-key))))

(defun dogapi--ensure-credentials ()
  (unless (dogapi--has-credentials)
    ;; first attempt: read from file
    (when dogapi-save-credentials
      (dogapi--set-credentials-from-file))
    (unless (dogapi--has-credentials)
      (dogapi-set-credentials))
   ))

(defun dogapi-set-credentials ()
  "Specify the API key and Application key to be used"
  (let* ((api-key (read-string "Enter api key: "))
         (app-key (read-string "Enter application key: ")))
    (if (and api-key app-key)
        (progn
          (setq dogapi-api-key api-key)
          (setq dogapi-application-key app-key)
          (when dogapi-save-credentials
            (dogapi--save-credentials-to-file)))
      (error "Couldn't set credentials"))))

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

(defun dogapi-dash-list ()
  (dogapi--request 'dogapi-get-dash-list nil nil))

(defun dogapi-dash (dash-id)
  (dogapi--request 'dogapi-get-dash dash-id nil))

;; Helper functions

(defun dogapi--request (request-name url-param extra-args)
  "docstring here"
  (dogapi--ensure-credentials)
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
