;;; datadog.el --- Emacs major mode for Datadog

;; Copyright (C) 2014 Drew Werner

;; Author: Drew Werner <wernerandrew at gmail.com>
;; Package-Requires:
;; Version: 0.0.1alpha

;; LICENSE GOES HERE

;; Commentary:

;; Code

;; Testing stuff

(defun dog/make-random-series (start end interval rand-min rand-max)
  (defun make-random ()
    (+ rand-min (random* rand-max)))
  (let ((output nil)
        (current end))
    (while (> current start)
      (setq output (cons (list current (make-random)) output))
      (setq current (- current interval)))
    output))

;; Time utilities

(defun dog/utc-now (&optional before-secs)
  "Get seconds since EPOCH as an integer, with optional offset"
  (let ((ts (truncate (float-time))))
    (if before-secs (- ts before-secs) ts)))

(defun dog/to-js-time (timestamp)
  "Our API expects string timestamps in milliseconds.  Who are we to argue?"
  (number-to-string (* 1000 timestamp)))


;; Axis and chart formatting

(defconst dog/supported-query-timeframes
  '(one-hour four-hours one-day one-week))

;; TODO: use this rather than hardcoded tick sizes
(defconst dog/preferred-tick-spacings
  '(10 30 60 120 300 600 1800 3600 10800 21600 86400 172800)
  "spacings are in seconds, up to two days")

(defconst dog/ticks-for-timeframe
  (list '(one-hour . 900)
        '(four-hours . 3600)
        '(one-day . 21600)
        '(one-week . 172800)))

(defun dog/time-axis-ticks (start end interval tick-size)
  (defun closest-below (ts factor) (- ts (mod ts factor)))

  (let* ((current (closest-below end tick-size))
         (ticks nil))
    (while (> current start)
      (setq ticks (cons (closest-below current interval) ticks))
      (setq current (- current tick-size)))
    ticks
    ))

;; Buffer state variables

(defvar dog/active-query nil
  "Stores the active query that we're looking at")

(defvar dog/current-result nil
  "Stores the active result as a list of (scope . pointlist)")

(defvar dog/timeframe 'one-hour
  "Currently supported")

(defvar dog/last-refresh-time nil)

;; Functions
(defun dog/next-series ()
  nil)

(defun dog/previous-series ()
  nil)

(defun dog/go-forward ()
  nil)

(defun dog/go-backward ()
  nil)

(defun dog/refresh ()
  nil)

;; Mode definition stuff

(defvar datadog-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'dog/next-series)
    (define-key map (kbd "p") 'dog/previous-series)
    (define-key map (kbd "f") 'dog/go-forward)
    (define-key map (kbd "b") 'dog/go-backward)
    (define-key map (kbd "r") 'dog/refresh)
    map)
  "Datadog mode keymap")

(define-derived-mode datadog-mode special-mode "Datadog"
  "Major mode for viewing and having all sorts of Datadog fun.

\\{datadog-mode-map}"
  (switch-to-buffer (get-buffer-create "*datadog*"))
  (buffer-disable-undo)
  (make-variable-buffer-local 'show-trailing-whitespace)
  (setq show-trailing-whitespace nil)
  (use-local-map datadog-mode-map))
