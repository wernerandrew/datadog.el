;;; datadog.el --- Emacs major mode for Datadog

;; Copyright (C) 2014 Drew Werner

;; Author: Drew Werner <wernerandrew at gmail.com>
;; Package-Requires:
;; Version: 0.0.1alpha

;; LICENSE GOES HERE

;; Commentary:

;; Code

;; Testing stuff

(defun datadog--make-random-series (start end interval rand-min rand-max)
  (defun make-random ()
    (+ rand-min (random* rand-max)))
  (let ((output nil)
        (current end))
    (while (> current start)
      (setq output (cons (list current (make-random)) output))
      (setq current (- current interval)))
    output))

;; Time utilities

(defun datadog--utc-now (&optional before-secs)
  "Get seconds since EPOCH as an integer, with optional offset"
  (let ((ts (truncate (float-time))))
    (if before-secs (- ts before-secs) ts)))

(defun datadog--to-js-time (timestamp)
  "Our API expects string timestamps in milliseconds.  Who are we to argue?"
  (number-to-string (* 1000 timestamp)))

;; Axis and chart formatting

(defconst datadog--supported-query-timeframes
  '(one-hour four-hours one-day one-week))

;; TODO: use this rather than hardcoded tick sizes
(defconst datadog--preferred-tick-spacings
  '(10 30 60 120 300 600 1800 3600 10800 21600 86400 172800)
  "spacings are in seconds, up to two days")

(defconst datadog--ticks-for-timeframe
  (list '(one-hour . 900)
        '(four-hours . 3600)
        '(one-day . 21600)
        '(one-week . 172800)))

(defun datadog--time-axis-ticks (start end interval tick-size)
  (defun closest-below (ts factor) (- ts (% ts factor)))

  (let* ((current (closest-below end tick-size))
         (ticks nil))
    (while (> current start)
      (setq ticks (cons (closest-below current interval) ticks))
      (setq current (- current tick-size)))
    ticks
    ))

(defun datadog--y-axis-ticks (minval maxval ticks)
  nil)

(defvar datadog--number-scale
  '(("u" . 1e-6)
    ("m" . 1e-3)
    ("" . 1.0)
    ("K" . 1e3)
    ("M" . 1e6)
    ("G" . 1e9)
    ("T" . 1e12)))

(defvar datadog--precision 1)

(defun datadog--format-number (x)
  (defun scale-number (num order suffix orders)
    (if orders
        (let ((next-suffix (caar orders))
              (next-order (cdar orders)))
          (if (> num next-order)
              (scale-number num next-order next-suffix (cdr orders))
            (cons suffix (/ num order))))
      (cons suffix (/ num order))))

  (defun strip-trailing-zeros (x)
    (let ((xs (if (stringp x) x (number-to-string x))))
      (replace-regexp-in-string "\\.0+" "" x)))

  (defun truncate-to-precision (x p)
    (let ((scale (expt 10.0 p)))
      (number-to-string (/ (truncate x (/ 1 scale)) scale))))

  (let* ((scale-data (scale-number x 1e-6 "" datadog--number-scale))
         (unit (car scale-data))
         (val (truncate-to-precision (cdr scale-data) datadog--precision)))
    (if (= (length unit) 0)
        (strip-trailing-zeros val)
      (concat val unit))))

;; Drawing functions

;; Buffer state variables

(defvar datadog-active-query nil
  "Stores the active query that we're looking at")

(defvar datadog-current-result nil
  "Stores the active result as a list of (scope . pointlist)")

(defvar datadog-timeframe 'one-hour
  "Currently supported")

(defvar datadog-last-refresh-time nil)

;; Functions that like, you know, map to series
(defun datadog-next-series ()
  (interactive)
  nil)

(defun datadog-previous-series ()
  (interactive)
  nil)

(defun datadog-go-forward ()
  (interactive)
  nil)

(defun datadog-go-backward ()
  (interactive)
  nil)

(defun datadog-refresh ()
  (interactive)
  nil)

(defun datadog-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Graph drawing routines

;; maybe could have done this as a bunch of alists,
;; but I'm lazy (and maybe stupid)
(defconst datadog--graph-sizes
  '((0 0 0 40 8)
    (3 8 2 60 20)
    (3 8 2 120 40)))

(defvar datadog--graph-size nil)

(defvar datadog--graph-origin nil
  "(column, row) pair")

(defvar datadog--dim-names
  '(margin-below margin-left margin-top width height))

(defun datadog--get-graph-dim (dim-name &optional dims)
  "If dims not provided, defaults to datadog--graph-size"
  (defun find-dim (dims dim-name names-list)
    (when dims
      (if (eq dim-name (car names-list))
          (car dims)
        (find-dim (cdr dims) dim-name (cdr names-list)))))
  (let ((sz (or dims datadog--graph-size)))
    (find-dim sz dim-name datadog--dim-names)))

(defun datadog--graph-extent (&optional dims)
  "returns (width, height) pair"
  (let ((dims (or dims datadog--graph-size)))
    (cons (+ (datadog--get-graph-dim 'margin-left dims)
             (datadog--get-graph-dim 'width dims))
          (+ (datadog--get-graph-dim 'margin-below dims)
             (datadog--get-graph-dim 'margin-top dims)
             (datadog--get-graph-dim 'height dims)))))

(defun datadog--set-graph-size ()
  (defun graph-fits (dims w h)
    (let ((extent (datadog--graph-extent dims)))
      (and (< (car extent) w)
           (< (cdr extent) h))))
  (defun iter-get-graph-size (current dims-list w h)
    (when dims-list
      (if (graph-fits (car dims-list) w h)
          (iter-get-graph-size (car dims-list) (cdr dims-list) w h)
        current)))
  (setq datadog--graph-size
        (iter-get-graph-size nil
                             datadog--graph-sizes
                             (window-width)
                             (window-height))))

(defun datadog--set-graph-origin ()
  (when datadog--graph-size
    (let* ((extent (datadog--graph-extent))
           (extra-width (- (window-width) (car extent))))
      (setq datadog--graph-origin
            (cons (/ extra-width 2)
                  (cdr extent))))))

(defun datadog--clear-chart-area ()
  (erase-buffer))

(defun datadog--draw-axes ()
  (save-excursion
    (goto-char (point-min))
    (newline (datadog--get-graph-dim 'margin-top))
    (let* ((graph-base (cdr datadog--graph-origin))
           (offset (car datadog--graph-origin))
           (graph-max-col (+ offset (datadog--get-graph-dim 'width))))
      (while (< (line-number-at-pos) graph-base)
        (insert-char ?\s offset)
        (insert-char ?|)
        (newline))
      (insert-char ?\s offset)
      (insert ?+)
      (while (< (current-column) graph-max-col)
        (insert ?-)))))

;; Main entry function

(defun datadog ()
  (interactive)
  (let ((existing-buffer (get-buffer "*datadog*")))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (progn
        (switch-to-buffer (get-buffer-create "*datadog*"))
        (datadog-mode)))))

;; Mode definition stuff

(defvar datadog-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'datadog-next-series)
    (define-key map (kbd "p") 'datadog-previous-series)
    (define-key map (kbd "f") 'datadog-go-forward)
    (define-key map (kbd "b") 'datadog-go-backward)
    (define-key map (kbd "r") 'datadog-refresh)
    map)
  "Datadog mode keymap")

(define-derived-mode datadog-mode fundamental-mode "Datadog"
  "Major mode for viewing and having all sorts of Datadog fun.

\\{datadog-mode-map}"
  (buffer-disable-undo)
  (use-local-map datadog-mode-map)

  ;; local variable definitions
  (make-local-variable 'show-trailing-whitespace)
  ;; (make-local-variable 'datadog--series-data)
  ;; (make-local-variable 'datadog--graph-size)
  ;; (make-local-variable 'datadog--graph-origin)
  (datadog--init)
  )

(defun datadog--init ()
  (setq show-trailing-whitespace nil)
  (datadog--set-graph-size)
  (datadog--set-graph-origin)
  (datadog--clear-chart-area)
  (datadog--draw-axes)
  (datadog-refresh))

(provide 'datadog)
