;;; datadog.el --- Emacs major mode for Datadog

;; Copyright (C) 2014 Drew Werner

;; Author: Drew Werner <wernerandrew at gmail.com>
;; Package-Requires:
;; Version: 0.0.1alpha

;; LICENSE GOES HERE

;; Commentary:

;; Code

;; Testing stuff

(require 'dogapi)

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

(defun datadog--utc-now (&optional offset-secs)
  "Get seconds since EPOCH as an integer, with optional offset"
  (let ((ts (truncate (float-time))))
    (if offset-secs (+ ts offset-secs) ts)))

(defun datadog--to-js-time (timestamp)
  "Our API expects string timestamps in milliseconds.  Who are we to argue?"
  (number-to-string (* 1000 timestamp)))

;; Drawing functions

;; Buffer state variables

(defvar datadog--active-query nil
  "Stores the active query that we're looking at")

(defvar datadog--active-interval nil)

(defvar datadog--active-from-ts nil)

(defvar datadog--active-to-ts nil)

(defvar datadog--current-result nil
  "Stores the active result as a list of (scope . pointlist)")

(defvar datadog--looking-at 0
  "This the the current result we're looking at")

(defvar datadog--timeframe 'one-hour
  "Currently supported")

(defvar datadog--time-spans
  (list '(one-hour . 3600)
        '(four-hours . 14400)
        '(one-day . 86400)
        '(one-week . 604800)))

(defvar datadog-last-refresh-time nil)

;; Functions that like, you know, map to series
(defun datadog-metric-query (query &optional skip-render)
  "It's a metric query, kids."
  (interactive "sEnter metric query: ")
  (let* ((offset (cdr (assoc datadog--timeframe datadog--time-spans)))
         (to-utc (datadog--utc-now))
         (from-utc (- to-utc offset))
         (interval (datadog--rollup-interval))
         (series (dogapi-metric-query (datadog--make-query query interval)
                                      (datadog--to-js-time from-utc)
                                      (datadog--to-js-time to-utc))))
         ;; TODO: set interval through API!  This is a kludge
         ;; (rollup (datadog--rollup-method-for query)))
    (when series
      (message "Retrieved series from server")
      (setq datadog--active-query query)
      (setq datadog--active-interval interval)
      (setq datadog--active-from-ts
            (datadog--closest-below from-utc interval))
      (setq datadog--active-to-ts
            (datadog--closest-below to-utc interval))
      (setq datadog--current-result series)
      (setq datadog--looking-at 0)
      (when (not skip-render)
        (datadog--render-graph)))))

(defconst datadog--rollup-hack
  (list '("}$" . "}.rollup(%d)")
        '( "\\.rollup(\\([^0-9]*\\)[^)]*)" . ".rollup(\\1%d)")
        '("}[ ]*\\." . "}.rollup(%d).")
        '("}[ ]*)" . "}.rollup(%d))"))
  "Kid tested. Mother approved.")

(defun datadog--make-query (query interval)
  "Does some regex substitutions to ensure a proper interval
Hopefully to go the way of the dodo when / if we support
setting the interval directly through API requests."
  (defun maybe-substitute (patterns)
    (if patterns
        (let ((pat (caar patterns))
              (subst (format (cdar patterns) interval)))
          (if (string-match pat query)
              (replace-regexp-in-string pat subst query)
            (maybe-substitute (cdr patterns))))
      query))
  (maybe-substitute datadog--rollup-hack))

(defun datadog--rollup-interval ()
  (let* ((time-window (cdr (assoc datadog--timeframe
                                  datadog--time-spans)))
         (num-points (datadog--get-graph-dim 'width)))
    (/ time-window num-points)))

;; Datadog keyboard commands

(defun datadog--checked-nav (offset)
  (interactive)
  (let ((next-idx (+ datadog--looking-at offset)))
    (if (and (< next-idx (length datadog--current-result))
             (>= next-idx 0))
        (progn
          (setq datadog--looking-at next-idx)
          (datadog--render-graph))
      (error (if (< next-idx 0) "Beginning of series" "End of series")))
    ))

(defun datadog-next-series ()
  (interactive)
  (datadog--checked-nav 1))

(defun datadog-previous-series ()
  (interactive)
  (datadog--checked-nav -1))

(defun datadog-go-forward ()
  (interactive)
  nil)

(defun datadog-go-backward ()
  (interactive)
  nil)

(defun datadog-refresh ()
  (interactive)
  ;; TODO: save active looking-at
  (let ((current-idx datadog--looking-at))
    (datadog-metric-query datadog--active-query t)
    ;; stay on current graph index if possible
    (when (> (length datadog--current-result) current-idx)
      (setq datadog--looking-at current-idx))
    (datadog--render-graph)))

(defun datadog-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Graph drawing routines variables and routines

;; maybe could have done this as a bunch of alists,
;; but I'm lazy (and maybe stupid)
(defconst datadog--graph-sizes
  '((0 0 0 40 8)
    (3 4 2 60 20)
    (3 4 2 120 40)))

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
    ;; the "1" is space for the axis itself
    (cons (+ (datadog--get-graph-dim 'margin-left dims)
             (datadog--get-graph-dim 'width dims)
             1)
          (+ (datadog--get-graph-dim 'margin-below dims)
             (datadog--get-graph-dim 'margin-top dims)
             (datadog--get-graph-dim 'height dims)
             1))))

(defun datadog--set-graph-size ()
  (defun graph-fits (dims w h)
    (let ((extent (datadog--graph-extent dims)))
      (and (< (car extent) w)
           (< (cdr extent) h))))
  (defun iter-get-graph-size (current dims-list w h)
    (if dims-list
      (if (and (graph-fits (car dims-list) w h))
          (iter-get-graph-size (car dims-list) (cdr dims-list) w h)
        current)
      current))
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
            (cons (+ (/ extra-width 2)
                     (datadog--get-graph-dim 'margin-left))
                  (cdr extent))))))

(defun datadog--clear-chart-area ()
  (let ((buffer-read-only nil))
    (erase-buffer)))

(defun datadog--draw-axes ()
  (let* ((buffer-read-only nil)
         (graph-base (cdr datadog--graph-origin))
         (offset (car datadog--graph-origin))
         (graph-max-col (+ 1 offset (datadog--get-graph-dim 'width))))
    (save-excursion
      (goto-char (point-min))
      (newline (datadog--get-graph-dim 'margin-top))
      (while (< (line-number-at-pos) graph-base)
        (insert-char ?\s offset)
        (insert-char ?|)
        (newline))
      (insert-char ?\s offset)
      (insert ?+)
      (while (<= (current-column) graph-max-col)
        (insert ?-)))))

(defun datadog--reset-graph ()
  (datadog--set-graph-size)
  (datadog--set-graph-origin)
  (datadog--clear-chart-area)
  (datadog--draw-axes))

(defun datadog--series-extent (points)
  (let* ((vals (mapcar (lambda (p) (elt p 1)) points)))
    (cons (apply 'min vals)
          (apply 'max vals))))

;; Scaling convenience helpers
(defun datadog--scale-y (raw ymin yrange)
  (- (cdr datadog--graph-origin)
     (* (/ (- raw ymin) yrange)
        (datadog--get-graph-dim 'height))))

(defun datadog--scale-t (timestamp &optional not-js)
  (let* ((factor (if not-js 1 1000))
         (ts (truncate (/ timestamp factor))))
    (+ (car datadog--graph-origin) 1
       (/ (- ts datadog--active-from-ts) datadog--active-interval))))


(defun datadog--filter-points (points)
  "Filter points out of time range as well as nil points"
  (let ((min-t (datadog--scale-t datadog--active-from-ts t))
        (max-t (datadog--scale-t datadog--active-to-ts t)))
    (delq nil
          (mapcar (lambda (p)
                    (let ((ts (datadog--scale-t (elt p 0)))
                          (val (elt p 1)))
                      (if (or (< ts min-t)
                              (> ts max-t)
                              (null val))
                          nil p)))
                  points)
          )
    ))

(defun datadog--set-graph-title (title)
  (let* ((buffer-read-only nil)
         (w (window-width))
         (offset (max 0 (/ (- w (length title)) 2)))
         (title (if (> (length title) w)
                    (substring title 0 w)
                  title)))
    (when (> (datadog--get-graph-dim 'margin-top) 1)
      (save-excursion
        (goto-line 1)
        (when (not (looking-at "^$"))
          (kill-line))
        (insert-char ?\s offset)
        (insert title)))
    ))

(defun datadog--render-graph ()

  (datadog--reset-graph)

  (let* ((buffer-read-only nil)
         (series (elt datadog--current-result datadog--looking-at))
         (points (datadog--filter-points (cdr (assoc 'pointlist series))))
         (extent (datadog--series-extent points))
         (ymin (min (car extent) 0.0))
         (ymax (cdr extent))
         (yrange (if (> (- ymax ymin) 0)
                     (- ymax ymin)
                   (max ymax 1))))

    (datadog--set-graph-title (cdr (assoc 'expression series)))
    (save-excursion
      (goto-char (point-min))
      (forward-line (datadog--get-graph-dim 'margin-top))

      ;; this is where the magic happens
      (let ((scaled (mapcar (lambda (p)
                              (cons (datadog--scale-t (elt p 0))
                                    (datadog--scale-y (elt p 1)
                                                      ymin
                                                      yrange)))
                            points)))

        (while (< (line-number-at-pos) (cdr datadog--graph-origin))
          (forward-char (+ (car datadog--graph-origin) 1))
          (dolist (sc scaled)
            (insert-char ?\s (- (car sc) (current-column)))
            (if (> (line-number-at-pos) (cdr sc))
                (insert-char ?*)
              (insert-char ?\s)))
          (forward-line 1))))

    ;; draw our ticks
    (datadog--draw-t-ticks)
    (datadog--draw-y-ticks ymin ymax yrange)
    ;; and end up in some vaguely sensible place
    (goto-char (point-min))
    ))

;; TODO: use this rather than hardcoded tick sizes
(defconst datadog--preferred-tick-spacings
  '(10 30 60 120 300 600 1800 3600 10800 21600 86400 172800)
  "spacings are in seconds, up to two days")

;; little utility function here
(defun datadog--closest-below (ts interval)
  (- ts (% ts interval)))

(defconst datadog--ticks-for-timeframe
  (list '(one-hour . 900)
        '(four-hours . 3600)
        '(one-day . 21600)
        '(one-week . 172800)))

(defconst datadog--format-for-timeframe
  (list '(one-hour . "%H:%M")
        '(four-hours . "%H:%M")
        '(one-day . "%H:%M")
        '(one-week . "%b %d")))

(defun datadog--time-axis-ticks (start end interval tick-size)

  (let* ((current (datadog--closest-below end tick-size))
         (ticks nil))
    (while (> current start)
      (setq ticks (cons (datadog--closest-below current interval) ticks))
      (setq current (- current tick-size)))
    ticks
    ))

(defun datadog--ensure-next-line-empty ()
  "When you need one.  Probably a little inefficient,
but I mean c'mon.  It's 2014.  Leaves you at the beginning
of the line where you started."
  ;; ensure next line exists
  (when (> (forward-line) 0)
    (progn
      (goto-char (point-max))
      (newline)))
  ;; and is blank
  (beginning-of-line)
  (when (not (looking-at "^$"))
    (kill-line))
  ;; this is kinda ugly, but save-excursion didn't work
  ;; for whatever reason
  (previous-line)
  (beginning-of-line))

(defun datadog--draw-t-ticks ()
  (let* ((timeframe datadog--timeframe)
         (tick-size (cdr (assoc timeframe
                                datadog--ticks-for-timeframe)))
         (ticks (datadog--time-axis-ticks datadog--active-from-ts
                                          datadog--active-to-ts
                                          datadog--active-interval
                                          tick-size))
         (time-fmt (cdr (assoc timeframe
                               datadog--format-for-timeframe)))
         (last-time-column 0)
         (time-padding-cols 2))
    (save-excursion
      (goto-line (+ 1 (cdr datadog--graph-origin)))
      (datadog--ensure-next-line-empty)
      ;; goto the char one past the origin
      (goto-char (+ 1 (search-forward "+")))
      (dolist (tick ticks)
        (let* ((col (datadog--scale-t tick t))
               (time (format-time-string time-fmt (seconds-to-time tick)))
               (time-start-col (- col (/ (length time) 2))))

          (goto-char (+ (point) (- col (current-column))))
          (delete-char 1)
          (insert-char ?|)
          ;; Can we insert the time?
          (when (> time-start-col (+ last-time-column time-padding-cols))
            (save-excursion
              (forward-line)
              (end-of-line)
              (insert-char ?\s (- time-start-col (current-column)))
              (insert time)
              (setq last-time-column (current-column))))
          ))
        )))

;; Axis and chart formatting

(defconst datadog--supported-query-timeframes
  '(one-hour four-hours one-day one-week))

(defun datadog--get-number-scale (x)
  (floor (log10 x)))

(defun datadog--y-axis-tick-size (ymin ymax)
  (let* ((yrange (if (= ymax ymin)
                    (max ymax 1)
                  (- ymax ymin)))
         (scale (floor (log10 yrange)))
         (scale-factor (expt 10 scale))
         (scaled-yrange (/ yrange scale-factor)))
    (* scale-factor
       (cond ((< scaled-yrange 1) 0.1)
             ((< scaled-yrange 2) 0.5)
             ((< scaled-yrange 5) 1.0)
             (t 2.0)))
    ))

(defun datadog--y-axis-ticks (ymin ymax)
  "For now, assumes that the range includes zero.
Maybe we should relax that assumption at some point."
  (let* ((tick-size (datadog--y-axis-tick-size ymin ymax))
         (ticks '(0.0))
         (last-tick ticks)
         (current tick-size))
    ;; expand in both directions
    (while (< current ymax)
      (setcdr last-tick (list current))
      (setq last-tick (cdr last-tick))
      (setq current (+ current tick-size)))
    (setq current (- tick-size))
    (while (> current ymin)
      (setq ticks (cons current ticks))
      (setq current (- current tick-size)))
    ticks
    ))

(defun datadog--draw-y-ticks (ymin ymax yrange)
  (let* ((ticks (datadog--y-axis-ticks ymin ymax))
         (axis-offset (car datadog--graph-origin))
         (axis-line (cdr datadog--graph-origin))
         (min-tick-spacing 2)
         (last-tick (+ axis-line min-tick-spacing 1))
         (graph-top (datadog--get-graph-dim 'margin-top)))

    (dolist (tick ticks)
      (let* ((line (floor (datadog--scale-y tick ymin yrange)))
             (text (datadog--format-number tick))
             (text-field-size (+ 1 (length text))))
        (unless (or (< (- last-tick line) min-tick-spacing))
          ;; insert tick
          (goto-line line)
          (unless (or (= line axis-line)
                      (<= line graph-top))
            (goto-char (+ (point) axis-offset))
            (delete-char 1)
            (insert-char ?-)
          ;; insert formatted number, if space
            (when (> (current-column) text-field-size)
              (goto-char (- (point) (+ text-field-size 1)))
              (delete-char text-field-size)
              (insert (concat text "-")))))
        ))
    ))

(defvar datadog--number-scale
  '(("" . 1.0)
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
      (replace-regexp-in-string "\\.0+$" "" x)))

  (defun truncate-to-precision (x p)
    (let ((scale (expt 10.0 p)))
      (number-to-string (/ (truncate x (/ 1 scale)) scale))))

  (let* ((scale-data (scale-number x 1.0 "" datadog--number-scale))
         (unit (car scale-data))
         (val (cdr scale-data))
         (precision (if (= (length unit) 0) 3 datadog--precision))
         (string-val (truncate-to-precision val precision)))

    (if (= (length unit) 0)
        (strip-trailing-zeros string-val)
      (concat string-val unit))
    ))

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
    (define-key map (kbd "m") 'datadog-metric-query)
    (define-key map (kbd "n") 'datadog-next-series)
    (define-key map (kbd "p") 'datadog-previous-series)
    (define-key map (kbd "f") 'datadog-go-forward)
    (define-key map (kbd "b") 'datadog-go-backward)
    (define-key map (kbd "r") 'datadog-refresh)
    map)
  "Datadog mode keymap")

(define-derived-mode datadog-mode special-mode "Datadog"
  "Major mode for viewing and having all sorts of Datadog fun.

\\{datadog-mode-map}"
  (buffer-disable-undo)
  (use-local-map datadog-mode-map)

  ;; local variable definitions
  (datadog--init)
  )

(defun datadog--init ()
  (interactive)
  (make-local-variable 'show-trailing-whitespace)
  (make-local-variable 'datadog--active-query)
  (make-local-variable 'datadog--current-result)
  (make-local-variable 'datadog--timeframe)
  (make-local-variable 'datadog--series-data)
  (make-local-variable 'datadog--graph-size)
  (make-local-variable 'datadog--graph-origin)

  (setq show-trailing-whitespace nil)
  (datadog--reset-graph))

(provide 'datadog)