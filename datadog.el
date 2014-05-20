;;; datadog.el --- Emacs major mode for Datadog

;; Copyright (C) 2014 Drew Werner

;; Author: Drew Werner <wernerandrew at gmail.com>
;; Package-Requires: helm
;; URL: http://github.com/wernerandrew/datadog.el
;; Version: 0.0.1alpha

;; See LICENSE file for licensing details

;; Commentary:

(require 'helm)

(require 'dogapi)

;; Time utilities

(defun datadog--utc-now (&optional offset-secs)
  "Get seconds since EPOCH as an integer, with optional offset"
  (let ((ts (truncate (float-time))))
    (if offset-secs (+ ts offset-secs) ts)))

(defun datadog--to-js-time (timestamp)
  "Our API expects string timestamps in milliseconds.  Who are we to argue?"
  (number-to-string (* 1000 timestamp)))

;; Buffer state variables

(defvar datadog--active-tile nil
  "A (title . graphs) cons cell for the active tile.")

(defvar datadog--all-queries nil
  "A list of all queries that are navigable via
`datadog-next-series' and `datadog-previous-series'.
Most commonly, this is all queries within a graph tile.")

(defvar datadog--active-query nil
  "Stores the active query currently being navigated")

(defvar datadog--active-interval nil)

(defvar datadog--active-from-ts nil)

(defvar datadog--active-to-ts nil)

(defvar datadog--current-result nil
  "Stores the active result as a list of (scope . pointlist)")

(defvar datadog--looking-at-query 0
  "The index of the query currently being browsed")

(defvar datadog--looking-at-series 0
  "The current series within the current query")

(defvar datadog--timeframe 'one-hour
  "Currently supported")

(defvar datadog--time-spans
  '((one-hour . 3600)
    (four-hours . 14400)
    (one-day . 86400)
    (one-week . 604800)))

(defconst datadog--query-refresh-time
  '((one-hour . 60)
    (four-hours . 300)
    (one-day . 1800)
    (one-week . 3600)))

(defconst datadog--dash-list-refresh-time 300)

(defconst datadog--dash-refresh-time 300)

(defvar datadog--dash-list nil
  "Stored list of dashes so we don't always hit the server")

(defvar datadog--query-cache nil
  "Maps query string to (last-update-utc . result)")

(defvar datadog--dash-tiles nil
  "Stores tile definitions for current dash")

(defun datadog--fetch-series (query interval from-utc to-utc)
  "Get the query, but check the cache"
  (let* ((query-str (datadog--make-query query interval))
         (cached-result (gethash query-str datadog--query-cache)))
    (if (or (not cached-result)
            (> (- to-utc (car cached-result))
               (cdr (assoc datadog--timeframe
                           datadog--query-refresh-time))))
        (let ((result (dogapi-metric-query query-str
                                           (datadog--to-js-time from-utc)
                                           (datadog--to-js-time to-utc))))
          (puthash query-str (cons to-utc result) datadog--query-cache)
          result)
      (cdr cached-result))))

;; Functions that like, you know, map to series
(defun datadog-metric-query (query &optional skip-render)
  "It's a metric query, kids."
  (interactive)
  (let* ((offset (cdr (assoc datadog--timeframe datadog--time-spans)))
         (to-utc (datadog--utc-now))
         (from-utc (- to-utc offset))
         (interval (datadog--rollup-interval))
         (series (datadog--fetch-series query interval from-utc to-utc)))
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
      (setq datadog--looking-at-series 0)
      (when (not skip-render)
        (datadog--render-graph)))))

(defun datadog-explore-metric (query)
  "Query exactly one metric."
  (interactive  "sEnter metric query: ")
  (setq datadog--all-queries (list query))
  (setq datadog--active-tile nil)
  (setq datadog--looking-at-query 0)
  (datadog-metric-query query))

(defconst datadog--rollup-hack
  '(("}$" . "}.rollup(%d)")
    ( "\\.rollup(\\([^0-9]*\\)[^)]*)" . ".rollup(\\1%d)")
    ("}[ ]*\\." . "}.rollup(%d).")
    ("}[ ]*)" . "}.rollup(%d))"))
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
  (let ((next-idx (+ datadog--looking-at-series offset))
        (current-len (length datadog--current-result)))
    (cond
     ;; easy case: we can stay on this graph
     ((and (< next-idx current-len)
           (>= next-idx 0))
      (setq datadog--looking-at-series next-idx)
      (datadog--render-graph))
     ;; next: we are at the beginning but can go backwards
     ((and (= next-idx -1)
           (> datadog--looking-at-query 0))
      (setq datadog--looking-at-query (- datadog--looking-at-query 1))
      (datadog-metric-query (elt datadog--all-queries
                                 datadog--looking-at-query)
                            t) ;; skip render for now
      (let ((num-series (length datadog--current-result)))
        (when (> num-series 0)
          (setq datadog--looking-at-series (- num-series 1)))
        (datadog--render-graph)))
     ;; or, we're at the end, and can go forwards
     ((and (= next-idx 1)
           (< datadog--looking-at-query (- (length datadog--all-queries) 1)))
      (setq datadog--looking-at-query (+ datadog--looking-at-query 1))
      (datadog-metric-query (elt datadog--all-queries
                                 datadog--looking-at-query)))
     ((or (> offset 1) (< offset -1))
      ;; FIXME: uh, don't throw an error here.
      (error "Can only move one spot for now!"))
     (t (message (if (< next-idx 0) "Beginning of series" "End of series"))))
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
  (when datadog--active-query
    (let ((current-idx datadog--looking-at-series))
      (datadog-metric-query datadog--active-query t)
      ;; stay on current graph index if possible
      (when (> (length datadog--current-result) current-idx)
        (setq datadog--looking-at-series current-idx))
      (datadog--render-graph))))

(defun datadog-quit ()
  (interactive)
  (kill-buffer (current-buffer)))

;; Graph drawing variables and routines

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

(defun datadog--check-size-change (frame)
  (let ((last-size datadog--graph-size))
    (datadog--set-graph-size)
    (when (not (equal last-size datadog--graph-size))
      (datadog-refresh))))

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

(defun datadog--set-title (title line-num)
  (let* ((buffer-read-only nil)
         (w (window-width))
         (offset (max 0 (/ (- w (length title)) 2)))
         (title (if (> (length title) w)
                    (substring title 0 w)
                  title)))
    (when (>= (datadog--get-graph-dim 'margin-top) line-num)
      (save-excursion
        (goto-line line-num) ;; maybe don't hardcode this
        (when (not (looking-at "^$"))
          (kill-line))
        (insert-char ?\s offset)
        (insert title)
        (put-text-property (- (point) (length title)) (point)
                           'face 'datadog-chart-title)))
    ))

(defun datadog--set-tile-title (title)
  (datadog--set-title title 1))

(defun datadog--set-graph-title (title)
  (datadog--set-title title 2))

;; Graphing face definitions
(defface datadog-chart-area
  '((((class color) (min-colors 8))
     :foreground "blue"
     :background "blue")
    (t :inverse-video t))
  "Color for showing chart area"
  :group 'datadog-faces)

;; FIXME: uh, black might not be the best default foreground
(defface datadog-chart-title
  '((((class color) (min-colors 8) (background dark))
     :foreground "green")
    (t :foreground "black"))
  "Color for showing chart title"
  :group 'datadog-faces)

(defface datadog-chart-label
  '((((class color) (min-colors 8) (background dark))
     :foreground "yellow")
    ('t :foreground "black"))
  "Color for time and value labels on graph"
  :group 'datadog-faces)

(defun datadog--render-graph ()

  (datadog--reset-graph)

  (let* ((buffer-read-only nil))

    (when datadog--active-tile
      (datadog--set-tile-title (cdr (assoc 'title
                                           datadog--active-tile))))
    (if (> (length datadog--current-result) 0)
        ;; this is where the magic happens
        (let* ((series (elt datadog--current-result datadog--looking-at-series))
               (points (datadog--filter-points (cdr (assoc 'pointlist series))))
               (extent (datadog--series-extent points))
               (ymin (min (car extent) 0.0))
               (ymax (cdr extent))
               (yrange (if (> (- ymax ymin) 0)
                           (- ymax ymin)
                         (max ymax 1)))
               (scaled (mapcar (lambda (p)
                                 (cons (datadog--scale-t (elt p 0))
                                       (datadog--scale-y (elt p 1)
                                                         ymin
                                                         yrange)))
                               points)))

          ;; Set the title only if we have a real series
          (datadog--set-graph-title (cdr (assoc 'expression series)))
          ;; and only then render the graph
          (goto-char (point-min))
          (forward-line (datadog--get-graph-dim 'margin-top))


          (while (< (line-number-at-pos) (cdr datadog--graph-origin))
            (forward-char (+ (car datadog--graph-origin) 1))
            (dolist (sc scaled)
              (insert-char ?\s (- (car sc) (current-column)))
              (if (> (line-number-at-pos) (cdr sc))
                  (progn
                    (insert-char ?#)
                    (put-text-property (- (point) 1) (point)
                                       'face 'datadog-chart-area))
                (insert-char ?\s)))
            (forward-line 1))
          ;; only draw y-axis ticks with data
          (datadog--draw-y-ticks ymin ymax yrange))
      (datadog--set-graph-title (concat "No data: " datadog--active-query)))

  ;; but always draw t-ticks
  (datadog--draw-t-ticks)
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
  '((one-hour . 900)
    (four-hours . 3600)
    (one-day . 21600)
    (one-week . 172800)))

(defconst datadog--format-for-timeframe
  '((one-hour . "%H:%M")
    (four-hours . "%H:%M")
    (one-day . "%H:%M")
    (one-week . "%b %d")))

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
              (put-text-property (- (point) (length time)) (point)
                                 'face 'datadog-chart-label)
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
              (insert text)
              (put-text-property (- (point) (length text)) (point)
                                 'face 'datadog-chart-label)
              (insert-char ?-))))
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

;; Dash selection interface

(defvar datadog--current-dash-id nil)

(defvar helm-source-datadog-dash-list
  '((name . "Dash List")
    (candidates . datadog--make-dash-list)
    (action . (("Select Tile" . datadog-select-tile)))))

(defun datadog--fetch-dash-list ()
  (if (or (null datadog--dash-list)
          (> (- (datadog--utc-now) (car datadog--dash-list))
             datadog--dash-list-refresh-time))
      (let ((dash-list (dogapi-dash-list)))
        (setq datadog--dash-list (cons (datadog--utc-now) dash-list))
        dash-list)
    (cdr datadog--dash-list)))

(defun datadog--make-dash-list ()
  (let ((dash-list (datadog--fetch-dash-list)))
    (mapcar (lambda (d)
              (cons (format "%s (%s)"
                            (cdr (assoc 'title d))
                            (cdr (assoc 'description d)))
                    (string-to-number (cdr (assoc 'id d)))))
            (cdr (assoc 'dashes dash-list)))))

(defun datadog-select-dash ()
  (interactive)
  (helm :sources '(helm-source-datadog-dash-list)))

(defvar helm-source-datadog-select-tile
  '((name . "Select Tile")
    (candidates . datadog--dash-tiles)
    (action . (("Show Tile" . datadog-render-tile)))))

(defun datadog--get-dash-graphs (dash-object)
  (cdr (assoc 'graphs (cdr (assoc 'dash dash-object)))))

(defun datadog--queries-from-tile (tile)
  (let ((tile-def (cdr (assoc 'definition tile))))
    (mapcar (lambda (x) (cdr (assoc 'q x)))
            (cdr (assoc 'requests tile-def)))))

(defun datadog--fetch-dash (dash-id)
  (when (or (not datadog--dash-tiles)
            (not (= (car datadog--dash-tiles) dash-id)))
    (setq datadog--dash-tiles (cons dash-id
                                    (dogapi-dash dash-id))))
  (cdr datadog--dash-tiles))

(defun datadog--dash-tiles ()
  (let ((dash (datadog--fetch-dash datadog--current-dash-id)))
    (mapcar (lambda (tile)
              (let ((tile-title (cdr (assoc 'title tile))))
                (cons tile-title
                      (list (cons 'title tile-title)
                            (cons 'queries (datadog--queries-from-tile tile)))
                      )))
            (datadog--get-dash-graphs dash))))

(defun datadog-select-tile (&optional dash-id)
  (interactive)
  (when dash-id
    (setq datadog--current-dash-id dash-id))
  (if datadog--current-dash-id
      (helm :sources '(helm-source-datadog-select-tile))
    (error "No dash currently selected")))

(defun datadog-render-tile (tile)
  (setq datadog--active-tile tile)
  (setq datadog--looking-at-query 0)
  (let ((queries (cdr (assoc 'queries tile))))
    (when queries
      (setq datadog--all-queries queries)
      (datadog-metric-query (car queries)))))

;; Timeframe controls

(defun datadog-timeframe-one-hour ()
  (interactive)
  (datadog--set-timeframe 'one-hour))

(defun datadog-timeframe-four-hours ()
  (interactive)
  (datadog--set-timeframe 'four-hours))

(defun datadog-timeframe-one-day ()
  (interactive)
  (datadog--set-timeframe 'one-day))

(defun datadog-timeframe-one-week ()
  (interactive)
  (datadog--set-timeframe 'one-week))

(defun datadog--set-timeframe (timeframe)
  (let ((old-timeframe datadog--timeframe))
    (when (not (eq old-timeframe timeframe))
      (setq datadog--timeframe timeframe)
      ;; for now, this invalidates our query cache
      (clrhash datadog--query-cache)
      (datadog-refresh))))

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
    (define-key map (kbd "m") 'datadog-explore-metric)
    (define-key map (kbd "n") 'datadog-next-series)
    (define-key map (kbd "p") 'datadog-previous-series)
    (define-key map (kbd "f") 'datadog-go-forward)
    (define-key map (kbd "b") 'datadog-go-backward)
    (define-key map (kbd "r") 'datadog-refresh)
    (define-key map (kbd "D") 'datadog-select-dash)
    (define-key map (kbd "T") 'datadog-select-tile)

    ;; timeframes
    (define-key map (kbd "1") 'datadog-timeframe-one-hour)
    (define-key map (kbd "4") 'datadog-timeframe-four-hours)
    (define-key map (kbd "d") 'datadog-timeframe-one-day)
    (define-key map (kbd "w") 'datadog-timeframe-one-week)
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
  (make-local-variable 'datadog--active-tile)
  (make-local-variable 'datadog--all-queries)
  (make-local-variable 'datadog--active-query)
  (make-local-variable 'datadog--current-result)
  (make-local-variable 'datadog--timeframe)
  (make-local-variable 'datadog--series-data)
  (make-local-variable 'datadog--graph-size)
  (make-local-variable 'datadog--graph-origin)
  (make-local-variable 'datadog--dash-list)
  (make-local-variable 'datadog--query-cache)

  ;; variable setup
  (setq show-trailing-whitespace nil)
  ;; setup caches
  ;; note strings as keys for the query
  (setq datadog--query-cache (make-hash-table :test 'equal))

  ;; hook setup
  (add-hook 'window-size-change-functions 'datadog--check-size-change)

  ;; inital display
  (datadog--reset-graph))

(provide 'datadog)
