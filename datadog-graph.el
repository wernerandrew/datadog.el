;;; datadog-graph.el --- graph helpers for datadog-mode

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

;; Commentary: Does the actual work of rendering graphs and handling
;; navigation.  Meant to be as stateless as possible (except we do save
;; the current series data)

(defvar-local datadog--series-data nil
  "Holds the current series data")

(defvar-local datadog--active-query nil
  "Stores the active query currently being navigated")

(defvar-local datadog--active-interval nil)

(defvar-local datadog--active-from-ts nil)

(defvar-local datadog--active-to-ts nil)

(defconst datadog--dim-names
  '(margin-below margin-left margin-top width height)
  "Names we use to refer to the graph size factors,
defined in `datadog--graph-sizes'")

(defconst datadog--graph-sizes
  '((0 0 0 40 8)
    (0 0 0 40 16)
    (2 2 3 40 16)
    (3 4 4 60 20)
    (3 4 4 60 30)
    (3 4 4 60 40)
    (3 4 4 120 40)))

(defvar-local datadog--graph-size nil)

(defvar-local datadog--graph-origin nil
  "(column, row) pair")

;; Timeframe constants and state variables

(defvar-local datadog--timeframe 'one-hour
  "Supports values described in `datadog--time-spans'.")

(defconst datadog--time-spans
  '((one-hour . 3600)
    (four-hours . 14400)
    (one-day . 86400)
    (one-week . 604800)))

;; Faces we use when graphing

(defface datadog-chart-area
  '((((class color) (min-colors 8))
     :foreground "blue"
     :background "blue")
    (t :inherit 'default :inverse-video t))
  "Color for showing chart area"
  :group 'datadog-faces)

(defface datadog-chart-title
  '((((class color) (min-colors 8))
     :foreground "green")
    (t :inherit 'default))
  "Color for showing chart title"
  :group 'datadog-faces)

(defface datadog-chart-label
  '((((class color) (min-colors 8) (background dark))
     :foreground "yellow")
    (t :inherit 'default))
  "Color for time and value labels on graph"
  :group 'datadog-faces)

(defface datadog-chart-highlight-bar
  '((((class color) (min-colors 8))
     :foreground "magenta"
     :background "magenta")
    (t :inherit 'default :inverse-video nil))
  "Face for bars when selected by timecursor"
  :group 'datadog-faces)

(defface datadog-chart-timecursor
  '((((class color) (min-colors 8) (background dark))
     :foreground "white"
     :background "white")
    (t :inverse-video t))
  "Face for chart area with timecursor"
  :group 'datadog-faces)

(defface datadog-chart-tooltip
  '((t :inverse-video t))
  "Face for value shown at timecursor"
  :group 'datadog-faces)

;; Shortcut for inserting fonts with a given face

(defun datadog--set-face (face-name start &optional end)
  "Set text face at start, or optionally from start to end"
  (save-excursion
    (let ((end (or end (+ start 1))))
      (put-text-property start end 'face face-name))))

(defun datadog--insert-face (text face-name)
  (let* ((start (point))
         (end (+ start (length text))))
    (insert text)
    (put-text-property start end 'face face-name)))

;; maybe could have done this as a bunch of alists,
;; but I'm lazy (and maybe stupid)

(defun datadog--get-graph-dim (dim-name &optional dims)
  "If dims not provided, defaults to datadog--graph-size"
  ;; special case for one week
  (defun week-adjusted-width (w)
    "Helper that finds the largest number of points that still
divides the week nicely.  Currently just finds the largest multiple
of 28 (7*4) <= n"
    (- w (% w 28)))
  ;; helper to search the list
  (defun find-dim (dims dim-name names-list)
    (when dims
      (if (eq dim-name (car names-list))
          (car dims)
        (find-dim (cdr dims) dim-name (cdr names-list)))))
  (let* ((sz (or dims datadog--graph-size))
         (dim (find-dim sz dim-name datadog--dim-names)))
    ;; special handling for one week
    (if (and (equal dim-name 'width)
             (equal datadog--timeframe 'one-week))
        (week-adjusted-width dim)
      dim)))

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
           (extra-height (- (window-height) (cdr extent)))
           (extra-width (- (window-width) (car extent))))
      (setq datadog--graph-origin
            (cons (+ (/ extra-width 2)
                     (datadog--get-graph-dim 'margin-left))
                  (+ (/ extra-height 2)
                     (- (cdr extent)
                        (datadog--get-graph-dim 'margin-below))))))))

(defun datadog--clear-chart-area ()
  (interactive)
  (let ((buffer-read-only nil))
    (erase-buffer)))

(defun datadog--get-graph-start-line ()
  "Little helper to tell us the first line on which to
start rendering graph items."
  (- (cdr datadog--graph-origin)
     (+ (datadog--get-graph-dim 'height)
        (datadog--get-graph-dim 'margin-top))))

(defun datadog--draw-axes ()
  "Draw the axes for the graph, without tick marks or any
included graph data.  Requires `datadog--graph-origin' to
be defined, as well as `datadog--graph-size'."
  (interactive)
  (let* ((buffer-read-only nil)
         (graph-base (cdr datadog--graph-origin))
         (offset (car datadog--graph-origin))
         (graph-max-col (+ 1 offset (datadog--get-graph-dim 'width)))
         (axes-start-line (+ (datadog--get-graph-start-line)
                             (datadog--get-graph-dim 'margin-top))))

    (save-excursion
      (goto-char (point-min))
      (newline axes-start-line)
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
  "Finds the correct column for a given time, expressed
in absolute coordinates."
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
                  points))))

(defun datadog--set-title (title line-num)
  "Where line number has a minimum value of 0, and the actual line
is calculated with regard to the known graph dimensions"
  (let* ((buffer-read-only nil)
         (w (window-width))
         (h-offset (max 0 (/ (- w (length title)) 2)))
         (v-offset (datadog--get-graph-start-line))
         (title (if (> (length title) w)
                    (substring title 0 w)
                  title)))

    (when (> (datadog--get-graph-dim 'margin-top) line-num)
      (save-excursion
        (goto-line (+ v-offset line-num))
        (while (not (looking-at "$"))
          (delete-char 1))
        (insert-char ?\s h-offset)
        (datadog--insert-face title 'datadog-chart-title)))))

(defun datadog--set-tile-title (title)
  (datadog--set-title title 0))

(defun datadog--set-graph-title (title)
  (datadog--set-title title 1))


(defun datadog--render-graph (&optional series tile-title ymin ymax)
  "All graph redrawing happens through here.  Requires a series,
plus an optional tile title and range.  If no series provided,
defaults to value cached in `datadog--series-data'."
  (datadog--reset-graph)

  (let* ((buffer-read-only nil))

    (when tile-title (datadog--set-tile-title tile-title))

    (if series
        ;; this is where the magic happens
        (let* ((series (or series datadog--series-data))
               (points (datadog--filter-points (cdr (assoc 'pointlist series))))
               (extent (datadog--series-extent points))
               (ymin (or ymin
                         (min (car extent) 0.0)))
               (ymax (or ymax
                         (cdr extent)))
               (yrange (if (> (- ymax ymin) 0)
                           (- ymax ymin)
                         (max ymax 1)))
               (scaled (mapcar (lambda (p)
                                 (cons (datadog--scale-t (elt p 0))
                                       (datadog--scale-y (elt p 1)
                                                         ymin
                                                         yrange)))
                               points)))

          ;; Update the series data state variable
          (setq datadog--series-data series)
          ;; Set the title only if we have a real series
          (datadog--set-graph-title (cdr (assoc 'expression series)))
          ;; and only then render the graph
          (goto-char (point-min))
          (forward-line (+ (datadog--get-graph-start-line)
                           (datadog--get-graph-dim 'margin-top)))


          (while (< (line-number-at-pos) (cdr datadog--graph-origin))
            (forward-char (+ (car datadog--graph-origin) 1))
            (dolist (sc scaled)
              (insert-char ?\s (- (car sc) (current-column)))
              (if (> (line-number-at-pos) (cdr sc))
                  (datadog--insert-face "#" 'datadog-chart-area)
                (insert-char ?\s)))
            (forward-line 1))
          ;; only draw y-axis ticks with data
          (datadog--draw-y-ticks ymin ymax yrange)
          ;; try to keep the last timecursor, if possible,
          ;; by trying to use a bounds-checked version of it.
          ;; otherwise default to the to current to-timestamp
          (datadog--timecursor (or datadog--timecursor-at
                                   datadog--active-to-ts)))
          ;; store our series for future use
      (datadog--set-graph-title (concat "No data: " datadog--active-query)))

  ;; always draw t-ticks
  (datadog--draw-t-ticks)
  ;; and end up in some vaguely sensible place
  (goto-char (point-min))))

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
  (while (not (looking-at "$"))
    (delete-char 1))
  ;; this is kinda ugly, but save-excursion didn't work
  ;; for whatever reason
  (previous-line)
  (beginning-of-line))

;; Timecursor routines

(defun datadog--set-time-params (from-ts to-ts interval)
  "from-ts, to-ts, and interval should all be integers."
  (setq datadog--active-interval interval)
  (setq datadog--active-from-ts
        (datadog--closest-below from-ts interval))
  (setq datadog--active-to-ts
        (datadog--closest-below to-ts interval)))

(defvar datadog--timecursor-at nil
  "Current time being pointed at")

(defun datadog--closest-t (timestamp)
  "Expects an epoch timestamp"
  (let* ((series datadog--series-data)
         (points (cdr (assoc 'pointlist series)))
         (i 0)
         (out-ts nil))
    (when points
      (let ((max-ts (truncate (/ (apply 'max
                                        (mapcar (lambda (p) (elt p 0))
                                                (datadog--filter-points points)))
                                 1000))))
        (cond ((< timestamp datadog--active-from-ts) datadog--active-from-ts)
              ((> timestamp max-ts) max-ts)
              (t (datadog--closest-below timestamp
                                         datadog--active-interval)))))))

(defun datadog--timecursor (timestamp)
  "Internal drawing routine to the timecursor at a given
time, subject to a bounds check.  Expects an integer
epoch, or nil to simply flag the timecursor as off."
  ;; Formatting helper
  (defun format-column (bar-face blank-face col-num start-row end-row)
    (goto-line start-row)
    (beginning-of-line)
    (forward-char 1) ;; FIXME: why do I need this?

    (while (< (line-number-at-pos) end-row)
      (forward-char col-num)
      (if (looking-at "[[:space:]]")
          (datadog--set-face blank-face (point))
        (datadog--set-face bar-face (point)))
      (forward-line 1)))

  ;; Set the global timestamp value if within graph bounds
  ;; or if null (which turns it off, in effect
  (if (null timestamp)
      ;; FIXME: maybe more formal cleanup here, though
      ;; currently this works
      (setq datadog--timecursor-at timestamp)
    (let* ((buffer-read-only nil)
           (new-ts (datadog--closest-t timestamp))
           (old-ts datadog--timecursor-at)
           (old-col-num (and old-ts (datadog--scale-t old-ts t)))
           (new-col-num (and new-ts (datadog--scale-t new-ts t)))
           (graph-end (cdr datadog--graph-origin))
           (graph-start (- graph-end (datadog--get-graph-dim 'height))))

      ;; note that old column number *could* be out of range
      (when (and old-col-num (>= old-col-num 0))
        (format-column 'datadog-chart-area nil
                       old-col-num graph-start graph-end))
      (when new-col-num
        (format-column 'datadog-chart-highlight-bar 'datadog-chart-timecursor
                       new-col-num graph-start graph-end))

      (datadog--draw-tooltip
       (datadog--format-number (datadog--value-at new-ts))
       new-col-num)
      ;; and update so we know what to erase next time
      (setq datadog--timecursor-at new-ts)))
  ;; and reset to sompelace sensiblei
  (goto-char (point-min)))

;; Helper routines called by UI layer

(defun datadog--shift-timecursor (delta)
  "Convenience function for bumping the timecursor
position in a certain direction"
  (datadog--timecursor (+ datadog--timecursor-at
                          (* delta datadog--active-interval))))

(defun datadog--shift-by-tick (num-secs num-ticks)
  "Move cursor by num-secs and num-ticks, and then snap to
next lowest tick.  Helper function."
  (interactive)
  (let ((tick-size (cdr (assoc datadog--timeframe
                               datadog--ticks-for-timeframe))))
    (when datadog--timecursor-at
      (datadog--timecursor
       (datadog--closest-below (+ datadog--timecursor-at
                                  num-secs
                                  (* num-ticks tick-size))
                               tick-size)))))

(defun datadog--draw-tooltip (value column)
  ;; need at least 3 lines of margin to show tooltip
  (when (>= (datadog--get-graph-dim 'margin-top) 3)
    (let* ((buffer-read-only nil)
           (n-chars (length value))
           (w (window-width))
           (start-col (- column n-chars))
           ;; start line goes immediately above graph
           (start-line (- (cdr datadog--graph-origin)
                          ;; FIXME: why do I need +1 here???
                          (+ 1 (datadog--get-graph-dim 'height)))))
      (goto-line start-line)
      (beginning-of-line)
      (while (not (looking-at "$"))
        (delete-char 1))
      (insert-char ?\s start-col)
      (datadog--insert-face value 'datadog-chart-tooltip))))

(defun datadog--value-at (timestamp &optional js-time)
  "Get the value of the current series at the given timestamp.
If js-time is omitted or nil, assumes that timestamp is given in
seconds.  Otherwise, it's in milliseconds."
  (let* ((ts (if js-time timestamp (* timestamp 1000)))
         (points (cdr (assoc 'pointlist datadog--series-data)))
         (num-points (length points))
         (current 0)
         (result nil))

    (while (and (< current num-points)
                (not result))
      (let ((current-point (elt points current)))
        (when (= ts (elt current-point 0))
          (setq result (elt current-point 1)))
        (setq current (+ current 1))))
    result))

;; Scaling and tick formatting

;; Time axis formatting

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


(defun datadog--set-current-timeframe (timeframe)
  "A modest interface for setting the timeframe"
  (setq datadog--timeframe timeframe))

(defun datadog--get-current-timeframe ()
  "Again, just because we don't want clients accessing private vars."
  datadog--timeframe)

(defun datadog--current-time-window ()
  (cdr (assoc datadog--timeframe
              datadog--time-spans)))

(defun datadog--set-time-params (from-ts to-ts interval)
  "Helper to set time range information needed to successfully
render the graph."
  (setq datadog--active-interval interval)
  (setq datadog--active-from-ts
        (datadog--closest-below from-utc interval))
  (setq datadog--active-to-ts
        (datadog--closest-below to-utc interval)))

(defun datadog--closest-below (ts interval)
  (- ts (% ts interval)))

(defun datadog--time-axis-ticks (start end interval tick-size)
  (let* ((current (datadog--closest-below end tick-size))
         (ticks nil))
    (while (> current start)
      (setq ticks (cons (datadog--closest-below current interval) ticks))
      (setq current (- current tick-size)))
    ticks))

(defun datadog--draw-t-ticks ()
  "Helper to draw time axis ticks.  Should only be called from
`datadog--render-graph'."
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
              (datadog--insert-face time 'datadog-chart-label)
              (setq last-time-column (current-column)))))))))

;; Y-axis formatting helpers

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
             (t 2.0)))))

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
    ticks))

(defun datadog--draw-y-ticks (ymin ymax yrange)
  "Helper function to draw ticks and values on the y-axis.
Should only be called from `datadog--render-graph'."
  (let* ((ticks (datadog--y-axis-ticks ymin ymax))
         (axis-offset (car datadog--graph-origin))
         (axis-line (cdr datadog--graph-origin))
         (min-tick-spacing 2)
         (last-tick (+ axis-line min-tick-spacing 1))
         (graph-top (datadog--get-graph-start-line)))

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
              (datadog--insert-face text 'datadog-chart-label)
              (insert-char ?-))))))))

;; Number formatting helpers

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
  (if (null x) ;; what if, like, this isn't even a number, maaan
      "NaN"
    (let* ((scale-data (scale-number x 1.0 "" datadog--number-scale))
           (unit (car scale-data))
           (val (cdr scale-data))
           (precision (if (= (length unit) 0) 3 datadog--precision))
           (string-val (truncate-to-precision val precision)))

      (if (= (length unit) 0)
          (strip-trailing-zeros string-val)
        (concat string-val unit)))))


(provide 'datadog-graph)
