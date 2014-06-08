;;; datadog.el --- Emacs major mode for Datadog

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

;; Package-Requires: helm
;; URL: http://github.com/wernerandrew/datadog.el
;; Version: 0.0.1alpha

;; Commentary: A simple interactive console for browsing Datadog
;; dashes and performing metric queries.  Graphs are all rendered
;; as time series, and for ease of rendering, only a limited set of
;; time frames are supported.

(require 'helm)
(require 'dogapi)
(require 'datadog-graph)

;; User customizable variables: none at this time (though see dogapi)

;; Internal variables and functions
;; Time utilities

(defun datadog--utc-now (&optional offset-secs)
  "Get seconds since EPOCH as an integer, with optional offset"
  (let ((ts (truncate (float-time))))
    (if offset-secs (+ ts offset-secs) ts)))

(defun datadog--to-js-time (timestamp)
  "Our API expects string timestamps in milliseconds.  Who are we to argue?"
  (number-to-string (* 1000 timestamp)))

;; Buffer state variables

(defvar-local datadog--active-tile nil
  "A (title . graphs) cons cell for the active tile.")

(defvar-local datadog--all-queries nil
  "A list of all queries that are navigable via
`datadog-next-series' and `datadog-previous-series'.
Most commonly, this is all queries within a graph tile.")

(defvar-local datadog--current-result nil
  "Stores the active result as a list of (scope . pointlist)")

(defvar-local datadog--looking-at-query 0
  "The index of the query currently being browsed")

(defvar-local datadog--looking-at-series 0
  "The current series within the current query")

;; Various refresh / caching time limits

(defconst datadog--query-refresh-time
  '((one-hour . 60)
    (four-hours . 300)
    (one-day . 1800)
    (one-week . 3600)))

(defconst datadog--dash-list-refresh-time 300)

(defconst datadog--dash-refresh-time 300)

;; Cache variables

(defvar-local datadog--dash-list nil
  "Stored list of dashes so we don't always hit the server")

(defvar-local datadog--query-cache nil
  "Maps query string to (last-update-utc . result)")

(defvar-local datadog--dash-tiles nil
  "Stores tile definitions for current dash")

;; Series retrieval commands

(defun datadog-metric-query (query &optional skip-render)
  "It's a metric query, kids."
  (interactive)
  (let* ((offset (datadog--current-time-window))
         (to-utc (datadog--utc-now))
         (from-utc (- to-utc offset))
         (interval (datadog--rollup-interval))
         (result (datadog--fetch-series query interval from-utc to-utc)))
    (when result
      (message "Retrieved series")
      (setq datadog--active-query query)
      (setq datadog--current-result result)
      (setq datadog--looking-at-series 0)
      (datadog--set-time-params from-utc to-utc interval)
      (unless skip-render
        (datadog--render-metric-graph)))))

(defun datadog-explore-metric (query)
  "Query exactly one metric."
  (interactive  "sEnter metric query: ")
  (setq datadog--all-queries (list query))
  (setq datadog--active-tile nil)
  (setq datadog--looking-at-query 0)
  (datadog-metric-query query))

;; Series retrieval helpers

(defun datadog--fetch-series (query interval from-utc to-utc)
  "Get the query, but check the cache.  Also hacks in the correct
rollup interval."
  (let* ((query-str (datadog--make-query query interval))
         (cached-result (gethash query-str datadog--query-cache)))
    (if (or (not cached-result)
            (> (- to-utc (car cached-result))
               (cdr (assoc (datadog--get-current-timeframe)
                           datadog--query-refresh-time))))
        (let ((result (dogapi-metric-query query-str
                                           (datadog--to-js-time from-utc)
                                           (datadog--to-js-time to-utc))))
          (puthash query-str (cons to-utc result) datadog--query-cache)
          result)
      (cdr cached-result))))

;; Functions that like, you know, map to series
(defconst datadog--basic-rollup-hack
  '(("}$" . "}.rollup(%d)")
    ( "\\.rollup(\\([^0-9)]*\\)[^)]*)" . ".rollup(\\1,%d)")
    ("}[ ]*\\." . "}.rollup(%d).")
    ("}[ ]*)" . "}.rollup(%d))"))
  "Kid tested. Mother approved.")

(defconst datadog--multi-series-hack
  '(("}[ ]*,[ ]*" . "}.rollup(%d),")))

(defconst datadog--series-arithmetic-hack
  '(( "}[ ]*\\([\\+/\\*\\-]\\)" . "}.rollup(%d) \\1 ")))

(defun datadog--make-query (query interval)
  "Does some regex substitutions to ensure a proper interval
Hopefully to go the way of the dodo when / if we support
setting the interval directly through API requests."
  (defun maybe-substitute (original-query patterns)
    (if patterns
        (let ((pat (caar patterns))
              (subst (format (cdar patterns) interval)))
          (if (string-match pat original-query)
              (replace-regexp-in-string pat subst original-query)
            (maybe-substitute original-query (cdr patterns))))
      original-query))
  (let ((new-query query))
    (setq new-query (maybe-substitute new-query
                                      datadog--basic-rollup-hack))
    (setq new-query (maybe-substitute new-query
                                      datadog--multi-series-hack))
    (setq new-query (maybe-substitute new-query
                                      datadog--series-arithmetic-hack))))

(defun datadog--rollup-interval ()
  (let* ((time-window (datadog--current-time-window))
         (num-points (datadog--get-graph-dim 'width)))
    ;; One week requires special work to get "nice" rollups
    (/ time-window num-points)))

;; Navigation commands

(defun datadog-next-series ()
  (interactive)
  (datadog--checked-nav 1))

(defun datadog-previous-series ()
  (interactive)
  (datadog--checked-nav -1))

(defun datadog--checked-nav (offset)
  "Helper to navigate between graphs in a tile.  Handles switching
between different metric queries for the same tile, hence some of
the complexity.  Currently, only supports values of 1 or -1 for offset;
for bigger jumps, the best bet currently is something like
`datadog-jump-to-graph'."
  (let ((next-idx (+ datadog--looking-at-series offset))
        (current-len (length datadog--current-result)))
    (cond
     ;; easy case: we can stay on this graph
     ((and (< next-idx current-len)
           (>= next-idx 0))
      (setq datadog--looking-at-series next-idx)
      (datadog--render-metric-graph))
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
        (datadog--render-metric-graph)))
     ;; or, we're at the end, and can go forwards
     ((and (= next-idx 1)
           (< datadog--looking-at-query (- (length datadog--all-queries) 1)))
      (setq datadog--looking-at-query (+ datadog--looking-at-query 1))
      (datadog-metric-query (elt datadog--all-queries
                                 datadog--looking-at-query)))
     ((or (> offset 1) (< offset -1))
      ;; FIXME: uh, don't throw an error here.
      (error "Can only move one spot for now!"))
     (t (message (if (< next-idx 0) "Beginning of series" "End of series"))))))

(defun datadog-timecursor-forward ()
  (interactive)
  (datadog--shift-timecursor 1))

(defun datadog-timecursor-backward ()
  (interactive)
  (datadog--shift-timecursor -1))

(defun datadog-forward-tick ()
  (interactive)
  (datadog--shift-by-tick 0 1))

(defun datadog-backward-tick ()
  (interactive)
  (datadog--shift-by-tick -1 0))

(defun datadog--make-graph-list ()
  "Creates a list of graphs, where the format depends slightly
on the composition of the graphs on the tile."
  ;; still experimenting with what's best
  (defun format-scope (s)
    (cdr (assoc 'scope s)))

  (defun format-scope-and-metric (s)
    (format "%s (%s)"
            (cdr (assoc 'scope s))
            (cdr (assoc 'metric s))))

  (let ((format-func 'format-scope)
        (metric-names nil)
        (i -1))
    ;; use different format strategy if metrics aren't unique
    (mapcar (lambda (s)
              (add-to-list 'metric-names (cdr (assoc 'metric s))))
            datadog--current-result)
    (when (> (length metric-names) 1)
      (setq format-func 'format-scope-and-metric))
    (mapcar (lambda (s)
              (setq i (+ i 1))
              (cons (funcall format-func s) i))
            datadog--current-result)))

(defun datadog--jump-set-graph (graph-idx)
  "Jumps to a graph index and displays it.  Only works if you're
looking at a tile."
  (if datadog--active-tile
      (progn
        (setq datadog--looking-at-series graph-idx)
        (datadog--render-metric-graph))
    (error "Not currently looking at a tile")))

(defun datadog-jump-to-graph ()
  (interactive)
  (unless datadog--active-tile
    (error "Not currently looking at a tile"))
  (helm :sources
        ;; can't pass a function name, alas
        ;; due to scoping issues
        (list (cons 'name "Select Graph by Scope")
              (cons 'candidates (datadog--make-graph-list))
              (cons 'action
                    (list (cons "Select Graph" 'datadog--jump-set-graph))))))

;; Rendering helpers

(defun datadog--active-tile-title ()
  "Little helper to access the tile title, or otherwise return nil."
  (and datadog--active-tile
       (cdr (assoc 'title datadog--active-tile))))

(defun datadog--render-metric-graph ()
  "Helper to render the current graph when navigating a tile.
Relies on `datadog--looking-at-series' and `datadog--active-tile'
to be appropriately set."
  (if (< datadog--looking-at-series (length datadog--current-result))
      (datadog--render-graph
       (elt datadog--current-result datadog--looking-at-series)
       (datadog--active-tile-title))
    (datadog--render-graph
     nil
     (concat (datadog--active-tile-title)))))

;; Graph redrawing / refreshing commands

(defun datadog-refresh ()
  "Refreshes the graph display, querying the server for newer
data if necessary.  Query results are cached for a period that
varies based on the timeframe of the query."
  (interactive)
  (when datadog--active-query
    (let ((current-idx datadog--looking-at-series))
      ;; do the metric query but don't render
      (datadog-metric-query datadog--active-query t)
      ;; stay on current graph index if possible
      (when (> (length datadog--current-result) current-idx)
        (setq datadog--looking-at-series current-idx))
      (datadog--render-metric-graph))))

(defun datadog--check-size-change (frame)
  "Hook function for resizing."
  (let ((last-size datadog--graph-size))
    (datadog--set-graph-size)
    (when (not (equal last-size datadog--graph-size))
      (datadog-refresh))))

;; Dash and tile selection interface

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

(defun datadog--make-title (dash-info)
  (replace-regexp-in-string
   "[[:space:]]+"
   " "
   (format "%s (%s)"
           (cdr (assoc 'title d))
           (cdr (assoc 'description d)))))

(defun datadog--make-dash-list ()
  (let ((dash-list (datadog--fetch-dash-list)))
    (mapcar (lambda (d)
              (cons (datadog--make-title d)
                    (string-to-number (cdr (assoc 'id d)))))
            (cdr (assoc 'dashes dash-list)))))

(defun datadog-select-dash ()
  "Opens an interactive dialog to browse your list of Datadog dashes.
Selecting a dash will bring up a follow-up dialog to choose a tile."
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
  "Data source for the formatted list mapping tile names to
a list of queries."
  (let ((dash (datadog--fetch-dash datadog--current-dash-id)))
    (mapcar (lambda (tile)
              (let ((tile-title (cdr (assoc 'title tile))))
                (cons tile-title
                      (list (cons 'title tile-title)
                            (cons 'queries
                                  (datadog--queries-from-tile tile))))))
            (datadog--get-dash-graphs dash))))

(defun datadog-select-tile (&optional dash-id)
  "Command to choose a tile, if there's a currently selected dash."
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
  "Set the query timeframe to one hour"
  (interactive)
  (datadog--set-timeframe 'one-hour))

(defun datadog-timeframe-four-hours ()
  "Set the query timeframe to four hours"
  (interactive)
  (datadog--set-timeframe 'four-hours))

(defun datadog-timeframe-one-day ()
  "Set the query timeframe to one day"
  (interactive)
  (datadog--set-timeframe 'one-day))

(defun datadog-timeframe-one-week ()
  "Set the query timeframe to one week"
  (interactive)
  (datadog--set-timeframe 'one-week))

(defun datadog--set-timeframe (timeframe)
  "Helper function to change the timeframe."
  (let ((old-timeframe (datadog--get-current-timeframe)))
    (when (not (eq old-timeframe timeframe))
      (datadog--set-current-timeframe timeframe)
      ;; for now, this invalidates our query cache
      (clrhash datadog--query-cache)
      ;; and the timecursor position
      (datadog--timecursor nil)
      (datadog-refresh))))

;; Miscellaneous

(defconst datadog--splash-screen-text
  '("Welcome to"
    "______  _______ _______ _______ ______   _____   ______"
    "|     \\ |_____|    |    |_____| |     \\ |     | |  ____"
    "|_____/ |     |    |    |     | |_____/ |_____| |_____|"
    ""
    "Press `D' (shift-D) to browse dashes."
    "Press `m' to perform a metric query."
    "Select the timeframe with `1', `4', `d' or `w'."
    "C-h m for additional commands."))


(defun datadog--splash-screen ()
  "Currently displayed only on start"
  (let* ((buffer-read-only nil)
         (n-chars (apply 'max (mapcar 'length datadog--splash-screen-text)))
         (h-offset (/ (- (window-width) n-chars) 2))
         (v-offset (/ (- (window-height)
                         (length datadog--splash-screen-text))
                      2)))
    (erase-buffer)
    (if (and (>= h-offset 0)
               (>= v-offset 0))
        (progn
          (newline v-offset)
          (dolist (text datadog--splash-screen-text)
            (insert-char ?\s h-offset)
            (insert text)
            (newline)))
      ;; fall back to extremely simple text
      ;; should rarely be needed
      (insert "Welcome to Datadog. C-h m for help."))))

;; Main entry function

(defun datadog ()
  "Opens a Datadog session, or switches to an existing session."
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
    (define-key map (kbd "j") 'datadog-jump-to-graph)
    (define-key map (kbd "f") 'datadog-timecursor-forward)
    (define-key map (kbd "b") 'datadog-timecursor-backward)
    (define-key map (kbd "F") 'datadog-forward-tick)
    (define-key map (kbd "B") 'datadog-backward-tick)
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
  (datadog--init))

(defun datadog--init ()
  (interactive)

  ;; we won't highlight trailing whitespace
  (make-local-variable 'show-trailing-whitespace)
  (setq show-trailing-whitespace nil)

  ;; query cache; note strings as keys for the query
  (setq datadog--query-cache (make-hash-table :test 'equal))

  ;; hook setup
  (add-hook 'window-size-change-functions 'datadog--check-size-change)

  ;; initialize display
  (datadog--set-graph-size)
  (datadog--set-graph-origin)
  (datadog--splash-screen))

(provide 'datadog)
