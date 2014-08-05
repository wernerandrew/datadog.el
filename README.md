datadog.el
==========

A tool to browse Datadog dashboards from the safety and comfort of
an Emacs buffer.

### Requirements

datadog.el requires:

* Emacs 24 (preferably 24.3 or later), and
* Helm, a popular interactive navigation framework.

Helm is available through `package.el`, and can be installed through
`M-x package-list-packages` or otherwise via the `package.el` API.
Additional details on Helm, including further notes on installation,
can be found [here](https://github.com/emacs-helm/helm).

### Installation

Once the dependencies are installed, you can clone `datadog.el` into
an appropriate directory, e.g., `~/emacs.d`:

```shell
$ cd ~/.emacs.d
$ git clone git@github.com:wernerandrew/datadog.el.git
```

You can then add the following lines to your initialization file:

```lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/datadog.el"))
(require 'datadog)
```

### Use

The following discussion assumes to the default keybindings.
Capital letters are meant to refer to the letter with the shift
key held down; that is, `D` means "shift-D".
A full lists of available functions can be found in the mode-specific
help (`h`, `C-h m`, or `M-x describe-mode`, as you prefer).

#### Browsing Dashboards

Press `D` (`datadog-select-dash`) to bring up a Helm buffer with a list
of dashes. Selecting a dash sets it as the current active dash and brings
up a list of tiles. Selecting a tile will bring up a graph.

#### Executing Metric Queries

Press `m` (`datadog-explore-metric`) to enter a metric query string
in the minibuffer.

#### Navigating Graphs

Because of the limitations of the text display, datadog.el supports only
a simplified version of Datadog graph visualizations. All time series
returned from the server are represented as area charts, and given the
low resoluion, stacking is not supported. When multiple series are
returned in response to a query, the series can be viewed separately.
Use `n` (`datadog-next-series`) and `p` (`datadog-previous-series`)
to switch between adjacent series.
Pressing `j` (`datadog-jump-to-graph`) brings up an interactive window
where you can skip to a particular scope.

Graphs have a timecursor, indicated by a vertical line. The cursor
can be moved by steps with `f` (`datadog-timecursor-forward`)
and `b` (`datadog-timecursor-backward`), or by jumps with `F`
(`datadog-forward-tick`) and `B` (`datadog-backward-tick`).

You can select the graph timeframe by pressing `1` (one hour),
`4` (four hours), `d` (one day), or `w` (one week)

#### Quitting

Pressing `q` hides the `*datadog*` buffer but does not kill it.
To kill it, kill as you would nay other buffer.
