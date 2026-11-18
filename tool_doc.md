# yml2stats Documentation

`yml2stats` is a tool for processing `.yml` benchmark result files and generating summary tables, matrices, and plots. It supports both standard verification results (Safe/Unsafe) and SMT solver results (Sat/Unsat).

## Usage

```bash
yml2stats [options] <input_file_or_directory>...
```

When a directory is provided, all `.yml` files within it are processed.

The tables and lists are printed on stdout, the warnings and other messages (`-v:N`) on stderr. An error in the arguments or inputs is printed on stderr with the exit code 1.

Several inputs are groups, e.g. categories of benchmarks, in the order given and named by the last part of their paths. Each group is compared on its own: merging, portfolios and the benchmarks run by all tools are per group. The tables and plots of the options below contain the runs of each tool over all groups, for a tool only the runs of its groups, with the benchmarks prefixed by their group (`GROUP/name`).

## Options

### Tables
- `-summary`: Simplified summary table (text). Columns: Tool, Sat, Unsat, Unknown, Total.
- `-summary-tex`: Simplified summary table (LaTeX).
- `-summary-ext`: Extended summary table (text). Splits unknowns into Timeout, Memout, Error, and Unknown. Memout covers out-of-memory errors of the tool and runs killed before the wall time limit with their peak memory close to the memory limit, when the log contains both values.
- `-summary-ext-tex`: Extended summary table (LaTeX).
- `-details`: Detailed per-benchmark results (text). Shows S/U/E/? status for each benchmark.
- `-details-tex`: Detailed per-benchmark results (LaTeX).
- `-matrix`: Comparative matrix (text). Shows Benchmarks solved by Row tool but not Column tool.
- `-matrixtex`: Comparative matrix (LaTeX).
- `-matrix-diff`: The benchmarks of each cell of `-matrix`: per ordered pair of tools, the benchmarks solved by the first and not by the second, with the result of the second.

### Tables over groups
One row per group and a row `Total`, one column per tool; the columns `Group` and `Benchmarks` (keys `group`, `benchmarks`) contain the name and the number of benchmarks of the group. A cell contains sat/unsat benchmarks solved with a correct verdict, `-` (LaTeX: `--`) for a tool without runs in the group. In LaTeX, sat and unsat are two columns per tool.
- `-overview`: Solved benchmarks per group and tool (text).
- `-overview-tex`: The same in LaTeX, the best sat and the best unsat value of each group in bold.
- `-no-best:TOOL1,TOOL2,...`: Leaves these tools, e.g. portfolios, out of the best values of `-overview-tex`.
- `-unique`: Uniquely solved benchmarks per group and tool (text): solved by the tool and by no other tool. A portfolio is compared with the tools that are not its members, and portfolios are no other tools of a tool.
- `-unique-tex`: The same in LaTeX.
- `-midrule-after:GROUP`: A `\midrule` after the row of GROUP in `-overview-tex` and `-unique-tex`. Repeatable.

### Comparisons over all groups
These tables contain the runs of all groups together; tools are named as in `-tools`.
- `-compare:TOOL,OTHER`: One row per pair (text), e.g. a tool with and without a feature: `Tool`, then `Sat`, `Unsat` (solved) and `Memout` of TOOL, `Other`, the same of OTHER, and the benchmarks (sat/unsat) solved only by TOOL (`Tool only`) and only by OTHER (`Other only`), as in `-matrix`. Column keys `tool`, `sat`, `unsat`, `memout`, `other`, `tool-only`, `other-only`; `sat`, `unsat` and `memout` are the keys of the columns of both tools. Repeatable.
- `-compare-tex:TOOL,OTHER`: The same in LaTeX, with a row of column groups above the header: `Tool` (key `tool-group`) over `Sat`, `Unsat` and `Memout` of TOOL, `Other` (`other-group`) over those of OTHER, `Solved only` (`only-group`) over `Tool only` and `Other only`. Repeatable.
- `-runtimes:BASE[=TOOL1,TOOL2,...]`: Durations compared with those of BASE (text): two rows per tool of the list, without a list per tool other than BASE, one per verdict (`sat`, `unsat`), with the benchmarks solved with the verdict by both (`Both`), those solved in less and in more time by the tool (`Faster`, `Slower`), and the median and the geometric mean of the time ratio, the duration of the tool divided by that of BASE (`Median`, `Geo. mean`), `-` without benchmarks. A benchmark with a duration of 0 s has no ratio. Column keys `tool`, `base`, `verdict`, `both`, `faster`, `slower`, `median`, `geomean`. Repeatable; the rows of all bases are in one table, with the base in the column `Base`.
- `-runtimes-tex:BASE[=TOOL1,TOOL2,...]`: The same in LaTeX, with a `\midrule` between the bases and a row of column groups above the header: `Benchmarks` (key `benchmarks-group`) over `Both`, `Faster` and `Slower`, `Time ratio` (`ratio-group`) over `Median` and `Geo. mean`. Repeatable.
- `-wall-times`: Over all groups, one row per tool: the wall times in seconds of the solved benchmarks, their number (`Solved`), median, 90th and 99th percentile (nearest rank) and maximum, `-` without solved benchmarks, and the number solved after 90% of the wall time limit (`After 90% of limit`); different wall time limits in the inputs of a tool are an error, as for `-scatter-pdf`. Column keys `tool`, `solved`, `median`, `p90`, `p99`, `max`, `late`. In text, the column group is part of the header.
- `-wall-times-tex`: The same in LaTeX, with the column group `Wall time (s)` (key `durations-group`) over the median, the percentiles and the maximum.
- `-peak-memory`: Over all groups, one row per tool: the median and the 90th percentile (nearest rank) of the peak memory in MB (`maxRss` of the log, 1 MB = 1024 kB) of the solved benchmarks and of all runs; runs without a peak memory in their log are left out, `-` without runs. Column keys `tool`, `solved-median`, `solved-p90`, `all-median`, `all-p90`. In text, the column group is part of the header.
- `-peak-memory-tex`: The same in LaTeX, with the column groups `Solved` (`solved-group`) and `All runs` (`all-group`).

### Virtual portfolios
- `-portfolio:NAME=TOOL1,TOOL2,...`: Adds the row `V. Portfolio [NAME]` to every table and plot: per benchmark the fastest run of the tools (named as in `-tools`) that solved it, else a timeout of one of them. Repeatable.

### Selection of tools
- `-tools:TOOL1,TOOL2,...`: Only these tools, in this order, in every table and plot. A tool is named as in the logs (`tool-name`, with or without the notes in brackets) or, for a portfolio, by the NAME of `-portfolio`. The other tools are left out before the benchmarks run by all tools are determined; portfolios of left-out tools stay possible. `-rename-tool` changes only the printed names.

### Status rules
- `-expected:FILE`: Expected verdicts from FILE in place of those in the logs, before any counting. One benchmark per line: its name as in the logs (`bmName`) and its expected verdict, `sat` or `unsat`, separated by white space; empty lines and lines starting with `#` are skipped. Repeatable.
- `-incorrect-as-unsolved`: An incorrect verdict, a sat or unsat answer that contradicts the expected verdict, is unsolved: the extended summary gets the column `Incorrect` (key `incorrect`), `Sat` and `Unsat` count only correct verdicts, and a portfolio solves a benchmark only with a correct verdict. The other tables and plots count only correct verdicts as solved anyway.
- `-crashes-as-errors`: A crashed run is an error (`Error`), for every output parser: a run that ended with a nonzero exit code (the line `other error (exit code: N)` of the run script) without an error result of the output parser, and a run killed before the wall time limit without a verdict that is not a memout. Without the option, a run of a standard SMT solver with a nonzero exit code is an error only without a result (`sat`, `unsat` or `unknown`) in its output, and a killed run is a timeout.
- `-unknown-as-timeout:N`: A run with the result unknown that lasted at least N seconds is a timeout (`Timeout`), e.g. for a tool that answers unknown at its own time limit below the wall time limit. Errors stay errors.

### Columns of the tables
The keys of the columns are given with the tables above; those of the summaries are `tool`, `sat`, `unsat`, `incorrect`, `timeout`, `memout`, `error`, `unknown` and `total`.
- `-rename-col:KEY=TO`: The header of the columns with the key KEY is TO, e.g. where two columns have the same header and `-rename-header` would rename both; also for the key of a column group. Repeatable.
- `-col-order:KEY1,KEY2,...`: The columns with these keys first, in this order, then the other columns in their order, e.g. `-col-order:tool,total` for the number of benchmarks after the tool. With several columns of one key, e.g. `sat` in `-compare`, all of them move.

### Appearance of the LaTeX tables
- `-tex-tabular`: Every LaTeX table in a float, all but the `longtable` of `-details-tex`, as a `tabular` environment only, without the `table` float with its label and without the comment on the packages, e.g. for a float of a document; the caption is in a comment line `% caption: ...` above the tabular. The rules of booktabs stay.

Every table has a caption: its title and what the headers do not say, e.g. how incorrect verdicts are counted, what a cell of a matrix is, or how a time ratio is defined, with the final headers (after `-rename-header`, `-rename-col` and `-hide-col`). In LaTeX it is the caption of the float, in text it is below the table.
- `-matrix-upright`: The column headers of `-matrixtex` upright instead of rotated by 90 degrees, and no vertical rule after the first column, e.g. for short tool names.

### Plots
- `-cactus-pdf`: Generate a Cactus plot (Solved Benchmarks vs Time), the time on a logarithmic axis.
  - Output, in the current directory: `cactus-plot-by-time-total-solved.pdf`, `cactus-plot-by-time-safe-solved.pdf`, `cactus-plot-by-time-unsafe-solved.pdf`.
  - With `-tools`, the tools are in the order of `-tools`.
- `-cactus-verdict-pdf:NAME`: Cactus plots over all groups, one per verdict: `NAME-sat.pdf` and `NAME-unsat.pdf`, NAME a path without extension. Per tool, the durations of the benchmarks solved with the verdict in increasing order (x: benchmarks solved, y: duration on a logarithmic axis, a duration of 0 s at the power of ten below the shortest positive one), the legend above the plot. Repeatable.
- `-scatter-pdf:NAME=TOOL,OTHER`: Scatter plot `NAME.pdf` over all groups: per benchmark run by both tools and solved by at least one of them, a point with the duration of TOOL (x) and of OTHER (y), by verdict, a benchmark solved by the two tools with different verdicts as `inconsistent` (a series shown only if there is such a benchmark; only without an expected verdict, since a verdict that contradicts the expected one is not counted as solved), the number of points per series in the legend. The duration of an unsolved benchmark is the time limit (the larger wall time limit of the two), marked by dashed lines; logarithmic axes and the diagonal, a duration of 0 s at the power of ten below the shortest positive duration. Repeatable.
- `-rename-label:FROM=TO`: The axis label FROM of every plot is TO, e.g. `-rename-label:Duration (s)=Wall time (s)`. The labels of `-cactus-verdict-pdf` are `sat benchmarks solved`, `unsat benchmarks solved` and `Duration (s)`, those of `-scatter-pdf` the tool name followed by ` duration (s)`. Repeatable.
- `-plot-width:PT`: The width of the plots in the document, in points, e.g. the width of a subfigure. The text of the plots is scaled so that it has its size in the document.
- `-plot-font-size:PT`: The size of all text of the plots (axis labels, tick labels, legend), in points; in the document with `-plot-width`.
- `-plot-aspect:R`: The height of the plots relative to their width, e.g. `1` for square plots. Without it, scatter plots are slightly higher than wide and cactus plots wider than high.
- `-scatter-marker-size:PT`: The size of the markers of the scatter plots (filled circles for sat, triangles for unsat, diamonds for inconsistent), in points; in the document with `-plot-width`; default 2.5.
- `-scatter-marker-opacity:F`: The opacity of the markers of the scatter plots, from 0 (transparent) to 1; default 0.8.
- `-plot-html`: Next to every plot `NAME.pdf`, an interactive version `NAME.html` with the same series, axes and lines, plotly.js in the file (about 4 to 6 MB): zoom with a box or the mouse wheel, the benchmark and the values of a point on hover, a series hidden by a click on the legend, the box of the plot resized by its lower right corner, with its size and height / width shown below (for `-plot-aspect`), a download as SVG.

## Status Code Mapping (Tables)

| Result | Summary Headers | Details Code |
| :--- | :--- | :--- |
| Satisfiable / Safe | **Sat** | **S** |
| Unsatisfiable / Unsafe | **Unsat** | **U** |
| Timeout | Unknown (Simple) | **?** |
| Error | Unknown (Simple) | **E** |
| Unknown | Unknown (Simple) | **?** |

## SMT Support

The tool automatically detects SMT benchmarks by the `.smt2` extension in benchmark names.
- `sat` -> **Sat / S**
- `unsat` -> **Unsat / U**
- a nonzero exit code (`other error (exit code: N)`) without `sat`, `unsat` or `unknown` -> **Error / E**
