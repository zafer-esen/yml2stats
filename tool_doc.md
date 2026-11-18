# yml2stats Documentation

`yml2stats` is a tool for processing `.yml` benchmark result files and generating summary tables, matrices, and plots. It supports both standard verification results (Safe/Unsafe) and SMT solver results (Sat/Unsat).

## Usage

```bash
yml2stats [options] <input_file_or_directory>...
```

When a directory is provided, all `.yml` files within it are processed.

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
- `-compare-tex:TOOL,OTHER`: The same in LaTeX. Repeatable.
- `-runtimes:BASE`: Durations compared with those of BASE (text): two rows per tool other than BASE, one per verdict (`sat`, `unsat`), with the benchmarks solved with the verdict by both (`Both`), those solved in less and in more time by the tool (`Faster`, `Slower`), and the median and the geometric mean of the time ratio, the duration of the tool divided by that of BASE (`Median`, `Geo. mean`), `-` without benchmarks. A benchmark with a duration of 0 s has no ratio. Column keys `tool`, `verdict`, `both`, `faster`, `slower`, `median`, `geomean`. Repeatable, one table per BASE.
- `-runtimes-tex:BASE`: The same in LaTeX. Repeatable.

### Virtual portfolios
- `-portfolio:NAME=TOOL1,TOOL2,...`: Adds the row `V. Portfolio [NAME]` to every table and plot: per benchmark the fastest run of the tools (named as in `-tools`) that solved it, else a timeout of one of them. Repeatable.

### Selection of tools
- `-tools:TOOL1,TOOL2,...`: Only these tools, in this order, in every table and plot. A tool is named as in the logs (`tool-name`, with or without the notes in brackets) or, for a portfolio, by the NAME of `-portfolio`. The other tools are left out before the benchmarks run by all tools are determined; portfolios of left-out tools stay possible. `-rename-tool` changes only the printed names.

### Status rules
- `-expected:FILE`: Expected verdicts from FILE in place of those in the logs, before any counting. One benchmark per line: its name as in the logs (`bmName`) and its expected verdict, `sat` or `unsat`, separated by white space; empty lines and lines starting with `#` are skipped. Repeatable.
- `-incorrect-as-unsolved`: An incorrect verdict, a sat or unsat answer that contradicts the expected verdict, is unsolved: the extended summary gets the column `Incorrect` (key `incorrect`), `Sat` and `Unsat` count only correct verdicts, and a portfolio solves a benchmark only with a correct verdict. The other tables and plots count only correct verdicts as solved anyway.
- `-crashes-as-errors`: A crashed run is an error (`Error`), for every output parser: a run that ended with a nonzero exit code (the line `other error (exit code: N)` of the run script) without an error result of the output parser, and a run killed before the wall time limit without a verdict that is not a memout. Without the option, the output parser for standard SMT solvers skips the `other error` lines, so that a run without other output is unknown, and a killed run is a timeout.
- `-unknown-as-timeout:N`: A run with the result unknown that lasted at least N seconds is a timeout (`Timeout`), e.g. for a tool that answers unknown at its own time limit below the wall time limit. Errors stay errors.

### Plots
- `-cactus`: Generate a Cactus plot (Solved Benchmarks vs Time).
  - Output: `cactus-plot-by-time-total-solved.pdf`, `cactus-plot-by-time-safe-solved.pdf`, `cactus-plot-by-time-unsafe-solved.pdf`.

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
