# yml2stats Documentation

`yml2stats` is a tool for processing `.yml` benchmark result files and generating summary tables, matrices, and plots. It supports both standard verification results (Safe/Unsafe) and SMT solver results (Sat/Unsat).

## Usage

```bash
yml2stats [options] <input_file_or_directory>
```

When a directory is provided, all `.yml` files within it are processed.

## Options

### Tables
- `-summary`: Simplified summary table (text). Columns: Tool, Sat, Unsat, Unknown, Total.
- `-summary-tex`: Simplified summary table (LaTeX).
- `-summary-ext`: Extended summary table (text). Splits unknowns into Timeout, Error, and Unknown.
- `-summary-ext-tex`: Extended summary table (LaTeX).
- `-details`: Detailed per-benchmark results (text). Shows S/U/E/? status for each benchmark.
- `-details-tex`: Detailed per-benchmark results (LaTeX).
- `-matrix`: Comparative matrix (text). Shows Benchmarks solved by Row tool but not Column tool.
- `-matrixtex`: Comparative matrix (LaTeX).

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
