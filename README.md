# ComparingSUPs

An R package designed to synthesize realistic microdata and hierarchies, apply various statistical disclosure control (SDC) methods, and compare secondary cell suppression algorithms (such as GAUSS and MODULAR/HiTaS) on risk, utility, and performance.

---

## Installation

You can install the development version of `ComparingSUPs` directly from GitHub using the `remotes` package:

```R
# install.packages("remotes")
remotes::install_github("michell233/Comparing_SUPs", ref = "dev2_oyl")
```

---

## Core Workflow

A typical workflow with this package involves:
1. Generating hierarchical structures and microdata.
2. Generating a numeric response variable.
3. Applying secondary cell suppression methods.
4. Analyzing and comparing results.

### Quick Start Example

```R
library(ComparingSUPs)

# 1. Load pre-packaged hierarchies and remove redundant (bogus) codes
my_hierarchies <- list(
  var1 = remove_bogus(read_hier("minimal")),
  var2 = remove_bogus(read_hier("table_header"))
)

# 2. Generate synthetic microdata (includes a Pareto Type II response variable)
microdata <- create_microdata(my_hierarchies, n_ids = 500, n_unique = 50)

# 3. Initialize the SDC pipeline and compute primary suppressions
dir.create("merged", showWarnings = FALSE)
initialize_gauss("example_run", microdata, my_hierarchies, overwrite = TRUE)

# 4. Run secondary cell suppression methods (e.g., GAUSS, sdcTable, MODULAR)
add_gauss("example_run")
add_sdcTable("example_run", method = "SIMPLEHEURISTIC")
add_modular_native("example_run")

# 5. Compute intervals, unsafe flags, and comparative analysis
add_intervals("example_run")
add_unsafe("example_run")
analysis <- add_analysis("example_run", output = "analysis")
print(analysis)
```

---

## Key Functions

Here are the main functions provided by the package:

| Function | Description |
| :--- | :--- |
| `create_microdata()` | Synthesizes realistic tabular microdata from specified hierarchies. |
| `add_response()` | Generates and appends Pareto Type II (Lomax) whole nonzero random values as a response variable. |
| `add_gauss()` | Applies the GAUSS cell suppression method using `GaussSuppression`. |
| `add_modular()` | Applies the MODULAR / HiTaS secondary cell suppression algorithm using `rtauargus` (Tau-Argus). |
| `add_modular_native()` | Applies the MODULAR / HiTaS secondary cell suppression algorithm natively using `rtauargus.native`. |
| `add_sdcTable()` | Applies secondary cell suppression algorithms (e.g., `SIMPLEHEURISTIC`) using `sdcTable`. |
| `add_intervals()` | Computes interval bounds for suppressed values to assess how tightly they can be estimated. |
| `add_unsafe()` | Evaluates whether primary suppressed cells can be precisely reconstructed. |
| `add_analysis()` | Compiles comparative statistics across multiple suppression runs (data loss, runtime, protection level). |

---

## Running Benchmarks Interactively

If you are exploring the repository itself, the interactive benchmarking and analysis scripts have been moved to the `analysis/` folder to keep a clean R package structure:
* `analysis/microdata.R` – Synthesizes benchmark datasets.
* `analysis/run_GAUSS.R` – Benchmarks the GAUSS cell suppression runtime and outputs.
* `analysis/analysis.R` – Merges outcomes and compares the different algorithms.

To run these scripts, open `Comparing_SUPs.Rproj` in RStudio, load all package functions with `devtools::load_all()` (or `Ctrl+Shift+L`), and run the scripts interactively from the `analysis/` directory.
