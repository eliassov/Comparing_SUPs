# ComparingSUPs

An R package designed to synthesize realistic microdata and hierarchies, apply various statistical disclosure control (SDC) methods, and compare secondary cell suppression algorithms (such as GAUSS and MODULAR/HiTaS) on risk, utility, and performance.

---

## Installation

You can install the development version of `ComparingSUPs` directly from GitHub using the `remotes` package:

```R
# install.packages("remotes")
remotes::install_github("eliassov/Comparing_SUPs", ref = "dev2_oyl")
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

# 1. Create hierarchical structure and microdata
# (Ensure your hierarchies are defined; here we use an example list)
my_hierarchies <- list(
  region = sdcHierarchies::hier_create("TOTAL", c("North", "South")),
  nace = sdcHierarchies::hier_create("TOTAL", c("Agriculture", "Manufacturing"))
)

microdata <- create_microdata(my_hierarchies, n_ids = 500, n_unique = 50)

# 2. Add randomly generated response variable based on Pareto distribution
microdata_with_response <- add_response(microdata)

# 3. Add suppression markers (e.g. MODULAR)
suppressed_data <- add_modular(microdata_with_response, my_hierarchies)
```

---

## Key Functions

Here are the main functions provided by the package:

| Function | Description |
| :--- | :--- |
| `create_microdata()` | Synthesizes realistic tabular microdata from specified hierarchies. |
| `add_response()` | Generates and appends Pareto Type II (Lomax) whole nonzero random values as a response variable. |
| `add_gauss()` | Applies the GAUSS cell suppression method using `GaussSuppression`. |
| `add_modular()` | Applies the MODULAR / HiTaS secondary cell suppression algorithm using `sdcTable`. |
| `add_intervals()` | Computes interval bounds for suppressed values to assess how tightly they can be estimated. |
| `add_unsafe()` | Evaluates whether primary suppressed cells can be precisely reconstructed. |
| `add_analysis()` | Compiles comparative statistics across multiple suppression runs (data loss, runtime, protection level). |

---

## Running Benchmarks Interactively

If you are exploring the repository itself, you can find active benchmarking scripts at the root directory:
* `microdata.R` – Synthesizes benchmark datasets.
* `run_GAUSS.R` – Benchmarks the GAUSS cell suppression runtime and outputs.
* `analysis.R` – Merges outcomes and compares the different algorithms.

To run these scripts, open `Comparing_SUPs.Rproj` in RStudio, load all package functions with `devtools::load_all()` (or `Ctrl+Shift+L`), and run the scripts interactively.
