# ==============================================================================
#                      ComparingSUPs Five-Way Testing Script
# ==============================================================================
# This script executes a complete pipeline comparison across all 5 secondary cell
# suppression methods (GAUSS, standard MODULAR, native MODULAR, SIMPLEHEURISTIC,
# and SIMPLEHEURISTIC_OLD) using generated mock microdata.
#
# You can run this script directly in your RStudio or Jupyter session on Dapla.
# ==============================================================================

# 1. Load local development files (load the package functions)
project_dir <- "/home/onyxia/work/Comparing_SUPs"
r_files <- list.files(file.path(project_dir, "R"), full.names = TRUE)
for (f in r_files) {
  source(f)
}

# 2. Load standard packaged hierarchies
hiers <- list(
  var1 = read_hier("minimal", path = file.path(project_dir, "inst/extdata")),
  var2 = read_hier("table_header", path = file.path(project_dir, "inst/extdata"))
)

# 3. Generate mock transactional microdata and whole random response variable
cat("\n--- [1/6] Generating mock data ---\n")
microdata <- create_microdata(hiers, n_ids = 100, n_unique = 20, rnd_seed = 42)
microdata_with_response <- add_response(microdata, shape = 1.1)

# 4. Initialize pipeline and compute primary suppressions (p = 5%)
cat("\n--- [2/6] Initializing SDC pipeline & primary suppressions ---\n")
dir.create("merged", showWarnings = FALSE)
initialize_gauss(
  filename = "vignette_test", 
  df_microdata = microdata_with_response, 
  hierarchies = hiers, 
  overwrite = TRUE,
  pvalue = 5
)

# 5. Execute SDC secondary cell suppression methods side-by-side
cat("\n--- [3/6] Running SDC secondary cell suppression methods ---\n")

# Run standard GAUSS
cat("-> Running GAUSS...\n")
add_gauss("vignette_test")

# Run sdcTable (Simple Heuristic & Old Simple Heuristic)
cat("-> Running sdcTable (SIMPLEHEURISTIC)...\n")
add_sdcTable("vignette_test", method = "SIMPLEHEURISTIC")
cat("-> Running sdcTable (SIMPLEHEURISTIC_OLD)...\n")
add_sdcTable("vignette_test", method = "SIMPLEHEURISTIC_OLD")

# Run the NEW native MODULAR suppression (runs in-process, no TauArgus.exe required!)
cat("-> Running MODULAR Native (rtauargus.native)...\n")
add_modular_native("vignette_test")

# (Optional: Run the standard modular if you have options(rtauargus.tauargus_exe = ...) configured)
# cat("-> Running standard MODULAR...\n")
# add_modular("vignette_test")

# 6. Compute Safety Intervals and Unsafe statuses (auto-detects 'modular_native' columns!)
cat("\n--- [4/6] Computing safety intervals and unsafe flags ---\n")
add_intervals("vignette_test")
add_unsafe("vignette_test")

# 7. Compile the five-way comparative utility & security analysis
cat("\n--- [5/6] Compiling comparative SDC analysis ---\n")
analysis_results <- add_analysis("vignette_test", output = "analysis")

# 8. Inspect comparative results
cat("\n--- [6/6] SDC Comparative Analysis Results Summary ---\n\n")
print(analysis_results)
