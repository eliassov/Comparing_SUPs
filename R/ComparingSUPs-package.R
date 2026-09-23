# Declare global variables to satisfy R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ":=", ".I", "x.row_id__", "row_id__", "Status", "sdcStatus",
    "response", "mean_n_at", "suppressed_gauss", "primary_gauss",
    "suppressed_modular", "primary_modular",
    "suppressed_simpleheuristic", "primary_simpleheuristic",
    "suppressed_simpleheuristic_old", "primary_simpleheuristic_old",
    "lomax_response", "lo_gauss", "upmin_response", "up_gauss",
    "lo_modular", "up_modular", "lo_simpleheuristic", "up_simpleheuristic",
    "lo_simpleheuristic_old", "up_simpleheuristic_old", "HiTaS_Class",
    "n_contr", "primary"
  ))
}

#' @importFrom stats runif setNames
#' @importFrom utils flush.console write.table
NULL

