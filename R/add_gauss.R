


#' Add GAUSS Suppression to SDC Object
#'
#' Runs secondary cell suppression using the GAUSS method (via the `GaussSuppression` package)
#' on a saved SDC object and adds the results.
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "df_merged" to return the merged data frame instead of saving.
#'
#' @return Modifies the saved RDS file with GAUSS suppression results, or returns a data frame depending on the `output` parameter.
#' @examples
#' hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
#' df <- create_microdata(hiers, n_ids = 100, n_unique = 20)
#' tmp_dir <- tempdir()
#' initialize_gauss("test_gauss", df, hiers, path = tmp_dir, overwrite = TRUE)
#' add_gauss("test_gauss", path = tmp_dir)
#' all <- readRDS(file.path(tmp_dir, "test_gauss.rds"))
#' head(all$df_merged)
#' @export
add_gauss <- function(filename, 
                      path = "merged", 
                      output = NULL) {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  df_microdata <- all[["df_microdata"]]
  df_merged <- all[["df_merged"]]
  
  
  if("gauss" %in% df_merged$method) {
    rlang::inform(c(
      v = "gauss output already included",
      i = "Skipping computation"
    ))
    return(invisible(NULL))
  }
  
  
  primary <- df_merged$primary_gauss
  
  
  timing <- system.time({
    out  <- GaussSuppression::SuppressDominantCells(data=df_microdata,
                                  numVar = "response",
                                  hierarchies = hrc_GAUSS,
                                  preAggregate = TRUE,
                                  extraAggregate = FALSE,
                                  primary = primary,
                                  singletonMethod = "none",
                                  removeEmpty = TRUE)
  })
  
  hier_names <- names(hrc_GAUSS)
  ok <- all.equal(out[hier_names], df_merged[hier_names])
  
  if (!isTRUE(ok)) {
    print(ok)
    stop("generated table was not identical")
  }
  
  df_merged <- add_info(df_merged, "gauss", timing)
  
  df_merged$suppressed_gauss <- out$suppressed 
  
  if(identical(output,  "df_merged")){
    return(df_merged)
  }
  
  all[["df_merged"]] <- df_merged
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
}



