


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  100, n_unique = 20)
  initialize_gauss("test", df, hiers, overwrite = TRUE)
  all <- readRDS("merged/test.rds")
  head(all$df_merged)
}


#' Initialize GAUSS Suppression Object
#'
#' Initializes the SDC object and computes the primary suppressions using the GAUSS method (without secondary suppressions).
#'
#' @param filename Character string; name of the file (excluding extension) to save the SDC list object under.
#' @param df_microdata A data frame of generated microdata.
#' @param hierarchies A list of hierarchies corresponding to the dimensions in the microdata.
#' @param path Character string; directory where the RDS file should be saved. Default is "merged".
#' @param overwrite Logical; if TRUE, overwrites any existing RDS file with the same name. Default is FALSE.
#' @param pvalue Numeric; threshold for identifying unsafe cells. Default is 5.
#' @param output Character string specifying the output format, or NULL. Use "all" to return the complete SDC list, or "df_merged" to return the merged data frame.
#'
#' @return Modifies or creates the RDS file with initial primary suppressions, or returns the specified object depending on the `output` parameter.
#' @export
initialize_gauss <- function(filename, df_microdata, hierarchies, path = "merged", 
                             overwrite = FALSE,  
                             pvalue = 5, output = NULL) {
  
  
  for(nam in names(hierarchies)) {
    df_microdata[[nam]] <- toT(df_microdata[[nam]])
    hierarchies[[nam]][,2] <- toT(hierarchies[[nam]][,2]) 
  }
  
  remove_primary <- function(crossTable, ...) {
    rep(NA, nrow(crossTable))
  }
  
  cat(" initialize_gauss ...\n")
  flush.console()
  
  timing <- system.time({
    res <- GaussSuppression::SuppressDominantCells(
      data=df_microdata,
      hierarchies = hierarchies,
      numVar = "response", 
      pPercent = pvalue,
      allDominance = TRUE,
      singletonMethod = "none",
      primary = c(GaussSuppression::MagnitudeRule, remove_primary),
      protectionIntervals = TRUE, 
      intervalSuppression = FALSE,
      removeEmpty = TRUE)
  })
  
  res$pvalue <- 100*(1 - res$dominant2) / res$dominant1
  # res$primary <- res$pvalue < pvalue
  # Instead use lomax_response to avoid boundary issues when pvalue == cutoff
  # and ensure consistent identification of primary cells
  res$primary <- !is.na(res$lomax_response)
  
  remove_vars <- 
    c("dominant1", "dominant2", "max1contributor", "max2contributor", "n_non0_contr", "suppressed")
  
  res <- res[!(names(res) %in% remove_vars)]
  
  
  rename_vars <- names(res) %in% c("primary")
  names(res)[rename_vars] <- paste( names(res)[rename_vars],  "gauss", sep = "_")
  
  
  res$method <- NA
  res$user <- NA
  res$system <- NA
  res$elapsed <- NA
  res$HiTaS_log_time <- NA
  res$error <- NA
  
  res <- add_info(res, "init_gauss_primary", timing)
  
  res <- add_mean_n_at(res, hierarchies)
  
  pp <- prime_positions(hierarchies)
  inner <- rep(TRUE, nrow(res))
  for (nam in names(pp)) {
    inner[!(res[[nam]] %in% pp[[nam]])] <- FALSE
  }
  res$inner <- inner
  
  if(identical(output,  "df_merged")){
    return(res)
  }
  
  all <- list(df_merged = res, df_microdata = df_microdata, hierarchies = hierarchies)
  
  if(identical(output,  "all")){
    return(all)
  }
  
  saveRDS2(all, file.path(path, paste0(filename, ".rds")), overwrite = overwrite)
  
}

saveRDS2 <- function(object, file, overwrite = FALSE, ...) {
  if (file.exists(file) && !overwrite) {
    stop("File already exists: ", file)
  }
  saveRDS(object, file = file, ...)
}


toT <- function(x) {
  x[toupper(x) == "TOTAL"] <- "T"
  x
}


