


#' Add Safety Intervals to SDC Object
#'
#' Computes safety intervals (lower/upper bounds) for suppressed cells and adds them to a saved SDC object.
#' Intervals are computed for all methods that do not already have intervals, and the file is updated with new results and saved after each method.
#'
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "df_merged" to return the merged data frame (applies only when `multiple = FALSE`).
#' @param multiple Logical; if TRUE, computes intervals for all suppression methods that lack them. If FALSE, computes intervals for only a single method. Default is TRUE.
#' @param lpPackage Character string; the LP solver package to use for calculations (e.g., "sdcTable"). Default is "sdcTable".
#' @param sample_size Numeric; number of primary suppressed cells to sample and calculate intervals for. Use `Inf` to compute for all. Default is `Inf`.
#' @param n_workers Integer; number of parallel workers/cores to use for calculations. Default is 1.
#'
#' @return Modifies the saved RDS file with safety interval results, or returns a data frame depending on the `output` parameter.
#' @examples
#' \dontrun{
#' hiers <- list(
#'   var1 = remove_bogus(read_hier("minimal")),
#'   var2 = remove_bogus(read_hier("table_header"))
#' )
#' df <- create_microdata(hiers, n_ids = 100, n_unique = 20)
#' tmp_dir <- tempdir()
#' initialize_gauss("test_int", df, hiers, path = tmp_dir, overwrite = TRUE)
#' add_gauss("test_int", path = tmp_dir)
#' add_intervals("test_int", path = tmp_dir)
#' }
#' @export
add_intervals <- function(filename, 
                          path = "merged", 
                          output = NULL, 
                          multiple = TRUE,
                          lpPackage = "sdcTable", 
                          sample_size = Inf, 
                          n_workers = 1) {
  
  rnd_seed <- 123
  if (!is.null(rnd_seed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rnd_seed)
  }
  
  on.exit(clean_up(), add = TRUE)
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  df_microdata <- all[["df_microdata"]]
  df_merged <- all[["df_merged"]]
  
  method_names <- sub("^suppressed_", "", grep("^suppressed_", names(df_merged), value = TRUE))
  interval_names <- sub("^lo_", "", grep("^lo_", names(df_merged), value = TRUE))
  
  n_primary <- sum(df_merged$primary_gauss)
  
  if (sample_size > n_primary) {
    sample_size <- n_primary
  }
  for (i in seq_along(interval_names)) {
    if (sum(!is.na(df_merged[[paste0("lo_", interval_names[i])]])) < sample_size) {
      interval_names[i] <- NA
    }
  }
  interval_names <- interval_names[!is.na(interval_names)]
  
  
  compute_names <- setdiff(method_names, interval_names)
  

  if(!length(compute_names)) {
    # stop(paste("All intervals computed"))
    rlang::inform(c(
      v = "Intervals already computed",
      i = "Skipping computation"
    ))
    return(invisible(NULL))
  }
  
  method <- compute_names[1]
  
  
  i <- match(NA, df_merged$method)
  
  
  cat("\n\n\n")
  
  print(df_merged[seq_len(i-1), c("method", "elapsed", "error")])
  
  cat("\n\n\n  #####################################################################\n")
  cat("  #############   INTERVAL CALCULATIONS FOR   ################\n")
  cat("  #############   ", method, "      ################\n")
  cat("  #####################################################################\n")
  if( length(compute_names) > 1 ) {
    cat("  #  remaining methods:", paste(compute_names[-1], collapse = ", "), "\n\n\n")
  } else {
    cat("  ##############  last method         ###############\n\n\n")
  }
  
  
  primary <- df_merged[[paste("primary", method, sep = "_")]]
  
  lo_name <- paste0("lo_", method)
  up_name <- paste0("up_", method)
  old_intervals <- lo_name %in% names(df_merged)
  
  if (sample_size < n_primary) {
    ind_primary <- which(primary)
    primary[] <- FALSE
    ind_primary <- sample(ind_primary, size = sample_size)
    primary[ind_primary] <- TRUE
  }
  if (old_intervals) {
    primary[!is.na(df_merged[[lo_name]])] <- FALSE
  }
  
  
  suppressed <- df_merged[[paste("suppressed", method, sep = "_")]]
  forced <- !suppressed 
  hidden <- suppressed  & !primary 
  
  hier_names <- names(hrc_GAUSS)
  
  timing <- system.time({
    if (lpPackage == "sdcTable") {
      out <- df_merged[hier_names]
      out$primary <- primary
      out$suppressed  <- suppressed
      intervals <- intervals_by_sdcTable(hrc_GAUSS, 
                                         df_microdata, 
                                         out,
                                         n_workers = n_workers)
      out <- cbind(out, intervals)
    } else {
      out  <- GaussSuppression::SuppressDominantCells(data=df_microdata,
                                  numVar = "response",
                                  hierarchies = hrc_GAUSS,
                                  preAggregate = TRUE,
                                  extraAggregate = FALSE,
                                  primary = primary,
                                  hidden = hidden, 
                                  forced = forced,
                                  singletonMethod = "none",
                                  lpPackage = lpPackage, 
                                  removeEmpty = TRUE,
                                  forcedInOutput = FALSE)
    }
  })
  
  ok <- all.equal(out[hier_names], df_merged[hier_names])
  
  if (!isTRUE(ok)) {
    print(ok)
    stop("generated table was not identical")
  }
  
  df_merged <- add_info(df_merged, paste("interval", method, lpPackage, sum(primary), sep = "_"), timing)
  
  if (old_intervals) {
    new_rows <- !is.na(out$lo)
    df_merged[[lo_name]][new_rows] <- out$lo[new_rows]
    df_merged[[up_name]][new_rows] <- out$up[new_rows]
  } else {
    df_merged[[lo_name]] <- out$lo
    df_merged[[up_name]] <- out$up
  }

  if(identical(output,  "df_merged")){
    return(df_merged)
  }
  
  all[["df_merged"]] <- df_merged
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
  if(multiple & length(compute_names) > 1) {
    add_intervals(filename, path = path, multiple = TRUE,
                  lpPackage = lpPackage, sample_size = sample_size, 
                  n_workers = n_workers)
  } 
  invisible(NULL)
}
  




