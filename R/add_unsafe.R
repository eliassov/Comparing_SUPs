


if (FALSE) {  # Example 
  
  
  hiers <- list(var1 = read_hier("region_mid")[1:85, ],
                var2 = read_hier("nace_2")[1:35, ],
                var3 = simple_hier(5, "A"))
  prod(sapply(prime_positions(hiers), length))
  hiers <- lapply(hiers, remove_bogus)
  df <- create_microdata(hiers, n_ids =  30000, n_unique = 9000)
  info_microdata(df, hiers)
  all <- check_primary(df, hiers)
  
  path <- "merged"
  dir.create(path)  # create if the folder does not exist
  filename <- "test3" 
  initialize_gauss(filename , df, hiers, path = path, overwrite = TRUE)
  add_sdcTable(filename, method = "SIMPLEHEURISTIC_OLD", path = path)
  add_unsafe(filename, path = path)  # unsafe for all methods not already computed
  sum(readRDS(file.path(path, paste0(filename, ".rds")))$df_merged$unsafe_simpleheuristic_old)
  
   
}


#' Add Unsafe Secondary Suppressions to SDC Object
#'
#' Identifies "unsafe" secondary suppressions (cells that can be exactly disclosed via linear dependencies) and adds unsafe indicators.
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "df_merged" to return the merged data frame (applies only when `multiple = FALSE`).
#' @param multiple Logical; if TRUE, computes unsafe indicators for all suppression methods that lack them. If FALSE, computes for only a single method. Default is TRUE.
#'
#' @return Modifies the saved RDS file with unsafe suppression results, or returns a data frame depending on the `output` parameter.
#' @export
add_unsafe <- function(filename, 
                          path = "merged", 
                          output = NULL, 
                          multiple = TRUE) {
  

  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  df_microdata <- all[["df_microdata"]]
  df_merged <- all[["df_merged"]]
  
  method_names <- sub("^suppressed_", "", grep("^suppressed_", names(df_merged), value = TRUE))
  unsafe_names <- sub("^unsafe_", "", grep("^unsafe_", names(df_merged), value = TRUE))
  
  unsafe_names <- c(unsafe_names, "gauss")
  
  compute_names <- setdiff(method_names, unsafe_names)
  

  if(!length(compute_names)) {
    #stop(paste("All unsafe computed"))
    rlang::inform(c(
      v = "All unsafe computed",
      i = "Skipping computation"
    ))
    return(invisible(NULL))
  }
  
  method <- compute_names[1]
  
  cat("\n\n\n  #####################################################################\n")
  cat("  #############   USAFE CALCULATIONS FOR   ################\n")
  cat("  #############   ", method, "      ################\n")
  cat("  #####################################################################\n")
  if( length(compute_names) > 1 ) {
    cat("  #  remaining methods:", paste(compute_names[-1], collapse = ", "), "\n\n\n")
  } else {
    cat("  ##############  last method         ###############\n\n\n")
  }
  
  
  primary <- df_merged[[paste("primary", method, sep = "_")]]
  
  suppressed <- df_merged[[paste("suppressed", method, sep = "_")]]
  forced <- !suppressed 
  hidden <- suppressed  & !primary 
  
  # To calculate unsafe all cell cannot be forced/suppressed 
  not_forced_id <- which.max(df_merged$response * forced)
  
  # hack
  not_forced_id = which(df_merged$response == df_merged$response[not_forced_id]) 
  
  
  forced[not_forced_id] <- FALSE 
  
  
  timing <- system.time({
    out  <- GaussSuppression::SuppressDominantCells(data=df_microdata,
                                  numVar = "response",
                                  hierarchies = hrc_GAUSS,
                                  preAggregate = TRUE,
                                  extraAggregate = FALSE,
                                  primary = primary,
                                  hidden = hidden, 
                                  forced = forced,
                                  singletonMethod = "none",
                                  removeEmpty = TRUE,
                                  forcedInOutput = FALSE, 
                                  unsafeInOutput = TRUE)
  })
  
  hier_names <- names(hrc_GAUSS)
  ok <- all.equal(out[hier_names], df_merged[hier_names])
  
  if (!isTRUE(ok)) {
    print(ok)
    stop("generated table was not identical")
  }
  
  if( sum(out$suppressed[not_forced_id]) ) {
    stop("problematic to calculate unsafe in this case")
  }
  
  df_merged[[paste0("unsafe_", method)]] <- out$unsafe
  
  if(identical(output,  "df_merged")){
    return(df_merged)
  }
  
  all[["df_merged"]] <- df_merged
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
  if(multiple & length(compute_names) > 1) {
    add_unsafe(filename, path = path, multiple = TRUE)
  } 
  invisible(NULL)
}
  
