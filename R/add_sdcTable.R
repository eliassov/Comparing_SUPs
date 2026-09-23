


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("nace_2"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  10000, n_unique = 1000)
  initialize_gauss("test1", df, hiers)
  add_sdcTable("test1")
  add_sdcTable("test1", method = "SIMPLEHEURISTIC_OLD")
  all <- readRDS("merged/test1.rds")
  head(all$df_merged)
}


#' Add sdcTable Suppression to SDC Object
#'
#' Runs secondary cell suppression using the `sdcTable` package on a saved SDC object and adds the results.
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "out_simple", "df_merged", "prob.microDat", or "resSIMPLE" to return objects instead of saving.
#' @param method Character string; the secondary suppression method to use (e.g., "SIMPLEHEURISTIC"). Default is "SIMPLEHEURISTIC".
#' @param pvalue Numeric; threshold for identifying unsafe cells if not using external primary suppressions. Default is 5.
#' @param use_external_primary Logical; if TRUE, uses existing primary suppressions from the precomputed "primary_gauss" column in `df_merged`. Default is TRUE.
#' @param time_limit Numeric; time limit in seconds for the solver. Default is 3600.
#' @param fatal_error Logical; if TRUE, manually registers a fatal error status to avoid re-running failing configurations. Default is FALSE.
#'
#' @return Modifies the saved RDS file with sdcTable suppression results, or returns a specific object depending on the `output` parameter.
#' @export
add_sdcTable <- function(filename, path = "merged", output = NULL, 
                         method = "SIMPLEHEURISTIC", pvalue = 5,
                         use_external_primary = TRUE,
                         time_limit = 3600,
                         fatal_error = FALSE) {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  df_microdata <- all[["df_microdata"]]
  df_merged <- all[["df_merged"]]

  if (method %in% df_merged$method) {
    if (is.null(output)) {
      #stop(paste(method, "output already included"))
      rlang::inform(c(
        v = paste(method, "output already included"),
        i = "Skipping computation"
      ))
      return(invisible(NULL))
    } else {
      warning(paste(method, "output already included"))
    }
  }
  
  hier_names <- names(hrc_GAUSS)
  
  cat("\n", "[makeProblem..")
  flush.console()
  
  #create sdcProblem object
  prob.microDat <- sdcTable::makeProblem(
    data = df_microdata,
    dimList = hrc_GAUSS,
    freqVarInd = NULL,
    numVarInd = match("response", names(df_microdata)),
    weightInd = NULL,
    sampWeightInd = NULL)
  
  
  
  cat("] [primarySuppression..")
  flush.console()
  
  #primary suppressions
  
  if(use_external_primary) {
    prob.microDat <- external_primary(prob.microDat, 
                                      df_external = df_merged, 
                                      dim_var = hier_names, 
                                      primary_var = "primary_gauss")
  } else {
    prob.microDat <- sdcTable::primarySuppression(prob.microDat,type = "p", p=pvalue, numVarName="response")
  }
  
  sdcTable_method <- method
  
  if(identical(output,  "prob.microDat")){
    return(prob.microDat)
  }
  
  cat("] [protectTable..")
  flush.console()
  
  
  if (fatal_error) {
    timing <- system.time({
      resSIMPLE <- try(
        stop("R Session Aborted. R encountered a fatal error."),
        silent = TRUE
      )
    })
  } else {
    if(isTRUE(is.finite(time_limit))) {
      timing <- system.time({
        resSIMPLE <- try({
          
          old_warn <- getOption("warn") # needed for time_limit to work in practice
          options(warn = 2)
          on.exit(options(warn = old_warn), add = TRUE)
          
          setTimeLimit(elapsed = time_limit)
          on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
          sdcTable::protectTable(prob.microDat, method = sdcTable_method)
        }, silent = TRUE)
      })  
    } else {
      timing <- system.time({
        resSIMPLE <- try(sdcTable::protectTable(prob.microDat, method = sdcTable_method), silent = TRUE)
      })
    }
  }
  
  
  cat("]\n")
  flush.console()
  
  if(identical(output,  "resSIMPLE")){
    return(resSIMPLE)
  }
  
  df_merged <- add_info(df_merged, method, timing, try_result = resSIMPLE)
  
  if (inherits(resSIMPLE, "try-error")) {
    ok <- FALSE
    error <- as.character(resSIMPLE)
    if(!is.null(output)){
      stop(error)
    } 
    #df_merged$error[i] <- error
  } else {
    #output data.frame
    result_simpleheuristic <- sdcTable::getInfo(resSIMPLE, type = "finalData")
    
    result_simpleheuristic <- as.data.frame(result_simpleheuristic )
    
    out_simple <- result_simpleheuristic |> 
      dplyr::mutate(Status = dplyr::recode(sdcStatus,
                             "s" = 2,
                             "x" = 12,
                             "u" = 9)) |> 
      dplyr::select(-sdcStatus)
  
      
    out_simple <- as.data.frame(out_simple)
    
    # remove empty
    out_simple <- out_simple[out_simple$Freq!=0, , drop = FALSE]
    
    if(identical(output,  "out_simple")){
      return(out_simple)
    }
    
    primary_method <- paste("primary", tolower(method), sep = "_")
    suppressed_method <- paste("suppressed", tolower(method), sep = "_")
    
    
    df_merged[[primary_method]] <-  primary_tau(out_simple,  df_merged[hier_names])
    df_merged[[suppressed_method]] <- df_merged[[primary_method]] 
    df_merged[[suppressed_method]][hidden_tau(out_simple,  df_merged[hier_names])] <- TRUE
    
    
    ok_primary <- all.equal(df_merged[[primary_method]], df_merged[["primary_gauss"]])
    
    if (!isTRUE(ok_primary)) {
      warning(paste("primary not as gauss:", ok_primary))
    }
    
  }
  
  if(identical(output,  "df_merged")){
    return(df_merged)
  }
  
  all[["df_merged"]] <- df_merged
  
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
  
}
  




