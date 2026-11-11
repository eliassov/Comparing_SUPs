if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  100, n_unique = 20)
  initialize_gauss("test", df, hiers)
  add_modular_native("test")
}


#' Add Modular Native Suppression to SDC Object
#'
#' Runs the MODULAR (Tau-Argus) secondary cell suppression method using `rtauargus.native`
#' (the native headless engine, which requires no external Tau-Argus installation) on a saved SDC object and adds the results.
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "out_tau" or "df_merged" to return data frames instead of saving.
#' @param split_tab Logical; parameter passed to `rtauargus.native::tab_rtauargus()`. Default is FALSE.
#' @param add_HiTaS_log_time Logical; whether to parse and include the Tau-Argus log-reported time. Default is TRUE.
#'
#' @return Modifies the saved RDS file with MODULAR native suppression results, or returns a data frame depending on the `output` parameter.
#' @export
add_modular_native <- function(filename, path = "merged", output = NULL, split_tab = FALSE,
                               add_HiTaS_log_time = TRUE) {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  tab_gauss <-  all[["df_merged"]]
  
  if("modular_native" %in% tab_gauss$method) {
    rlang::inform(c(
      v = "modular_native output already included",
      i = "Skipping computation"
    ))
    return(invisible(NULL))
  }

  clean_up()
  hrc_names <- paste0("hier",seq_along(hrc_GAUSS),"_df")
  
  for(i in seq_along(hrc_GAUSS)){
    write_argus_hrc(hrc_GAUSS[[i]], hrc_names[i])
  }

  #Read all hierarchy files
  hierarchies <- list.files(
    pattern = "^hier[0-9]+_df\\.hrc$",
    full.names = TRUE
  )
  hierarchies <- sub("^\\./", "", hierarchies)
  
  ######run TauArgus with the same parameters
  #prepare inputs for Argus
  tau <- prepare_tauargus_inputs_native(tab_gauss, hrc_GAUSS, hierarchies)
  
  timing <- system.time({
    ex1 <- try(rtauargus.native::tab_rtauargus(
      tau$tab_modular_input,
      dir_name = normalizePath("argus_files", mustWork = FALSE),
      files_name = "sy1",
      explanatory_vars = tau$vars,
      hrc = tau$hrc_vec,
      secret_var = "primary",
      secret_no_pl = "no_pl",
      value = "response",
      freq = "n_contr",
      totcode = tau$totcode,
      split_tab = split_tab,
      suppress = "MOD(1,5,0,0,0)"
    ), silent = TRUE)
  })
  
  ok <- !inherits(ex1, "try-error") 

  if (ok) { 
    out_tau <- ex1 |> 
      dplyr::select(tidyselect::starts_with("var"),response,Status) |> 
      dplyr::mutate(Status = dplyr::recode(Status,
                                           "V" = 2,
                                           "A" = 9,
                                           "B" = 9,
                                           "C" = 9,
                                           "D" = 12))
  } else {
    out_tau <- ex1
  }
  
  if(identical(output,  "out_tau")){
    return(out_tau)
  }
  
  if (ok) { 
    tab_gauss$primary_modular_native <-  primary_tau(out_tau,  tab_gauss[tau$vars])
    tab_gauss$suppressed_modular_native <- tab_gauss$primary_modular_native 
    tab_gauss$suppressed_modular_native[hidden_tau(out_tau,  tab_gauss[tau$vars])] <- TRUE
  } 

  i <- match(NA, tab_gauss$method)
  tab_gauss <- add_info(tab_gauss, "modular_native", timing,  try_result = ex1)
  
  if(ok & add_HiTaS_log_time) {
    log_path <- getOption("HiTaS.log_path")
    if(is.null(log_path)) {
      warning("No HiTaS_log_time: option HiTaS.log_path is empty")
    } else {
      file <- file.path(log_path, "HiTaS.log")
      if (file.exists(file)) {
        line <- grep("^[ \t]*Totaltime", readLines(file, warn = FALSE), value = TRUE)
        if (length(line) > 0) {
          line <- gsub("\\s+", " ", sub("^\\s*Totaltime:\\s*", "", line))
          tab_gauss$HiTaS_log_time[i] <- line
        }
      }
    }
  }
  
  if(identical(output,  "df_merged")){
    return(tab_gauss)
  }
  
  all[["df_merged"]] <- tab_gauss
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
}

prepare_tauargus_inputs_native <- function(
    tab_gauss,
    hrc_GAUSS,
    hierarchies
) {
  tab_modular_input <- tab_gauss
  names(tab_modular_input)[names(tab_modular_input) == "primary_gauss"] <- "primary" 
  
  tab_modular_input <- tab_modular_input  |>
    dplyr::select(tidyselect::starts_with("var"), response, n_contr, primary) |> 
    dplyr::mutate(no_pl = primary) |>   # primaries ohne Schutzlevel
    dplyr::filter(response > 0)
  
  vars <- paste0("var", seq_along(hrc_GAUSS))
  
  hrc_vec <- setNames(
    hierarchies,
    paste0("var", seq_along(hierarchies))
  )
  
  real_hrc <- sapply(hierarchies, check_real_hrc)
  
  hrc_vec <- hrc_vec[unname(real_hrc)]
  
  totcode <- setNames(
    rep("T", length(hierarchies)),
    paste0("var", seq_along(hierarchies))
  )
  
  list(
    tab_modular_input = tab_modular_input,
    vars = vars,
    hrc_vec = hrc_vec,
    totcode = totcode,
    real_hrc = real_hrc
  )
}
