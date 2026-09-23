


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  100, n_unique = 20)
  initialize_gauss("test", df, hiers)
  add_modular("test")
}


#' Add Modular Suppression to SDC Object
#'
#' Runs the MODULAR (Tau-Argus) secondary cell suppression method on a saved SDC object and adds the results.
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "out_tau" or "df_merged" to return data frames instead of saving.
#' @param split_tab Logical; parameter passed to `rtauargus::tab_rtauargus()`. Default is FALSE.
#' @param add_HiTaS_log_time Logical; whether to parse and include the Tau-Argus log-reported time. Default is TRUE.
#'
#' @return Modifies the saved RDS file with MODULAR suppression results, or returns a data frame depending on the `output` parameter.
#' @export
add_modular <- function(filename, path = "merged", output = NULL, split_tab = FALSE,
                        add_HiTaS_log_time = TRUE) {
  
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  
  hrc_GAUSS <- all[["hierarchies"]]
  tab_gauss <-  all[["df_merged"]]
  
  if("modular" %in% tab_gauss$method) {
    # stop("modular output already included")
    rlang::inform(c(
      v = "modular output already included",
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
  tau <- prepare_tauargus_inputs(tab_gauss, hrc_GAUSS, hierarchies)
  
  #write all resulting data to global environment (For Debugging only)
  # list2env(tau, envir = .GlobalEnv)
  #Measure Time
  #start_time_modular <- Sys.time()
  #######Call Modular with rtauargus
  
  timing <- system.time({
    ex1 <- try(rtauargus::tab_rtauargus(
      tau$tab_modular_input,
      dir_name = "argus_files",
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
    tab_gauss$primary_modular <-  primary_tau(out_tau,  tab_gauss[tau$vars])
    tab_gauss$suppressed_modular <- tab_gauss$primary_modular 
    tab_gauss$suppressed_modular[hidden_tau(out_tau,  tab_gauss[tau$vars])] <- TRUE
  } 

  i <- match(NA, tab_gauss$method)
  tab_gauss <- add_info(tab_gauss, "modular", timing,  try_result = ex1)
  
  if(ok & add_HiTaS_log_time) {
    log_path <- getOption("HiTaS.log_path")
    if(is.null(log_path)) {
      warning("No HiTaS_log_time: option HiTaS.log_path is empty")
    } else {
      file <- file.path(log_path, "HiTaS.log")
      line <- grep("^[ \t]*Totaltime", readLines(file, warn = FALSE), value = TRUE)
      line <- gsub("\\s+", " ", sub("^\\s*Totaltime:\\s*", "", line))
      tab_gauss$HiTaS_log_time[i] <- line
    }
  }
  
  if(identical(output,  "df_merged")){
    return(tab_gauss)
  }
  
  all[["df_merged"]] <- tab_gauss
  
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
}
  



write_argus_hrc <- function(df, hrc_name){
  write.table(
    df,
    file = paste0(hrc_name, ".hrc"),
    sep = "",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE
  )
  
  lines <- readLines(paste0(hrc_name, ".hrc"))
  lines <- sub("@@", "", lines)
  writeLines(lines[2:length(lines)], paste0(hrc_name, ".hrc"))
}


#clean working directory from previous run
clean_up <- function(x){
  hierarchies <- list.files(
    pattern = "^hier[0-9]+_df\\.hrc$",
    full.names = TRUE
  )
  if (length(hierarchies) > 0) {
    file.remove(hierarchies)
  }
  
  hierarchies_df <- list.files(
    pattern = "^hier[0-9]+_df",
    full.names = TRUE
  )
  if (length(hierarchies_df) > 0) {
    file.remove(hierarchies_df)
  }
}
###


###
#functions
check_real_hrc <- function(file) {
  txt <- readLines(file, warn = FALSE)
  any(grepl("@", txt, fixed = TRUE))
}

prepare_tauargus_inputs <- function(
    tab_gauss,
    hrc_GAUSS,
    hierarchies # loc_tauargus = "C:/TauArgus/TauArgus/TauArgus.exe"
) {
  
  # rename back to "primary" then continue with old code 
  tab_modular_input <- tab_gauss
  names(tab_modular_input)[names(tab_modular_input) == "primary_gauss"] <- "primary" 
  
  ## 1. Input-Tabelle vorbereiten
  tab_modular_input <- tab_modular_input  |>
    dplyr::select(tidyselect::starts_with("var"), response, n_contr, primary) |> 
    dplyr::mutate(no_pl = primary) |>   # primaries ohne Schutzlevel
    dplyr::filter(response > 0)
  
  ## 2. TauArgus-Pfad setzen
  #options(rtauargus.tauargus_exe = loc_tauargus)
  
  ## 3. erklärende Variablen
  vars <- paste0("var", seq_along(hrc_GAUSS))
  
  ## 4. Hierarchien (named vector)
  hrc_vec <- setNames(
    hierarchies,
    paste0("var", seq_along(hierarchies))
  )
  
  ## 5. Reale vs. nicht-reale Hierarchien
  real_hrc <- sapply(hierarchies, check_real_hrc)
  
  ## 6. Nur reale Hierarchien an rtauargus übergeben
  hrc_vec <- hrc_vec[unname(real_hrc)]
  
  ## 7. Totcodes
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







####  Functions written for other purpose, but reuse here  

### Special functions are defined
# Using hidden limits interval calculation to the primary suppressed cells
primary_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 9
}
hidden_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  taudata$Status[ma] == 12
}
forced_tau <- function(taudata, crossTable, ...) {
  ma <- SSBtools::Match(crossTable, taudata[names(crossTable)])
  if (anyNA(ma) | !identical(range(diff(sort(ma))), c(1L, 1L)))
    stop("Matching failed")
  !(taudata$Status[ma] %in% c(9, 12))
}







