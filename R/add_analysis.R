#' Add Comparison Analysis to SDC Object
#'
#' Compiles performance, risk, and utility comparative analysis metrics across the implemented suppression methods.
#'
#' @param filename Character string; name of the file (excluding extension) containing the saved RDS object.
#' @param path Character string; directory where the RDS file is stored. Default is "merged".
#' @param output Character string specifying the output format, or NULL. Use "analysis" to return the analysis list object instead of saving.
#'
#' @return Modifies the saved RDS file with structured analysis metrics, or returns an analysis list depending on the `output` parameter.
#' @export
add_analysis <- function(filename, 
                      path = "merged", 
                      output = NULL) {
  #Load
  all <- readRDS(file.path(path, paste0(filename, ".rds")))
  #initialise analysis list
  analysis <- list()
  
  
  result <- all$df_merged |> fix_empty()
  
  
  
  #add importance score for each cell
  result <- result |> 
    dplyr::mutate(importance_score = response / 10^(5 * mean_n_at))
  
  #identify secondary suppressed cells for each method:
  #GAUSS
  secondary_gauss <- result |>
    dplyr::filter(suppressed_gauss =="TRUE" & primary_gauss=="FALSE")
  #MODULAR
  secondary_modular <- result |>
    dplyr::filter(suppressed_modular=="TRUE" & primary_modular=="FALSE")
  #Simpleheuristic
  secondary_simple <- result |> 
    dplyr::filter(suppressed_simpleheuristic=="TRUE" & primary_simpleheuristic=="FALSE")
  #Simpleheuristic_old
  secondary_simple_old <- result |> 
    dplyr::filter(suppressed_simpleheuristic_old=="TRUE" & primary_simpleheuristic_old=="FALSE")
  #Number of secondaries per method
  secondaries_gauss <- nrow(secondary_gauss)
  secondaries_modular <- nrow(secondary_modular)
  secondaries_simple <- nrow(secondary_simple)
  secondaries_simple_old <- nrow(secondary_simple_old)
  #add to list
  analysis["no_secondaries"] <- list(c(secondaries_gauss,
                                                   secondaries_modular,
                                                   secondaries_simple,
                                                   secondaries_simple_old))
  
  #(absolute) suppressed response value for each method:
  value_surpressed_gauss <- sum(secondary_gauss$response)
  value_surpressed_modular <- sum(secondary_modular$response)
  value_surpressed_simple <- sum(secondary_simple$response)
  value_surpressed_simple_old <- sum(secondary_simple_old$response)
  #add to list
  analysis["absolute_value"] <- list(c(value_surpressed_gauss,
                                        value_surpressed_modular,
                                        value_surpressed_simple,
                                        value_surpressed_simple_old))
  #(relative to response of all cells) suppressed response value for each method:
  rel_value_surpressed_gauss <- sum(secondary_gauss$response)/sum(result$response)
  rel_value_surpressed_modular <- sum(secondary_modular$response)/sum(result$response)
  rel_value_surpressed_simple <- sum(secondary_simple$response)/sum(result$response)
  rel_value_surpressed_simple_old <- sum(secondary_simple_old$response)/sum(result$response)
  
  #add to list
  analysis["relative_value"] <- list(c(rel_value_surpressed_gauss,
                                       rel_value_surpressed_modular,
                                       rel_value_surpressed_simple,
                                       rel_value_surpressed_simple_old))
  
  #Hierarchical damage
  #suppressed importance relative to response of all cells
  importance_value_suppressed_gauss <- sum(secondary_gauss$importance_score)
  importance_value_suppressed_modular <- sum(secondary_modular$importance_score)
  importance_value_suppressed_simple <- sum(secondary_simple$importance_score)
  importance_value_suppressed_simple_old <- sum(secondary_simple_old$importance_score)
  
  analysis["importance_value_surpressed"]<- list(c(importance_value_suppressed_gauss,
                                                   importance_value_suppressed_modular,
                                                   importance_value_suppressed_simple,
                                                   importance_value_suppressed_simple_old))
  
  #Protectedness:
  #For how many primary suppressed cells [lo, up] is OK, i.e  
  #[lomax_response, upmin_respons] is covered by [lo, up]?
    
  #GAUSS
  Protectedness_gauss <- result |>
    dplyr::filter(lomax_response > lo_gauss & upmin_response < up_gauss)
  #in percent
  Protectedness_gauss_percent <- (nrow(Protectedness_gauss)/ sum(!is.na(result$lo_gauss))) *100
  #MODULAR
  Protectedness_modular <- result |>
    dplyr::filter(lomax_response > lo_modular & upmin_response < up_modular)
  #in percent
  Protectedness_modular_percent <- (nrow(Protectedness_modular)/ sum(!is.na(result$lo_modular))) *100
  #simple
  Protectedness_simple <- result |>
    dplyr::filter(lomax_response > lo_simpleheuristic & upmin_response < up_simpleheuristic)
  #in percent
  Protectedness_simple_percent <- (nrow(Protectedness_simple)/ sum(!is.na(result$lo_simpleheuristic ))) *100
  Protectedness_simple_old <- result |>
    dplyr::filter(lomax_response > lo_simpleheuristic_old & upmin_response < up_simpleheuristic_old)
  #in percent
  Protectedness_simple_old_percent <- (nrow(Protectedness_simple_old)/ sum(!is.na(result$lo_simpleheuristic_old))) *100
  
  #add to list
  analysis["Protectedness"] <- list(c(Protectedness_gauss_percent,
                                       Protectedness_modular_percent,
                                       Protectedness_simple_percent,
                                       Protectedness_simple_old_percent))
  
  
  
  # - Percentage disclosures based on exact linear relations.
  # - These disclosures are not reported for gauss (shown as NA), 
  #     since they are exactly what gauss protects against. 
  #     In fact, the calculations are performed using the gauss algorithm.
  # - The lower and upper interval bounds should be identical if the calculations are correct.
  
  analysis[["exact_linear_reveal_percent"]] <- fun4(exact_linear_reveal_percent, all$df_merged)
  analysis[["exact_integer_reveal_percent"]] <- fun4(exact_integer_reveal_percent, all$df_merged)
  analysis[["n_0_upper_bound"]] <- fun4(n_0_upper_bound, all$df_merged)
  analysis[["elapsed"]] <- fun4(elapsed, all$df_merged)
  analysis[["error"]] <- fun4(error, all$df_merged)
  analysis[["log_time"]] <- fun4(log_time, all$df_merged)
  
  analysis[["n_primary"]] <- sum(all$df_merged$primary_gauss)
  analysis[["n_cells"]] <- prod(sapply(all$hierarchies, nrow))
  analysis[["n_output"]] <- nrow(all$df_merged)
  analysis[["max_HiTaS_Class"]] <- max(all$df_merged$HiTaS_Class)
  
  analysis[["no_secondaries"]] <- add_names_and_NA(analysis[["no_secondaries"]], 
                                                   analysis[["no_secondaries"]])
  analysis[["absolute_value"]] <- add_names_and_NA(analysis[["absolute_value"]], 
                                                   analysis[["no_secondaries"]])
  analysis[["relative_value"]] <- add_names_and_NA(analysis[["relative_value"]], 
                                                   analysis[["no_secondaries"]])
  analysis[["importance_value_surpressed"]] <- add_names_and_NA(analysis[["importance_value_surpressed"]], 
                                                                analysis[["no_secondaries"]])
  analysis[["Protectedness"]] <- add_names_and_NA(analysis[["Protectedness"]], 
                                                  analysis[["no_secondaries"]])
  
  
  #suppressions by "Class" to evaluate hierarchical damage
  #For GAUSS
  Class_gauss <- result |> 
    dplyr::filter(primary_gauss==F & suppressed_gauss == T) |> 
    dplyr::count(HiTaS_Class, name = "n_GAUSS")
  #For Modular
  Class_modular <- result|> 
    dplyr::filter(primary_modular ==F & suppressed_modular ==T) |> 
    dplyr::count(HiTaS_Class, name = "n_modular")
  #For Simple
  Class_simple <- result|> 
    dplyr::filter(primary_simpleheuristic ==F & suppressed_simpleheuristic ==T) |> 
    dplyr::count(HiTaS_Class, name = "n_simple")
  #For Simple Old
  Class_simple_old <- result|> 
    dplyr::filter(primary_simpleheuristic_old ==F & suppressed_simpleheuristic_old ==T) |> 
    dplyr::count(HiTaS_Class, name = "n_simple_old")
  #for comparing the two
  Classes_table <- dplyr::full_join(dplyr::full_join(dplyr::full_join(Class_gauss, Class_modular), Class_simple),Class_simple_old)
  
  #add to list
  analysis["Suppressions_by_Class"] <- list(Classes_table)
  
  if (identical(output, "analysis")) {
    return(analysis)
  }
  
  #add to instance and save
  all[["analysis"]] <- analysis
  saveRDS(all, file.path(path, paste0(filename, ".rds")))
  
}


fix_empty <- function(result) {
  check_names <- c("lo_gauss", "lo_modular", "lo_simpleheuristic", "lo_simpleheuristic_old", 
    "primary_modular", "primary_simpleheuristic", "primary_simpleheuristic_old", 
    "suppressed_gauss", "suppressed_modular", "suppressed_simpleheuristic", 
    "suppressed_simpleheuristic_old", "unsafe_modular", "unsafe_simpleheuristic", 
    "unsafe_simpleheuristic_old", "up_gauss", "up_modular", "up_simpleheuristic", 
    "up_simpleheuristic_old")
  for (nam in check_names) {
    if (is.null(result[[nam]])) {
      result[[nam]] <- 0L
    }
  }
  result
}


fun4 <- function(fun, ..., name = c("gauss", "modular", "simpleheuristic", "simpleheuristic_old")) {
  res <- rep(NA, length(name))
  names(res) <- name
  for (nam in name) {
    res[[nam]] <- fun(..., name = nam)
  }
  res
}

elapsed <- function(df, name) {
  df$elapsed[match(name, tolower(df$method))]
}

error <- function(df, name) {
  df$error[match(name, tolower(df$method))]
}

log_time <- function(df, name) {
  s <- df$HiTaS_log_time[match(name, tolower(df$method))]
  if (!length(grep("seconds", s))) {
    return(NA)
  }
  seconds <- strsplit(s, split = "seconds")[[1]][1]
  as.numeric(seconds)
}



exact_linear_reveal_percent <- function(df, name) {
  unsafe <- df[[paste0("unsafe_", name)]]
  primary <- df[[paste0("primary_", name)]]
  if (is.null(unsafe)) {
    res <- NA
  } else {
    res <- 100 * sum(unsafe[primary])/sum(primary)
  }
  res
}


exact_integer_reveal_percent <- function(df, name, digits = 3) {
  
  lo <- df[[paste0("lo_", name)]]
  if (is.null(lo))
    return(NA)
  up <- df[[paste0("up_", name)]]
  
  lo <- ceiling(round(lo, digits))
  up <- floor(round(up, digits))
  
  n_reveal <- sum(lo == up, na.rm = TRUE)
  
  100 * n_reveal/sum(!is.na(lo))
}


n_0_upper_bound <- function(df, name) {
  up <- df[[paste0("up_", name)]]
  if (is.null(up))
    return(NA)
  sum(up == 0, na.rm = TRUE)
}


add_names_and_NA <- function(
      x, 
      no_secondaries, 
      name = c("gauss", "modular", "simpleheuristic", "simpleheuristic_old")) {
  names(x) <- name
  x[no_secondaries %in% c(0, NA)] <- NA
  x
}
