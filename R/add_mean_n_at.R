
#' Add mean number of at signs
#'
#' @param output Data frame. Output from a suppression function.   
#' @param hierarchies List of data frames as dimList in sdcTable
#' @param include_HiTaS_Class Logical; whether to include the HiTaS classification. Default is TRUE.
#'
#' @export
#'
#' @examples
#' 
#' d3 <- SSBtools::SSBtoolsData("d3")
#' d3$value <- (d3$freq + pi)^2
#' hi <- SSBtools::FindDimLists(d3[c(1:2, 4:6)])
#' a <- GaussSuppression::SuppressDominantCells(d3, 
#'                            dominanceVar = "value", 
#'                            hierarchies = hi, 
#'                            pPercent = pi, 
#'                            singletonMethod = "numttTtT")
#' 
#' b <- add_mean_n_at(a, hi)
#' head(b)
#' tail(b)
#' 
#' # if pipe operator is preferred (|> can be replaced by %>%)
#' b <- a |> add_mean_n_at(hi)
#' 
add_mean_n_at <- function(output, hierarchies, include_HiTaS_Class = TRUE){
  hierarchies <- lapply(hierarchies, add_n_at)
  at_matrix <- matrix(0L, nrow(output), length(hierarchies))
  for(i in 1:length(hierarchies)) {
    ma <- match(output[[names(hierarchies)[i]]], hierarchies[[i]][[2]])
    at_matrix[,i] <- hierarchies[[i]][["n_at"]][ma]
  }
  output$mean_n_at <-  rowMeans(at_matrix)
  if (include_HiTaS_Class) {
    output$HiTaS_Class <- rowSums(at_matrix -1) 
  }
  output
} 


add_n_at <- function(df) {
  all_at <- function(x) {
    if (!is.character(x)) {
      return(FALSE)
    } 
    all(grepl("^@+$", x), na.rm = FALSE)
  }
  if(!all_at(df[[1]])) {
    stop("Only @s in first column needed")
  }
  df$n_at <- nchar(df[[1]])
  df
} 

