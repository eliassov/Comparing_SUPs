
#' Pareto Type II (Lomax) whole nonzero random numbers  
#' 
#' Modified version of VGAM::rparetoII
#'
#' @param n number of observations
#' @param shape shape parameter
#' @param max_response preset value on largest observation. This automatically determines the scale.
#' @param cuttail Values above the (1 - cuttail) quantile are truncated.
#'   This prevents extremes from giving rise to a large proportion of ones.
#'
#' @examples
#' rpareto2(5)
#' rpareto2(5, shape = 2)
rpareto2 <- function(n, shape = 1.1, max_response = 10^9 - 1, cuttail = 1e-07) {
  x <- VGAM::rparetoII(n, shape = shape)
  tailcut <- VGAM::qparetoII(1 - cuttail, shape = shape)
  x[x > tailcut] <- tailcut
  x <- x * (max_response / max(x))
  x <- ceiling(x)
  x[x > max_response] <- max_response  # prevent numerical imprecision from giving too large a number
  x
}


#' Add a randomly generated response variable to data
#' 
#' Pareto Type II (Lomax) whole nonzero random numbers are added.
#'
#' @param df data frame  
#' @param shape shape parameter
#' @param response name of response variable 
#' @param rnd_seed If non-NULL, a random generator seed to be used locally 
#'   within the function without affecting the random value stream in R.
#'
#' @examples
#' df <- SSBtools::SSBtoolsData("paris2025_micro")
#' 
#' add_response(df)
#' 
#' # if pipe operator is preferred (|> can be replaced by %>%)
#' df |> add_response()
#' df |> add_response() |> add_response(shape = 2, response = "r2")
#' df |> add_response() |> add_response(rnd_seed = 44, response = "r44")
#' @export
add_response <- function(df, shape = 1.1, response = "response", rnd_seed = 123) {
  if (!is.null(rnd_seed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(rnd_seed)
  }
  df[[response]] <- rpareto2(nrow(df), shape = shape)
  df
}

# if the VGAM package is not available, one can replace 
# VGAM::rparetoII with VGAM_rparetoII

# Code copied from VGAM
VGAM_rparetoII <- function (n, location = 0, scale = 1, shape = 1) {
  VGAM_rparetoIV(n = n, location = location, scale = scale, inequality = 1, 
                 shape = shape)
} 
VGAM_rparetoIV <- function (n, location = 0, scale = 1, inequality = 1, shape = 1) {
  #if (!is.Numeric(inequality, positive = TRUE)) 
  #  stop("bad input for argument 'inequality'")
  ans <- location + scale * (-1 + runif(n)^(-1/shape))^inequality
  ans[scale <= 0] <- NaN
  ans[shape <= 0] <- NaN
  ans
}

