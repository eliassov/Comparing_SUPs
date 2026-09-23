


if (FALSE) {  # Example 
  hiers <- list(var1 = read_hier("minimal"), var2 = read_hier("table_header"))
  df <- create_microdata(hiers, n_ids =  20, n_unique = 5)
  df
  cbind(SSBtools::MakeFreq(df[1:2]),
        ppercent = inner_ppercent(df))
  info_microdata(df, hiers)
}


#' Generate Mock Microdata
#'
#' Generates mock microdata representing individual/transactional records with hierarchical dimensions and responses.
#'
#' @param hiers A list of hierarchy structures.
#' @param n_ids Total number of observations (rows) to generate. Default is 1000.
#' @param n_unique Number of unique dimension combinations to generate. Default is n_ids/10.
#' @param shape Shape parameter for the Pareto Type II (Lomax) distribution of cell responses. Default is 1.1.
#' @param prob_dim Probability vector for dimension levels.
#' @param prob_freq Probability vector for generating cell frequencies.
#' @param rnd_seed Random seed for cell response generation. Default is 123.
#' @param sample_seed Random seed for sampling the combinations. Default is 123.
#'
#' @return A data frame containing the generated mock microdata.
#' @export
create_microdata <- function(hiers, n_ids =  1000, n_unique = n_ids/10, 
                             shape = 1.1, 
                             prob_dim = 1/seq_len(100000),
                             prob_freq = 1/seq_len(100000),
                             rnd_seed = 123,
                             sample_seed = 123) {
  
  if (!is.null(sample_seed)) {
    if (!exists(".Random.seed"))
      if (runif(1) < 0)
        stop("Now seed exists")
    exitSeed <- .Random.seed
    on.exit(.Random.seed <<- exitSeed)
    set.seed(sample_seed )
  }
  pp <- prime_positions(hiers)
  dim_sizes <- sapply(pp, length)
  if (prod(dim_sizes) > 10^9) {
    stop("The data generation algorithm does not work with very many possible combinations.")
  }
  d <- skewsample(n_ids, n_unique, dim_sizes, prob_freq = prob_freq)
  for (i in seq_along(pp)) {
    d[[i]] <- pp[[i]][d[[i]]]
  }
  names(d) <- names(hiers)
  d$id <- seq_len(n_ids)
  add_response(d, shape = shape, rnd_seed = rnd_seed)
}

# Helper function that calculates p% values for inner cells 
# from microdata generated with create_microdata()
inner_ppercent <- function(df_microdata, include_freq = FALSE) {
  vars <- seq_len(ncol(df_microdata) - 2)
  d <- data.frame(a = SSBtools::RowGroups(df_microdata[vars]), r = -df_microdata$response)
  table(d$a)
  tt <- table(d$a)
  one_or_two <- tt <= 2
  pp <- rep(0, length(tt))
  d <- d[!(d$a %in% which(one_or_two)), ]
  d <- SSBtools::SortRows(d)
  d$r <- -d$r
  tot <- as.vector(rowsum(d$r, group = d$a))
  ma <- match(unique(d$a), d$a)
  m1 <- d$r[ma]
  m2 <- d$r[ma + 1]
  pp[!one_or_two] <- 100 * (tot - m2 - m1)/m1
  if (include_freq) {
    return(data.frame(freq = as.vector(tt), ppercent = pp))
  }
  pp
}

#' Get Information About Microdata
#'
#' Computes and prints key metrics of the generated microdata, such as empty inner cells, singleton cells, and unsafe cells.
#'
#' @param df_microdata A data frame of generated microdata.
#' @param hierarchies A list of hierarchies corresponding to the dimensions in the microdata.
#' @param return_frame Logical; if TRUE, returns the underlying frequencies and p-percent values. Default is FALSE.
#' @param pvalue The threshold for identifying unsafe cells based on the p-percent rule. Default is 5.
#'
#' @return Prints summary statistics. If `return_frame` is TRUE, returns a data frame with frequencies and p-percent values.
#' @export
info_microdata <- function(df_microdata, hierarchies, return_frame = FALSE, 
                           pvalue = 5) {
  n_inner_cells <- prod(sapply(prime_positions(hierarchies), length))
  ipp <- inner_ppercent(df_microdata, include_freq = TRUE)
  n_empty <- n_inner_cells - nrow(ipp)
  n_1 <- sum(ipp$freq == 1)
  n_2 <- sum(ipp$freq == 2)
  n_unsafe <- sum(ipp$ppercent < pvalue)
  freq_max <- max(ipp$freq)
  info <- c(n_inner_cells = n_inner_cells, n_empty = n_empty, n_1 = n_1, n_2 = n_2, n_unsafe = n_unsafe, freq_max = freq_max)
  info[2:5] <- paste0(info[2:5], " (", round(100*info[2:5]/info[1])  ,"%)")
  print(info, quote = FALSE)
  if (return_frame) {
    return(ipp)
  }
  invisible(NULL)
}


skewsample_unique <- function(n_unique, dim_sizes, prob_dim = 1/seq_len(1e+05)) {
  m_prob <- index_to_multi(seq_len(prod(dim_sizes)), dim_sizes)
  for (i in seq_along(dim_sizes)) {
    m_prob[, i] <- prob_dim[m_prob[, i]]
  }
  multi_index <- sample.int(prod(dim_sizes), size = n_unique, prob = apply(m_prob, 1, prod))
  as.data.frame(index_to_multi(multi_index, dim_sizes))
}


skewsample <- function(n, n_unique, dim_sizes, prob_dim = 1/seq_len(1e+05), prob_freq = 1/seq_len(1000)) {
  a <- skewsample_unique(n_unique, dim_sizes, prob_dim)
  a$freq <- sample_int(n_unique, n, prob_freq)
  a <- SSBtools::MakeMicro(a, "freq")
  a <- a[seq_along(dim_sizes)]
  a
}


sample_int <- function(size, tot, prob = 1/seq_len(1000)) {
  a_mean <- tot/size
  n <- n_from_mean(a_mean, prob)
  a <- 0
  while (sum(a) < tot) {
    a <- sample.int(n, size = size, replace = TRUE, prob = prob[seq_len(n)])
  }
  r <- runif(size)
  r <- r * (a - 1)
  r <- r * (sum(a) - tot)/sum(r)
  a <- a - r%/%1
  r <- r%%1
  ii <- order(r, decreasing = TRUE)[seq_len(sum(a) - tot)]
  a[ii] <- a[ii] - 1
  if (any(a <= 0)) {   # retry when 0 or negative
    return(sample_int(size, tot, prob))
  }
  a
}


n_from_mean <- function(a_mean, prob) {
  for (n in 1:1e+05) {
    if (sum(seq_len(n) * prob[seq_len(n)])/sum(prob[seq_len(n)]) >= a_mean) {
      return(n)
    }
  }
}


# written by ChatGPT
index_to_multi <- function(idx, dims) {
  if (any(idx < 1) || any(idx > prod(dims))) {
    stop("idx out of bounds")
  }
  
  idx0 <- idx - 1  
  
  res <- matrix(0L, nrow = length(idx), ncol = length(dims))
  
  for (k in seq_along(dims)) {
    res[, k] <- (idx0 %% dims[k]) + 1
    idx0 <- idx0 %/% dims[k]
  }
  
  colnames(res) <- paste0("dim", seq_along(dims))
  res
}

