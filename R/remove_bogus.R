

if (FALSE) {
  hiers1 <- list(var1 = read_hier("minimal"), 
                 var2 = read_hier("region_mid")[1:20, ])
  hiers1
  hiers2 <- lapply(hiers1, remove_bogus)
  hiers2
}

#' Remove Bogus Codes from a Hierarchy
#'
#' Removes redundant (single-child / "bogus") codes from a hierarchy data frame
#' and adjusts the level indicators (`@`) accordingly.
#'
#' @param hier A data frame representing a hierarchy (with level `@` strings in the first column and codes in the second).
#'
#' @return A data frame with bogus codes removed and level indicators adjusted.
#' @export
remove_bogus <- function(hier) {
  a <- SSBtools::AutoHierarchies(list(a = hier))[[1]]
  bogus <- a$mapsFrom[unique_once_l(a$mapsTo)]
  bogus_rows <- hier[, 2] %in% bogus
  hier <- hier[!bogus_rows, , drop = FALSE]
  hier[, 1] <- fix_ats(hier[, 1])
  hier
}


# similar to SSBtools:::unique_once
unique_once_l <- function(x) {
  !duplicated(x) & !duplicated(x, fromLast = TRUE)
}


fix_ats <- function(ats) {
  n_ats <- nchar(ats)
  w <- which(diff(n_ats) > 1)
  if (!length(w)) {
    return(ats)
  }
  start <- w[1]
  end <- start + match(n_ats[start], n_ats[-seq_len(start)])
  if (is.na(end)) {
    end <- length(ats) + 1
  }
  idx_to_fix <- SSBtools::SeqInc(start + 1, end - 1)
  idx_to_fix
  ats[idx_to_fix] <- substring(ats[idx_to_fix], 2)
  fix_ats(ats)
}
