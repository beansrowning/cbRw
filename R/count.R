# TODO: Probably consolidate, or just build an R6 class
#' @title Counting helper functions
#' @name counting_helpers

#' @rdname counting_helpers
#' @param data a tibble of categorical data
#' @return a tibble containing all unique combinations of feature values
#'    with columns: \emph{u}, \emph{v}, \emph{freq}, emph{p}, and \emph{feature}
intra_freq <- function(data) {
  var_quos <- lapply(names(data), as.name)

  out <- vector(mode = "list", length = length(var_quos))
  i <- 1L
  for (var in var_quos) {
    out[[i]] <- data %>%
      dplyr::count(!!var) %>%
      dplyr::rename(v = !!var, freq = n) %>%
      dplyr::mutate(
        u = v,
        p = NA_real_,
        feature = rlang::quo_name(var),
      ) %>%
      dplyr::select(u, v, freq, p, feature)
      
    i <- i + 1L
  }
  
  out <- dplyr::bind_rows(out)

  return(out)
}

#' Helper function to calculate all bivariate frequencies
#' within the dataset
#' @param data a tibble of categorical data
#' @return a tibble containing all unique combinations of feature values
#'    with columns: \emph{u}, \emph{v}, \emph{freq}, and \emph{group}
#' @rdname counting_helpers
inter_freq <- function(data) {

  # Calculate all 2 variable combinations of variables
  # in the supplied data, and take each as a quosures
  col_combn <- combn(names(data), 2, simplify = FALSE)
  col_quos <- lapply(col_combn, lapply, as.name)
  col_combn <- vapply(col_combn, paste, collapse = "-", character(1))

  out <- vector(mode = "list", length = length(col_quos))
  i <- 1L

  # Iterating over each 2 variable combination
  # calculate bi-variate frequencies, p(u,v)
  for (comb in col_quos) {
    out[[i]] <- data %>%
      dplyr::count(!!!comb) %>%
      dplyr::mutate(
        group = col_combn[i]
      ) %>%
      setNames(c("u", "v", "freq", "group"))

    i <- i + 1L
  }

  out <- dplyr::bind_rows(out)

  return(out)
}
