#' Helper fuction to transform a categorical dataset
#' into one represented by integers
#' @param data a tibble of categorical data
#' @return a list containing:
#' \itemize{
#'    \item character vector of all unique feature values (V)
#'    \item converted tibble with character values represented as integers
#' }
categorical_to_int <- function(data) {
  # Convert all observations into a
  # unique value which can be summarized into
  # a single vector of feature values, V
  # HACK: There has got to be a better way to do this
  data <- Map(
    f = paste,
    names(data),
    data,
    sep = "=="
  ) %>%
    dplyr::as_tibble()

  # Calculate V, the set of unique feature values
  # within the dataset
  all_fact <- unique(unlist(data))

  # Convert all categorical variables into integer by
  # first converting to factor with levels V
  # then coercing to its integer representation
  list(
    dplyr::as_tibble(data) %>%
      dplyr::mutate_all(
        dplyr::funs(
          as.integer(factor(., levels = all_fact))
        )
      ),
    all_fact
  )
}