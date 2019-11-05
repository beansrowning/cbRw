#' @title Coupled Biased Random Walks  
#' @description TODO
#' @param data a data.frame containing catgorical data
#' @return the input data frame with an additional \emph{score} variable representing
#'        relative outlier-ness of the observation
#' @importFrom tidyr spread
#' @export
cbrw <- function(data) {
  # TODO: defensive programming for type, id vars, high-dimensionality

  # Set up and compute the biased transistion matrix
  # returns a list with Wb, nodes, and edges
  computed <- biased_trans_matrix(data, all_data = TRUE)

  # Run random walk on Wb
  pi_t <- as.matrix(random_walk(computed$Wb))

  # Compute feature relevance and use that to compute
  # the value score for each feature value
  computed$nodes <- computed$nodes %>%
    dplyr::mutate(
      value_score = c(pi_t)
   ) %>%
   dplyr::group_by(feature) %>%
   dplyr::mutate(
     rel = sum(value_score)
   ) %>%
   dplyr::ungroup() %>%
   dplyr::mutate(total_score = rel * value_score)

  # Using the integer tibble as an index, create a new tibble
  # with the value scores in place of each feature value int
  # then take the rowsum to compute the observation score
  obs_scores <- computed$data %>%
    dplyr::mutate_all(dplyr::funs(computed$nodes[["total_score"]][.])) %>%
    rowSums

  # Append the observation scores and return
  data <- data %>%
    dplyr::mutate(score = obs_scores)
  
  # Assign feature relevance to an attribute
  attr(data, "feature_rel") <- computed$nodes %>%
    dplyr::distinct(feature, rel) %>%
    tidyr::spread(feature, rel)

  return(data)
  
}