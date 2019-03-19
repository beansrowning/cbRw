#' @title Coupled Biased Random Walks  
#'  TODO
#' @param data a data.frame containing catgorical data
#' @value the input data frame with an additional \emph{score} variable representing
#'        relative outlier-ness of the observation
#' @export
cbrw <- function(data) {
  # TODO: defensive programming for type, id vars, high-dimensionality

  # Set up and compute the biased transistion matrix
  # returns a list with Wb, nodes, and edges
  computed <- biased_trans_matrix(data, all_data = TRUE)

  # Run random walk on Wb
  pi_t <- random_walk(computed$Wb)

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

  return(data)

}