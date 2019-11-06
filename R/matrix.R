#' @title Generate Biased Transition Matrix
#' @description
#' Takes in a data.frame of categorical data and returns a weighted transition matrix
#' which characterizes the edge weights of a directed graph representation of the inter-feature value couplings
#'
#' @param data a data.frame containing mosly categorical data
#' @param all_data a boolean (default: FALSE) on whether to return additional node and edge data (see Note)
#' @return a biased transition matrix of dim [k,k] where k is the number of unique feature values
#' @section Note:
#' If all_data is `TRUE`, A list with three members will be returned:
#' \itemize{
#'    \item \strong{nodes} a tibble containing all relevant information for each node, or feature value
#'    \item \strong{edges} a tibble containing all relevant information for each edge, or feature value coupling
#'    \item \strong{Wb} the biased transition matrix
#' }
#' @import dplyr
#' @import Matrix
#' @import rlang
#' @export
biased_trans_matrix <- function(data, all_data = FALSE) {
  stopifnot(is.data.frame(data))

  # === Convert to integer tibble ===
  # This takes all unique feature values as a single
  # vector and assigns them an integer value.
  # We then use that to convert those values within the
  # supplied tibble to integer values for ease of processing
  converted <- categorical_to_int(data)
  data <- converted[[1]]
  feature_values <- converted[[2]]

  # === Calculate intra-feature value couplings ===
  # See EQ 1
  # Forall v, calculate p(v)
  # determine mode, p(m)
  # delta(v), or intra-feature outlierness

  # for each f in F
  # calculate p(v) for all v in dom(f), p
  # then calculate delta(v), intra_dev
  # delta(v) = .5 * (dev(v) + base(m))
  # where dev(v) = (p(m) - p(v)) / p(m)
  # and base(m) = 1 - p(m)
  nodes <- intra_freq(data) %>%
    dplyr::group_by(feature) %>%
    dplyr::mutate(
      p = freq / sum(freq),
      is_mode = freq == max(freq),
      intra_dev = .5 * (((max(p) - p) / max(p)) + (1 - max(p)))
    ) %>%
    dplyr::ungroup()

  edges <- inter_freq(data) %>%
    dplyr::mutate(p = freq / dim(data)[1])

  # === Calculate inter-feature value couplings ===
  # see EQ 2
  # A(u,v) = p(u|v) = p(u,v) / p(v)
  # Here I've defined diag(A) = p(v) forall v in V
  # As a consequence, we can create a symmetric matrix
  # of p(u,v) values and then use simple matrix
  # division
  A <- bind_rows(nodes, edges) %>%
    dplyr::select(u, v, p)

  A <- Matrix::sparseMatrix(A$u, A$v, x = A$p)

  # Compute A(u,v)
  # HACK: A + t(A) and halfing the diagonal
  # is both faster and less memory intensive
  # than A[lower.tri(A)] <- t(A)[lower.tri(A)]
  A <- A + Matrix::t(A)
  Matrix::diag(A) <- Matrix::diag(A) / 2
  A <- A / Matrix::diag(A)
  A <- Matrix::t(A)
  Matrix::diag(A) <- 0

  # Compute Wb(u,v) for all values
  # Wb(u,v) = (sigma(v) * A(u,v)) / SUM(sigma(v) * A(u,v), forall v in V)
  delta_v <- nodes[["intra_dev"]]
  Wb <- Matrix::t(Matrix::t(A) * delta_v)
  Wb <- Wb / Matrix::rowSums(Wb)

  if (!all_data) {
    return(Wb)
  }

  # If we want additional data, tidy things up a bit
  nodes <- nodes %>%
    dplyr::mutate(name = feature_values[u]) %>%
    dplyr::select(feature, name, freq, p, intra_dev, is_mode)

  out <- list(
    nodes = nodes,
    edges = edges,
    Wb = Wb,
    data = converted[[1]]
  )

  return(out)
}

# Internal Random-walk function used to calculate the stationary probabilities
# pi*
random_walk <- function(trans_matrix, alpha = 0.95, err_tol = 0.001, max_iter = 100) {
  
  n <- dim(trans_matrix)[1L]
  dampen_vec <- rep(1, n) * ( (1 - alpha) / n )

  pi_t <- (1 / n) * rep(1, n)

  for (i in seq_len(max_iter)) {
    pi_next <- dampen_vec + (alpha * Matrix::t(trans_matrix) %*% pi_t)
    err <- Matrix::norm(pi_t - pi_next, type = "I")

    pi_t <- pi_next

    if (err <= err_tol) {
      break
    }
  }

  return(pi_t)
}