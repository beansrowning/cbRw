#' @title Generate Biased Transition Matrix
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
#' @import tidyr
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
    dplyr::select(u, v, p) %>%
    tidyr::spread(v, p, fill = 0L) %>%
    dplyr::select(-u) %>%
    as.matrix()

  # Compute A(u,v)
  A[lower.tri(A)] <- t(A)[lower.tri(A)]
  A <- A / diag(A)
  A <- t(A)
  diag(A) <- 0

  # Compute Wb(u,v) for all values
  # Wb(u,v) = (sigma(v) * A(u,v)) / SUM(sigma(v) * A(u,v), forall v in V)
  delta_v <- nodes[["intra_dev"]]
  Wb <- t(t(A) * delta_v)
  Wb <- Wb / rowSums(Wb)

  if (!all_data) {
    return(Wb)
  }

  # If we want additional data, tidy things up a bit
  nodes <- nodes %>%
    mutate(name = feature_values[u]) %>%
    select(feature, name, freq, p, intra_dev, is_mode)

  out <- list(
    nodes = nodes,
    edges = edges,
    Wb = Wb
  )

  return(out)
}

# TODO: Make a nodes + Wb -> tbl_graph and viz
# function
plot_cbrw <- function(...) {
  # TODO
  hollow_edges <- bind_rows(
    select(edges, u, v, group),
    select(edges, v = u, u = v, group)
  )

  new_edges <- Wb %>%
    as.data.frame() %>%
    setNames(seq_along(.)) %>%
    mutate(u = row_number()) %>%
    gather("v", "weight", -u) %>%
    mutate(v = as.integer(v)) %>%
    right_join(hollow_edges, by = c("u", "v")) %>%
    rename(from = u, to = v)

  graph_out <- tbl_graph(
    nodes = rename(nodes, from = u, to = v),
    edges = new_edges
  )

  graph_plot <- graph_out %>%
    ggraph("drl") +
    geom_edge_arc(
      arrow = arrow(length = unit(4, "mm")),
      aes(edge_width = (weight / max(weight)) * 0.01),
      alpha = 0.4
    ) +
    geom_node_point(
      aes(size = intra_dev),
      color = "red"
    ) +
    geom_node_text(
      aes(label = feature_values[to]),
      vjust = -0.6
    )
}
