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

# TODO: Make a nodes + Wb -> tbl_graph and viz
# Just scrap code which could become a func eventually
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
