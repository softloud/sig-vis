#' SIG Visualization
#'
#' An R6 class for creating visualizations of structured intelligence 
#' governance networks using ggraph.
#'
#' @description
#' The `SigVis` class provides methods to visualize SIG networks with
#' customizable plotting options using ggraph and ggplot2.
#'
#' @details
#' This class takes SigGraph objects and creates publication-ready
#' network visualizations with appropriate theming and layout.
#'
#' @examples
#' \dontrun{
#' # Create visualization from graph
#' sig_dat <- SigDat$new(source = "template")
#' sig_graph <- SigGraph$new(sig_dat = sig_dat)
#' sig_vis <- SigVis$new(sig_graph = sig_graph)
#' 
#' # Create plot
#' plot <- sig_vis$plot()
#' print(plot)
#' }
#'
#' @export
SigVis <- R6::R6Class(
  "SigVis",
  public = list(
    #' @field sig_graph SigGraph object (currently unused)
    sig_graph = NULL,
    #' @field graph tidygraph object for visualization
    graph = NULL,
    
    #' Initialize SigVis object
    #'
    #' @param source Character. Data source identifier (unused)
    #' @param sig_graph SigGraph object containing the tidygraph to visualize
    #' @return A new `SigVis` object
    initialize = function(source = "template", sig_graph = NULL) {
      # Input validation
      if (is.null(sig_graph)) {
        stop("'sig_graph' cannot be NULL")
      }
      
      # Check if it's an R6 object with a graph field
      if (inherits(sig_graph, "R6") && "graph" %in% names(sig_graph)) {
        graph_obj <- sig_graph$graph
      } else if (is.list(sig_graph) && "graph" %in% names(sig_graph)) {
        graph_obj <- sig_graph$graph
      } else {
        stop("'sig_graph' must be an R6 object or list with a 'graph' component")
      }
      
      if (is.null(graph_obj)) {
        stop("Graph component cannot be NULL")
      }
      
      self$graph <- graph_obj
    },
    
    #' Get the graph object
    #'
    #' @return tidygraph object
    get_graph = function() {
      self$graph
    },
    
    #' Create network visualization plot
    #'
    #' @return ggplot object with network visualization
    plot = function() {
      # Validate graph before plotting
      if (is.null(self$graph)) {
        stop("No graph available for plotting")
      }
      
      if (!requireNamespace("ggraph", quietly = TRUE)) {
        stop("Install ggraph package for plotting")
      }
      
      if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Install ggplot2 package for plotting")
      }
      
      self$graph %>%
        ggraph::ggraph() +
        ggraph::geom_edge_fan(
          ggplot2::aes(linetype = arrowkeeper, color = status),
          alpha = 0.8
        ) +
        ggraph::geom_node_point(ggplot2::aes(shape = node_context),
          size = 20, alpha = 0.1
        ) +
        ggraph::geom_node_text(ggplot2::aes(label = name)) +
        ggplot2::theme_minimal(
          base_size = 10
        ) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank()
        ) +
        ggplot2::labs(
          x = "",
          y = "",
          title = "Structured intelligence governance",
          subtitle = "All elements"
        )
    }
  ),
  private = list()
)
