#' SIG Graph Constructor
#'
#' An R6 class for constructing and manipulating graph objects from 
#' structured intelligence governance data using tidygraph.
#'
#' @description
#' The `SigGraph` class creates tidygraph objects from SIG data, with
#' support for node aggregation and graph manipulation operations.
#'
#' @details
#' This class takes SigDat objects and converts them into tidygraph
#' networks, supporting different aggregation strategies and providing
#' methods for graph analysis.
#'
#' @examples
#' \dontrun{
#' # Create from SigDat object
#' sig_dat <- SigDat$new(source = "template")
#' sig_graph <- SigGraph$new(sig_dat = sig_dat)
#' graph <- sig_graph$get_graph()
#' 
#' # Check graph properties
#' sig_graph$node_count()
#' sig_graph$edge_count()
#' }
#'
#' @export
SigGraph <- R6::R6Class(
  "SigGraph",
  public = list(
    #' @field sig_dat SigDat object containing the source data
    sig_dat = NULL,
    #' @field edges Data frame of edges for graph construction
    edges = NULL,
    #' @field nodes Data frame of nodes for graph construction  
    nodes = NULL,
    #' @field graph tidygraph object representing the network
    graph = NULL,
    #' @field aggregation Character vector of aggregation options
    aggregation = c("none", "node_context"),
    
    #' Initialize SigGraph object
    #'
    #' @param sig_dat SigDat object containing nodes and edges data
    #' @param source Character. Data source identifier (unused in current implementation)
    #' @return A new `SigGraph` object
    initialize = function(sig_dat = NULL, source = "template") {
      # Input validation
      if (is.null(sig_dat)) {
        stop("'sig_dat' cannot be NULL")
      }
      
      if (!is.list(sig_dat) || !all(c("get_edges", "get_nodes") %in% names(sig_dat))) {
        stop("'sig_dat' must have get_edges() and get_nodes() methods")
      }
      
      self$sig_dat <- sig_dat
      self$edges <- self$sig_dat$get_edges()
      self$nodes <- self$sig_dat$get_nodes()
      
      # Validate data before building graph
      private$validate_graph_data()
      
      self$graph <- private$build_graph()
      invisible(self)
    },
    
    #' Get the tidygraph object
    #'
    #' @return tidygraph object representing the network
    get_graph = function() self$graph,
    
    #' Refresh the graph
    #'
    #' @return Invisibly returns self for method chaining
    refresh = function() {
      # Rebuild the graph based on updated edges and nodes
      self$graph <- private$build_graph()
      invisible(self)
    },
    
    #' Count nodes in the graph
    #'
    #' @return Integer number of nodes
    node_count = function() {
      if (is.null(self$graph)) {
        return(0)
      }
      
      if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Install tidygraph")
      }
      
      tidygraph::activate(self$graph, nodes) |>
        nrow()
    },
    
    #' Count edges in the graph
    #'
    #' @return Integer number of edges
    edge_count = function() {
      if (is.null(self$graph)) {
        return(0)
      }
      
      if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Install tidygraph")
      }
      
      tidygraph::activate(self$graph, edges) |>
        nrow()
    },
    
    #' Set node aggregation strategy
    #'
    #' @param aggregation Character. Either "none" or "node_context"
    #' @return Invisibly returns self for method chaining
    set_aggregation = function(aggregation = c("none", "node_context")) {
      aggregation <- match.arg(aggregation)
      
      if (aggregation == "node_context") {
        # Validate that node_context column exists
        if (!"node_context" %in% names(self$nodes)) {
          stop("node_context column not found in nodes data")
        }
        
        get_node_context <- function(node) {
          if (node %in% self$nodes$node_context) {
            return(node)
          } else {
            node_context <- self$nodes$node_context[self$nodes$node == node]
            if (length(node_context) == 0) {
              warning(paste("Node context not found for node:", node))
              return(NA) # Return NA if not found
            }
            return(node_context)
          }
        }

        # Update edges with new from/to values
        self$edges <- self$edges %>%
          dplyr::mutate(
            from = purrr::map_chr(from, get_node_context),
            to = purrr::map_chr(to, get_node_context)
          )

        context_nodes <- self$nodes %>%
          dplyr::select(node = node_context) %>%
          dplyr::distinct() %>%
          dplyr::mutate(node_context = node)

        self$nodes <- context_nodes

        # Check for NA values after mapping
        if (any(is.na(self$edges$from))) {
          warning("Some 'from' nodes could not be found in the nodes data frame.")
        }
        if (any(is.na(self$edges$to))) {
          warning("Some 'to' nodes could not be found in the nodes data frame.")
        }

        # Refresh the graph only when needed
        self$refresh() # Uncomment this line if you want to refresh automatically
      }
      
      invisible(self)
    }
  ),
  private = list(
    # Validate graph input data
    # Called internally to ensure data integrity
    validate_graph_data = function() {
      # Validate edges
      if (!is.data.frame(self$edges)) {
        stop("Edges must be a data frame")
      }
      
      required_edge_cols <- c("from", "to")
      if (!all(required_edge_cols %in% names(self$edges))) {
        stop("Edges must contain 'from' and 'to' columns")
      }
      
      # Validate nodes
      if (!is.data.frame(self$nodes)) {
        stop("Nodes must be a data frame")
      }
      
      if (!"node" %in% names(self$nodes)) {
        stop("Nodes must contain 'node' column")
      }
      
      # Check for empty data
      if (nrow(self$edges) == 0) {
        warning("No edges found in data")
      }
      
      if (nrow(self$nodes) == 0) {
        warning("No nodes found in data")
      }
      
      invisible(NULL)
    },
    
    build_graph = function() {
      if (!requireNamespace("tidygraph", quietly = TRUE)) {
        stop("Install tidygraph")
      }
      tidygraph::as_tbl_graph(self$edges) |>
        tidygraph::activate(nodes) |>
        dplyr::left_join(self$nodes, by = c("name" = "node")) |>
        dplyr::mutate(
          object = dplyr::if_else(node_context == "humans", node_context, "not human")
        )
    }
  )
)

# Example usage:
# sig_dat <- SigDat$new(source = "template")
# sig_graph <- SigGraph$new(sig_dat = sig_dat)
# g <- sig_graph$get_graph()
