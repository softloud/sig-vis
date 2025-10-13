#' Template edges data for SIG visualization
#'
#' A dataset containing example edge relationships for structured intelligence
#' governance networks. This data represents connections between different
#' components in a governance system.
#'
#' @format A data frame with 12 rows and 5 variables:
#' \describe{
#'   \item{from}{Character. Source node of the edge}
#'   \item{to}{Character. Target node of the edge}
#'   \item{to_minimum_requirements}{Character. Description of minimum requirements}
#'   \item{arrowkeeper}{Character. Entity responsible for the connection}
#'   \item{status}{Character. Current status of the connection}
#' }
#' 
#' @source Template data for demonstration and testing purposes
"template_edges"

#' Template nodes data for SIG visualization  
#'
#' A dataset containing example node information for structured intelligence
#' governance networks. This data represents different components and their
#' contextual classifications in a governance system.
#'
#' @format A data frame with 8 rows and 2 variables:
#' \describe{
#'   \item{node}{Character. Name of the node}
#'   \item{node_context}{Character. Contextual category of the node}
#' }
#' 
#' @source Template data for demonstration and testing purposes  
"template_nodes"