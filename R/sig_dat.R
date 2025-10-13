#' SIG Data Loader
#'
#' An R6 class for loading structured intelligence governance data from
#' Google Sheets. Supports both template and client configurations with
#' public sheet access (no authentication required).
#'
#' @description
#' The `SigDat` class provides methods to load nodes and edges data from
#' Google Sheets for structured intelligence governance visualization.
#' It supports environment-based configuration through .env files.
#'
#' @details
#' This class reads Google Sheets IDs from environment variables and loads
#' both nodes and edges data. It can work with template data or client-specific
#' configurations based on different .env files.
#'
#' @examples
#' \dontrun{
#' # Load template data
#' sig_dat <- SigDat$new(source = "template")
#' edges <- sig_dat$get_edges()
#' nodes <- sig_dat$get_nodes()
#'
#' # Refresh data
#' sig_dat$refresh()
#' }
#'
#' @export
SigDat <- R6::R6Class(
  "SigDat",
  public = list(
    #' @field edges Data frame containing edge data from Google Sheets
    edges = NULL,
    #' @field nodes Data frame containing node data from Google Sheets  
    nodes = NULL,
    #' @field sheet_id Google Sheets ID extracted from environment variables
    sheet_id = NULL,
    #' @field source Source type - either "template" or "client"
    source = NULL,
    
    #' Initialize SigDat object
    #'
    #' @param source Character. Either "template" or "client" to specify data source
    #' @param env_template Character. Path to template environment file
    #' @param env_client Character. Path to client environment file
    #' @return A new `SigDat` object
    initialize = function(source = c("template", "client"), env_template = "client_credentials/.env",
                          env_client = "client_credentials/.env-client") {
      # Input validation
      if (!is.character(source) || length(source) == 0) {
        stop("'source' must be a non-empty character vector")
      }
      
      self$source <- match.arg(source)
      env_file <- if (self$source == "template") env_template else env_client

      if (file.exists(env_file)) {
        if (!requireNamespace("dotenv", quietly = TRUE)) stop("Install 'dotenv'")
        dotenv::load_dot_env(env_file)
      }

      # Public sheet: no auth
      if (!requireNamespace("googlesheets4", quietly = TRUE)) stop("Install 'googlesheets4'")
      googlesheets4::gs4_deauth()

      self$sheet_id <- Sys.getenv("GS_SHEET_ID", unset = NA)
      if (is.na(self$sheet_id) || self$sheet_id == "") {
        url <- Sys.getenv("GS_DATA_ENTRY_URL", unset = "")
        m <- regmatches(url, regexpr("(?<=/d/)[A-Za-z0-9-_]+", url, perl = TRUE))
        if (length(m)) self$sheet_id <- m
      }
      if (is.na(self$sheet_id) || self$sheet_id == "") stop("Sheet ID not found in env vars.")

      self$edges <- self$read_sheet_simple("edges")
      self$nodes <- self$read_sheet_simple("nodes")
      
      # Validate loaded data
      private$validate_data()
    },
    
    #' Read sheet data
    #'
    #' @param sheet_name Character. Name of the sheet to read
    #' @return Data frame containing the sheet data
    read_sheet_simple = function(sheet_name) {
      if (!is.character(sheet_name) || length(sheet_name) != 1 || is.na(sheet_name)) {
        stop("'sheet_name' must be a single non-NA character string")
      }
      
      if (is.null(self$sheet_id) || is.na(self$sheet_id)) {
        stop("Sheet ID not available. Initialize object first.")
      }
      
      googlesheets4::read_sheet(ss = self$sheet_id, sheet = sheet_name, .name_repair = "minimal")
    },
    
    #' Refresh data from Google Sheets
    #'
    #' @return Invisibly returns self for method chaining
    refresh = function() {
      self$edges <- self$read_sheet_simple("edges")
      self$nodes <- self$read_sheet_simple("nodes")
      
      # Validate refreshed data
      private$validate_data()
      
      invisible(self)
    },
    
    #' Get edges data
    #'
    #' @return Data frame containing edges data
    get_edges = function() self$edges,
    
    #' Get nodes data
    #'
    #' @return Data frame containing nodes data
    get_nodes = function() self$nodes
  ),
  
  private = list(
    # Validate loaded data structure
    # Called internally to ensure data integrity
    validate_data = function() {
      # Validate edges data
      if (is.null(self$edges) || !is.data.frame(self$edges)) {
        stop("Edges data must be a data frame")
      }
      
      required_edge_cols <- c("from", "to")
      missing_edge_cols <- setdiff(required_edge_cols, names(self$edges))
      if (length(missing_edge_cols) > 0) {
        stop("Missing required edge columns: ", paste(missing_edge_cols, collapse = ", "))
      }
      
      # Check for NA values in critical columns
      if (any(is.na(self$edges$from)) || any(is.na(self$edges$to))) {
        warning("NA values found in edge from/to columns")
      }
      
      # Validate nodes data
      if (is.null(self$nodes) || !is.data.frame(self$nodes)) {
        stop("Nodes data must be a data frame")
      }
      
      required_node_cols <- c("node")
      missing_node_cols <- setdiff(required_node_cols, names(self$nodes))
      if (length(missing_node_cols) > 0) {
        stop("Missing required node columns: ", paste(missing_node_cols, collapse = ", "))
      }
      
      # Check for NA values in node names
      if (any(is.na(self$nodes$node))) {
        stop("NA values found in node names")
      }
      
      # Check for duplicate node names
      if (length(self$nodes$node) != length(unique(self$nodes$node))) {
        warning("Duplicate node names detected")
      }
      
      invisible(NULL)
    }
  )
)

# Example usage:
# sig_dat <- SigDat$new(source = "client")
# sig_dat$get_edges()
# sig_dat$get_nodes()
