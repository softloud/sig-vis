test_that("SigGraph class can be instantiated", {
  # Skip if dependencies not available
  skip_if_not_installed("R6")
  skip_if_not_installed("tidygraph")
  
  # Test that the class exists and has expected structure
  expect_s3_class(SigGraph, "R6ClassGenerator")
  
  # Test that the class has expected public methods
  sig_graph_methods <- SigGraph$public_methods
  expected_methods <- c("initialize", "get_graph", "refresh", "node_count", "edge_count", "set_aggregation")
  expect_true(all(expected_methods %in% names(sig_graph_methods)))
})

test_that("SigGraph can work with template data", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  # Create a mock SigDat-like object for testing
  mock_sig_dat <- list(
    get_edges = function() template_edges,
    get_nodes = function() template_nodes
  )
  
  # Test that we can create a SigGraph with mock data
  expect_no_error({
    sig_graph <- SigGraph$new(sig_dat = mock_sig_dat)
  })
})

test_that("Graph building logic validates input data", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  # Test with invalid edges (missing required columns)
  invalid_edges <- data.frame(
    source = c("A", "B"),
    target = c("B", "C")
  )
  
  invalid_nodes <- data.frame(
    id = c("A", "B", "C"),
    type = c("type1", "type2", "type3")
  )
  
  mock_invalid_dat <- list(
    get_edges = function() invalid_edges,
    get_nodes = function() invalid_nodes
  )
  
  # This should handle the case gracefully or provide informative error
  expect_error({
    sig_graph <- SigGraph$new(sig_dat = mock_invalid_dat)
  }, class = "error")
})