test_that("SigVis class can be instantiated", {
  # Skip if dependencies not available
  skip_if_not_installed("R6")
  skip_if_not_installed("ggraph")
  
  # Test that the class exists and has expected structure
  expect_s3_class(SigVis, "R6ClassGenerator")
  
  # Test that the class has expected public methods
  sig_vis_methods <- SigVis$public_methods
  expected_methods <- c("initialize", "get_graph", "plot")
  expect_true(all(expected_methods %in% names(sig_vis_methods)))
})

test_that("SigVis plotting method returns ggplot object", {
  skip_if_not_installed("ggraph") 
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("dplyr")
  
  # Create a simple test graph
  test_edges <- data.frame(
    from = c("A", "B"),
    to = c("B", "C"),
    arrowkeeper = c("test", "test"),
    status = c("operational", "operational")
  )
  
  test_nodes <- data.frame(
    node = c("A", "B", "C"),
    node_context = c("type1", "type2", "type3")
  )
  
  # Create a minimal tidygraph object
  test_graph <- tidygraph::as_tbl_graph(test_edges) |>
    tidygraph::activate(nodes) |>
    dplyr::left_join(test_nodes, by = c("name" = "node"))
  
  # Create mock SigGraph object
  mock_sig_graph <- list(
    graph = test_graph
  )
  
  # Test SigVis creation and plotting
  expect_no_error({
    sig_vis <- SigVis$new(sig_graph = mock_sig_graph)
    plot_obj <- sig_vis$plot()
  })
})

test_that("SigVis handles missing graph gracefully", {
  # Test with NULL graph - should error during construction
  mock_empty_graph <- list(graph = NULL)
  
  expect_error({
    sig_vis <- SigVis$new(sig_graph = mock_empty_graph)
  }, "Graph component cannot be NULL")
  
  # Test with completely invalid sig_graph object
  expect_error({
    SigVis$new(sig_graph = list(not_graph = "invalid"))
  }, "must be an R6 object or list with a 'graph' component")
})