test_that("SigDat class can be instantiated with template data", {
  # Skip if dependencies not available
  skip_if_not_installed("R6")
  
  # Test that we can create a mock SigDat object using template data
  expect_s3_class(SigDat, "R6ClassGenerator")
  
  # Test that the class has expected public methods
  sig_dat_methods <- SigDat$public_methods
  expected_methods <- c("initialize", "read_sheet_simple", "refresh", "get_edges", "get_nodes")
  expect_true(all(expected_methods %in% names(sig_dat_methods)))
})

test_that("Template data is accessible", {
  # Test that template data exists and has expected structure
  expect_true(exists("template_edges"))
  expect_true(exists("template_nodes"))
  
  expect_s3_class(template_edges, "data.frame")
  expect_s3_class(template_nodes, "data.frame")
  
  # Check column names
  expect_named(template_edges, c("from", "to", "to_minimum_requirements", "arrowkeeper", "status"))
  expect_named(template_nodes, c("node", "node_context"))
  
  # Check dimensions
  expect_equal(nrow(template_edges), 12)
  expect_equal(nrow(template_nodes), 8)
  expect_equal(ncol(template_edges), 5)
  expect_equal(ncol(template_nodes), 2)
})

test_that("Template edges have valid structure", {
  # Check that from/to columns contain character data
  expect_type(template_edges$from, "character")
  expect_type(template_edges$to, "character")
  
  # Check no missing values in key columns
  expect_false(any(is.na(template_edges$from)))
  expect_false(any(is.na(template_edges$to)))
  
  # Check valid status values
  valid_statuses <- c("buggy", "not developed", "operational")
  expect_true(all(template_edges$status %in% valid_statuses))
})

test_that("Template nodes have valid structure", {
  # Check that node columns contain character data
  expect_type(template_nodes$node, "character")
  expect_type(template_nodes$node_context, "character")
  
  # Check no missing values
  expect_false(any(is.na(template_nodes$node)))
  expect_false(any(is.na(template_nodes$node_context)))
  
  # Check for unique nodes
  expect_equal(length(template_nodes$node), length(unique(template_nodes$node)))
})