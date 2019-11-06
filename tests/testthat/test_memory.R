context("Memory Handling")

test_that("cbrw can handle large data", {
  # See #6, sparseMatrix revisions should
  # allow for much faster computation and
  # larger input data allowances
  set.seed(1234)
  
  # Generate some data at random
  synth_data <- as.data.frame(
    matrix(
      sample(
        c(letters, LETTERS, 1:1000),
        50000,
        replace = TRUE,
      ),
      ncol = 20L
    )
  )

  synth_data <- try(
    cbrw(synth_data),
    silent = TRUE
  )

  # Should pass if it can handle large(r) data
  # This would fail previous versions without sparseMatrix
  # and it would take /forever/
  expect_false(inherits(synth_data, "try-error"))
})