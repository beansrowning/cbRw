context("Algorithm accuracy")

test_that("Score values are equal to canonical", {

  # Canonical values pulled from running cbrw test data through
  # the python CBRW package, dkaslovsky/Coupled-Biased-Random-Walks
  canonical <- c(
    0.123556072282254,
    0.0504085179712585,
    0.0476111020550283,
    0.0495085666704524,
    0.0844538406306188,
    0.0468666953948249,
    0.0476111020550283,
    0.0493436236504131,
    0.0459772220233669,
    0.0694800337106498,
    0.0495038674807341,
    0.0668533395540888
  )

  # Process example dataset and compare values
  data(cbrw_example)
  cbrw_example <- cbrw(cbrw_example)

  expect_equal(cbrw_example$score, canonical)
})