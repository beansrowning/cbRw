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

test_that("Feature relevance is equal to canonical", {

  # Canonical values pre-computed and verified from the Python CBRW package
  # dkaslovsky/Coupled-Biased-Random-Walks
  canonical <- c(
    Education = 0.262728418353589,
    Gender = 0.16078750024988,
    Income = 0.293898197381611,
    Marriage = 0.282585884014921
  )

  # Process data and compare values
  data(cbrw_example)
  
  cbrw_output <- cbrw(cbrw_example[, names(cbrw_example) != "Cheat?"])
  cbrw_feature_rel <- unlist(feature_relevance(cbrw_output))

  expect_equal(cbrw_feature_rel, canonical)
})

test_that("Feature relevance sums to 1", {
  # Feature relevance should always sum to 1, else something has gone horribly awry
  data(cbrw_example)

  cbrw_output <- cbrw(cbrw_example)
  cbrw_feature_rel <- sum(unlist(feature_relevance(cbrw_output)))

  expect_equal(1, cbrw_feature_rel)
})