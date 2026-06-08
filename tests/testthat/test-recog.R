test_that("recogniseCdf returns NULL for an unknown distribution name", {
  expect_null(recogniseCdf("random"))
})

test_that("recogniseCdf returns correct distribution function for a known distribution name", {
  expect_equal(recogniseCdf("gamma"), "Gamma distribution")
  expect_equal(recogniseCdf("binom"), "binomial distribution")
  expect_equal(recogniseCdf("cauchy"), "Cauchy distribution")
})

test_that("recogniseCdf removes 'p' on the provided string", {
  expect_equal(recogniseCdf("pgamma"), "Gamma distribution")
  expect_null(recogniseCdf("poison")) # This test should fail but the 'p' is removed
})
