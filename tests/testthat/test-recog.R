test_that("recogniseCdf returns NULL for an unknown distribution name", {
  expect_null(recogniseCdf("random"))
})
