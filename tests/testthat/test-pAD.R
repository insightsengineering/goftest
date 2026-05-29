test_that("pAD warns when input cannot be parsed as numeric", {
  expect_warning(pAD("wrong vector", 10))
})

test_that("pAD returns a similar value compared to an empirical distribution", {
  # This dataset contains values retrieved from original table
  # P. A. Lewis’s 1961 paper, "Distribution of the Anderson-Darling Statistic" (Annals of Mathematical Statistics, 32, 1118–1124)
  empirical_cdf <- read.csv("anderson_darling_empirical_cdf.csv")

  result_n8_z0.375 <- pAD(0.375, n = 8)
  empirical_n8_z0.375 <- empirical_cdf[empirical_cdf[["z"]] == 0.375, "n8"]
  expect_identical(round(result_n8_z0.375, digits = 3), empirical_n8_z0.375)

  result_n7_z0.425 <- pAD(0.425, n = 7)
  empirical_n7_z0.425 <- empirical_cdf[empirical_cdf[["z"]] == 0.425, "n7"]
  expect_identical(round(result_n7_z0.425, digits = 3), empirical_n7_z0.425)
})

test_that("pAD returns 1 for q = Inf", {
  expect_equal(pAD(Inf), 1)
})

test_that("pAD works with n = Inf and fast = TRUE (default)", {
  result <- pAD(1.5)
  expect_true(result > 0 && result < 1)
})

test_that("pAD works with n = Inf and fast = FALSE (exact algorithm)", {
  result_fast   <- pAD(1.5, fast = TRUE)
  result_slow   <- pAD(1.5, fast = FALSE)
  expect_true(result_slow > 0 && result_slow < 1)
  expect_equal(result_fast, result_slow, tolerance = 1e-4)
})

test_that("pAD lower.tail = FALSE returns the survival probability", {
  expect_equal(pAD(1.5, lower.tail = FALSE), 1 - pAD(1.5))
})


test_that("qAD returns a similar quantile compared to an empirical distribution", {
  # This dataset contains values retrieved from original table
  # P. A. Lewis’s 1961 paper, "Distribution of the Anderson-Darling Statistic" (Annals of Mathematical Statistics, 32, 1118–1124)
  empirical_quantiles <- read.csv("anderson_darling_empirical_quantile.csv")

  result_n2_z0.90 <- qAD(0.90, 2)
  empirical_n2_z0.90 <- empirical_quantiles[empirical_quantiles[["z"]] == 0.90, "n2"]
  expect_identical(round(result_n2_z0.90, digits = 2), round(empirical_n2_z0.90, digits = 2))

  result_n5_z0.95 <- qAD(0.95, 5)
  empirical_n5_z0.95 <- empirical_quantiles[empirical_quantiles[["z"]] == 0.95, "n5"]
  expect_identical(round(result_n5_z0.95, digits = 2), round(empirical_n5_z0.95, digits = 2))
})

test_that("qAD lower.tail = FALSE returns the correct quantile", {
  expect_equal(qAD(0.1, lower.tail = FALSE), qAD(0.9))
})
