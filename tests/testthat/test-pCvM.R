describe("tests for CvM provability and quantile functions", {
  it("returns a valid qCvM quantile", {
    q <- qCvM(0.9)
    expect_true(is.numeric(q) && q > 0)
    expect_equal(pCvM(q), 0.9, tolerance = 1e-3)
  })

  it("returns matching qCvM quantiles for lower.tail inversion", {
    expect_equal(qCvM(0.1, lower.tail = FALSE), qCvM(0.9))
  })
})

# This test is ported from sciPy repo:
# https://github.com/scipy/scipy/blob/1e9a17b001c24e833eed24622741db8af92dc198/scipy/stats/tests/test_hypotests.py#L108
# the expected values of the cdfs are taken from Table 1 in
# Csorgo / Faraway: The Exact and Asymptotic Distribution of
# Cramér-von Mises Statistics, 1996.
dat <- list(
  list(n = 4, x = c(0.02983, 0.04111, 0.12331, 0.94251), ref = c(0.01, 0.05, 0.5, 0.999)),
  list(n = 10, x = c(0.02657, 0.03830, 0.12068, 0.56643), ref = c(0.01, 0.05, 0.5, 0.975)),
  list(n = 1000, x = c(0.02481, 0.03658, 0.11889, 1.16120), ref = c(0.01, 0.05, 0.5, 0.999)),
  list(n = Inf, x = c(0.02480, 0.03656, 0.11888, 1.16204), ref = c(0.01, 0.05, 0.5, 0.999))
)

lapply(dat, function(d) {
  test_that(paste("pCvM() returns correct quantiles for n =", d$n), {
    expect_equal(pCvM(d$x, n = d$n), d$ref, tolerance = 1e-4)
  })
})
