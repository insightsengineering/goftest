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

# The tests below are ported from sciPy repo:
# https://github.com/scipy/scipy/blob/1e9a17b001c24e833eed24622741db8af92dc198/scipy/stats/tests/test_hypotests.py#L108
# the expected values of the cdfs are taken from Table 1 in
# Csorgo / Faraway: The Exact and Asymptotic Distribution of
# Cramér-von Mises Statistics, 1996.
describe("TestCvm", {
  dat <- list(
    list(n = 4, x = c(0.02983, 0.04111, 0.12331, 0.94251), ref = c(0.01, 0.05, 0.5, 0.999)),
    list(n = 10, x = c(0.02657, 0.03830, 0.12068, 0.56643), ref = c(0.01, 0.05, 0.5, 0.975)),
    list(n = 1000, x = c(0.02481, 0.03658, 0.11889, 1.16120), ref = c(0.01, 0.05, 0.5, 0.999)),
    list(n = Inf, x = c(0.02480, 0.03656, 0.11888, 1.16204), ref = c(0.01, 0.05, 0.5, 0.999))
  )

  lapply(dat, function(d) {
    it(paste("returns correct quantiles for n =", d$n), {
      expect_equal(pCvM(d$x, n = d$n), d$ref, tolerance = 1e-4)
    })
  })

  # test_cdf_support: CDF has support on [1/(12*n), n/3]
  it("returns 0 and 1 at the boundaries of the CDF support", {
    n <- 533
    expect_equal(pCvM(1 / (12 * n), n = n), 0)
    expect_equal(pCvM(n / 3, n = n), 1)

    n <- 27
    expect_equal(pCvM(1 / (12 * (n + 1)), n = n), 0)
    expect_equal(pCvM((n + 1) / 3, n = n), 1)
  })

  # test_cdf_large_n: asymptotic CDF and CDF for large samples should be close
  it("asymptotic CDF closely matches CDF for large n", {
    x <- c(0.02480, 0.03656, 0.11888, 1.16204, 100)
    expect_equal(pCvM(x, n = 10000), pCvM(x, n = Inf), tolerance = 1e-4)
  })

  # test_large_x: for large x and n, the series converges slowly;
  # cdf should be in (0.99999, 1] for x = 333.3 and n = 1000
  it("returns a value just below 1 for large x near the upper support boundary", {
    x <- 333.3
    p_finite <- pCvM(x, n = 1000)
    p_asymp  <- pCvM(x, n = Inf)
    expect_gt(p_finite, 0.99999)
    expect_lte(p_finite, 1.0)
    expect_gt(p_asymp, 0.99999)
    expect_lte(p_asymp, 1.0)
  })
})
