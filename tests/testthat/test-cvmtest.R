describe("Argument verification for cvm.test", {
  it("throws error if 'x' is not the expected type", {
    expect_error(cvm.test(letters, "pnorm", mean = 0, sd = 1),
      regexp = "is.numeric(x) is not TRUE", fixed = TRUE)
  })

  it("throws error if 'estimated' is not the expected type", {
    expect_error(cvm.test(rnorm(10), "pnorm", mean = 0, sd = 1, estimated = "random"),
      regexp = "invalid argument type", fixed = TRUE)
  })

  it("throws error when CDF returns values outside [0,1]", {
    bad_cdf <- function(x, ...) rep(2, length(x))
    expect_error(cvm.test(1:10, bad_cdf),
      regexp = "null distribution function returned values outside [0,1]",
      fixed = TRUE)
  })

})

describe("The cvm.test should not reject the null hypothesis", {
  withr::local_seed(1)
  p_value_null_hypothesis <- 0.05

  it("does not reject with default parameters", {
    values <- rnorm(1000)
    result <- cvm.test(values, "pnorm")
    expect_true(p_value_null_hypothesis < result$p.value)
  })

  it("does not reject with defined parameters in normal distribution", {
    values <- rnorm(1000, mean = 5, sd = 1)
    result <- cvm.test(values, "pnorm", mean = 5, sd = 1)
    expect_true(p_value_null_hypothesis < result$p.value)
  })

  it("does not reject with estimated parameters in normal distribution", {
    values <- rnorm(2000, mean = 5, sd = 1)
    result <- cvm.test(values, "pnorm", mean = mean(values), sd = sd(values),
                      estimated = TRUE)
    expect_true(p_value_null_hypothesis < result$p.value)
  })

  it("does not reject with log normal distribution and default parameters", {
    values <- rlnorm(1000)
    result <- cvm.test(values, "plnorm")
    expect_true(p_value_null_hypothesis < result$p.value)
  })
})

describe("The cvm.test should reject the null hypothesis", {
  withr::local_seed(1)
  p_value_null_hypothesis <- 0.05

  it("does reject with default parameters with different distributions", {
    values <- rnorm(1000)
    result <- cvm.test(values, "plnorm")
    expect_true(p_value_null_hypothesis > result$p.value)
  })

  it("does reject with estimated parameters with different distributions", {
    values <- rnorm(1000, mean = 5, sd = 10)
    result <- cvm.test(values, "plnorm", mean = 5, sd = 10)
    expect_true(p_value_null_hypothesis > result$p.value)
  })
})
