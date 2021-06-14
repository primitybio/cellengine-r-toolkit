context("covarToParameters")

test_that("returns ellipse parameters from covariance matrix", {
  covar <- matrix(c(6879, 3612, 3612, 5215), nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("FSC-A", "FSC-W"),
                         c("FSC-A", "FSC-W")))
  params <- covarToParameters(covar)

  expect_equal(params$minor, 98.8, tolerance = 0.001)
  expect_equal(params$major, 48.4, tolerance = 0.001)
  expect_equal(params$angle, -0.899, tolerance = 0.001)
})
