context("covarToParameters")

test_that("returns ellipse parameters from covariance matrix", {
  covar <- matrix(c(6879, 3612, 3612, 5215), nrow = 2, ncol = 2, byrow = TRUE,
         dimnames = list(c("FSC-A", "FSC-W"),
                         c("FSC-A", "FSC-W")))
  params <- covarToParameters(covar)

  print(params)
  expect_equal(params$minor, 48.37784, tolerance = 0.001)
  expect_equal(params$major, 98.76024, tolerance = 0.001)
  expect_equal(params$angle, 0.6722, tolerance = 0.001)
})
