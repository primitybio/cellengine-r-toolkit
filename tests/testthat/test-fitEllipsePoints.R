context("fitEllipsePoints")

test_that("fits an ellipse to given points", {
  points <- t(data.frame(list(
    c(232913.3, 193084.0),
    c(148847.1, 216930.3),
    c(39857.19, 163833.57),
    c(-30211.74, 64897.12),
    c(-20314.21, -21923.39),
    c(63751.93, -45769.68),
    c(172741.885, 7327.087),
    c(242810.8, 106263.5)
  )))

  results <- fitEllipsePoints(points)
  expected <- matrix(as.numeric(list(6.081507e-11, -2.893471e-11, -2.893471e-11, 7.032573e-11)), ncol=2)
  expect_equal(expected, results$covar)
  expect_equal(as.numeric(list(106299.54, 85580.32)), results$center, tolerance = 0.005)
  expect_equal(as.numeric(list(166096.6, 102655.5)), results$axes, tolerance = 0.005)
  expect_equal(0.7039527, results$angle, tolerance = 0.005)
})
