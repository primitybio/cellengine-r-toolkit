context("utils")

# test_that("Sets the UA correctly", {})

test_that("baseGet requires baseURL to have been set", {
  {
    pkg.env$baseURL = ""
    expect_error(baseGet(), "setServer")
  }
})

test_that("basePut requires baseURL to have been set", {
  {
    pkg.env$baseURL = ""
    expect_error(basePut(), "setServer")
  }
})

test_that("basePost requires baseURL to have been set", {
  {
    pkg.env$baseURL = ""
    expect_error(basePost(), "setServer")
  }
})

test_that("baseDelete requires baseURL to have been set", {
  {
    pkg.env$baseURL = ""
    expect_error(baseDelete(), "setServer")
  }
})
