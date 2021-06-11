context("getEllipsePoints")

test_that("returns 8 points on an ellipse", {
  content='{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}'
  gate <- jsonlite::fromJSON(content)
  ell <- gate$model$ellipse

  points <- getEllipsePoints(ell$angle, ell$major, ell$minor, ell$center[1], ell$center[2])
  expect_equal(8, length(points))
  expect_equal(2, length(points[[1]]))
})
