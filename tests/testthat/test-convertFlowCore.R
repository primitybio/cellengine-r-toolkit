context("convertFlowCore")
library("flowCore")

test_that("rectangle gate is converted to flowCore", {
  content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"RectangleGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}'
  gate <- jsonlite::fromJSON(content)

  flowGate <- convertToFlowCore(gate)
  expect_equal(flowGate@filterId, "my gate")
  expect_true(flowGate@min[1] == gate$model$rectangle$x1)
  expect_true(flowGate@min[2] == gate$model$rectangle$y1)
  expect_true(flowGate@max[1] == gate$model$rectangle$x2)
  expect_true(flowGate@max[2] == gate$model$rectangle$y2)
})

test_that("ellipse gate is converted to flowCore", {
  content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-A","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}'
  gate <- jsonlite::fromJSON(content)

  flowGate <- convertToFlowCore(gate)
  expect_equal(flowGate@filterId, "my gate")
  expect_equal(flowGate@mean[1], gate$model$ellipse$center[1], tolerance=0.001, check.names=F)
  expect_equal(flowGate@mean[2], gate$model$ellipse$center[2], tolerance=0.001, check.names=F)
})

test_that("polygon gate is converted to flowCore", {
  content='{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}'
  gate <- jsonlite::fromJSON(content)

  flowGate <- convertToFlowCore(gate)
  expect_equal(flowGate@filterId, "my gate")
  expect_true(all(flowGate@boundaries == gate$model$polygon$vertices))
})
