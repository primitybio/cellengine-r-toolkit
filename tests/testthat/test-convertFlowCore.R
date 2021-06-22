context("convertFlowCore")
library("mockery")
capture_warnings({
  library("flowCore")
  library("flowDensity")
})

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

 test_that("flowDensity is converted to polygonGate", {
   # given
   content='{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[222400,94720],[216128,85568],[201344,77696],[194752,75264],[19584,67904],[19072,68864],[20800,75648],[25920,93312],[37184,101120],[51648,104576],[82816,109824],[95040,111488],[107712,112512],[144384,114304],[172224,114240],[176640,113920],[190336,112512],[197632,111040],[211072,104448],[222400,94720]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}'
   gate <- jsonlite::fromJSON(content)
   # createPolygonGate is stubbed to avoid API call:
   stub(convertFromFlowCore, 'createPolygonGate', gate)

   f <- read.FCS("../5k.fcs", transformation = "linearize")
   flow <- flowDensity(f, channels = c("FSC-A","FSC-W"),position = c(F,F),
                       percentile = c(.99999,.99999),use.percentile = c(T,T),
                       ellip.gate = T,scale = .99 )

   scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-A","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"FSC-W","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]'
   scaleSet <- jsonlite::fromJSON(scaleData)

   # when
   polygonGate <- convertFromFlowCore(flow, scaleSet, "converted gate")

   # then
   expect_equal(convertFromFlowCore(flow, scaleSet, "converted gate"), gate)
   expect_equal(polygonGate$name, "converted gate") # this is returned as [gate: {}, population: {}] by CE
   expect_true(all((flow@filter == gate$model$polygon$vertices)))
 })

 test_that("flowCore gate is converted to CE gate", {
   # given
   sqrcut <- matrix(c(300,300,600,600,50,300,300,50),ncol = 2,nrow = 4)
   colnames(sqrcut) <- c("FSC-H","SSC-H")
   pg <- polygonGate(filterId="nonDebris", .gate = sqrcut)

   content='{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[300,50],[300,300],[600,300],[600,50]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-A","type":"PolygonGate","name":"converted gate","parentPopulationId":null,"yChannel":"FSC-W","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}'
   gate <- jsonlite::fromJSON(content)

   # createPolygonGate is stubbed to avoid API call:
   stub(convertFromFlowCore, 'createPolygonGate', gate)

   scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-H","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"SSC-H","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]'
   scaleSet <- jsonlite::fromJSON(scaleData)

   # when
   polygonGate <- convertFromFlowCore(pg, scaleSet, "converted gate")

   # then
   expect_true(all((pg@boundaries == gate$model$polygon$vertices)))
 })

 test_that("ellipsoidGate is converted to CE EllipseGate", {
   # given
   cov <- matrix(c(6879, 3612, 3612, 5215), ncol = 2,
                 dimnames = list(c("FSC-H", "SSC-H"), c("FSC-H", "SSC-H")))
   mean <- c("FSC-H" = 430, "SSC-H" = 175)
   eg <- ellipsoidGate(filterId = "myEllipsoidGate", .gate = cov, mean = mean)

   content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-H","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}'
   gate <- jsonlite::fromJSON(content)

   # createPolygonGate is stubbed to avoid API call:
   stub(convertFromFlowCore, 'createEllipseGate', gate)

   scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-H","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"SSC-H","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]'
   scaleSet <- jsonlite::fromJSON(scaleData)

   # when
   ellipseGate <- convertFromFlowCore(eg, scaleSet, "converted gate")

   # then
   expect_equal(gate$type, "EllipseGate")
 })

test_that("ellipse gate makes correct round-trip: CE -> flowCore -> CE", {
  # given
  content <- '{"__v":0,"experimentId":"5d2f8b4b21fd0676fb3a6a70","model":{"label":[106299.536082474,85580.3298969073],"ellipse":{"angle":0.7039529178881421,"major":166096.6309940297,"minor":102655.51977381333,"center":[106299.53608247427,85580.32989690728]},"locked":false},"gid":"59289ff2461f1fd925fca4ff","xChannel":"FSC-H","type":"EllipseGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"59289ff59989cc7704ada3c0","tailoredPerFile":false,"id":"59289ff59989cc7704ada3c0"}'
  gate <- jsonlite::fromJSON(content)

  # createPolygonGate is stubbed to avoid API call:
  stub(convertFromFlowCore, 'createEllipseGate', gate)

  scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-H","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"SSC-H","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]'
  scaleSet <- jsonlite::fromJSON(scaleData)

  # when
  flowGate <- convertToFlowCore(gate)
  newCEGate <- convertFromFlowCore(flowGate, scaleSet, "hek 2")


  # then
  m1 = gate$model$ellipse
  m2 = newCEGate$model$ellipse # this is newGate$gate$model$population when hitting the real API
  expect_equal(m1$angle, m2$angle)
  expect_equal(m1$major, m2$major)
  expect_equal(m1$minor, m2$minor)
  expect_equal(unlist(m1$center)[1], unlist(m2$center)[1])
  expect_equal(unlist(m1$center)[2], unlist(m2$center)[2])
})

test_that("polygon gate makes correct round-trip: CE -> flowCore -> CE", {
  # given
  content='{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"polygon":{"vertices":[[37836.07,971.51],[1588732.12,154.646],[8139.405,664.78],[9441.949,781.32]]},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-H","type":"PolygonGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}'
  gate <- jsonlite::fromJSON(content)

  # createPolygonGate is stubbed to avoid API call:
  stub(convertFromFlowCore, 'createPolygonGate', gate)

  scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-H","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"SSC-H","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]'
  scaleSet <- jsonlite::fromJSON(scaleData)

  # when
  flowGate <- convertToFlowCore(gate)
  newCEGate <- convertFromFlowCore(flowGate, scaleSet, "test polygon gate")

  # then
  m1 = gate$model$polygon
  m2 = newCEGate$model$polygon # this is newGate$gate$model$population when hitting the real API
  expect_equal(m1, m2)
})

test_that("rectangle gate makes correct round-trip: CE -> flowCore -> CE", {
 # given
 content = '{"__v":0,"experimentId":"591a3b441d725115208a6fda","model":{"label":[150440.453608247,202688.886597938],"rectangle":{"y2":214399.74226804124,"x2":182870.51546391752,"y1":190978.03092783503,"x1":118010.39175257733},"locked":false},"gid":"592640a5a6a1d6256ec9b08a","xChannel":"FSC-H","type":"RectangleGate","name":"my gate","parentPopulationId":null,"yChannel":"SSC-H","_id":"592640aa298f1480900e10e4","tailoredPerFile":false,"id":"592640aa298f1480900e10e4"}'
 gate <- jsonlite::fromJSON(content)

 # createPolygonGate is stubbed to avoid API call:
 stub(convertFromFlowCore, 'createRectangleGate', gate)

 scaleData <- '[{"_id":"5d2f8b4b21fd0676fb3a6a8c","experimentId":"5d2f8b4b21fd0676fb3a6a70","name":"Scale Set 1","scales":[{"channelName":"FSC-H","scale":{"type":"LogScale","minimum":1,"maximum":100000}},{"channelName":"SSC-H","scale":{"type":"LinearScale","minimum":1,"maximum":262144}},{"channelName":"SSC-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax488-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-A","scale":{"type":"ArcSinhScale","minimum":-500,"maximum":262144,"cofactor":150}},{"channelName":"PE-TR-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PerCP-Cy55-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PE-Cy7-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax647-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax700-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Ax750-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacBlu-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot525-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"PacOrange-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot605-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot655-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Qdot705-A","scale":{"type":"ArcSinhScale","minimum":-200,"maximum":262144,"cofactor":150}},{"channelName":"Time","scale":{"type":"LinearScale","minimum":1,"maximum":262144}}],"__v":1,"updated":"2020-10-27T18:38:55.554Z"}]'
 scaleSet <- jsonlite::fromJSON(scaleData)

 # when
 flowGate <- convertToFlowCore(gate)
 newCEGate <- convertFromFlowCore(flowGate, scaleSet, "test rectangle gate")

 # then
 m1 = gate$model$rectangle
 m2 = newCEGate$model$rectangle
 expect_equal(m1$x1, m2$x1)
 expect_equal(m1$x2, m2$x2)
 expect_equal(m1$y1, m2$y1)
 expect_equal(m1$y2, m2$y2)
})
