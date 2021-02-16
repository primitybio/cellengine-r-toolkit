context("addAnnotation")

test_that("Correct HTTP request is made", {
 with_mock(
   `httr::request_perform` = function(req, handle, refresh) {
     switch(req$method,
           "PATCH" = {
              expect_equal(req$method, "PATCH")
              expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc")
              body = rawToChar(req$options$postfields)
              #expect_equal(body, '{"annotations":[{"type":"any","value":"myfirstvalue","name":"annotation 1"},{"name":"annotation 2","value":"mysecondvalue"},{"name":"annotation 3","value":2.12}]}')
              expect_equal(body, '{"annotations":[{"name":"annotation 1","value":"myfirstvalue"},{"name":"annotation 2","value":"myvalue"},{"name":"annotation 3","value":2.12}]}')
              response = httptest::fake_response(
                req$url,
                req$method,
                content='{"filename":"Specimen_001_A2_A02_MeOHperm(DL350neg).fcs","gridId":"593f3ae211005114b1bf03e2","hasFileInternalComp":true,"panelName":"Panel 1","__v":0,"_id":"591a3b441d725115208a6fdc","spillString":"8,Blue530-A,Vio450-A,Vio605-A,UV450-A,Red670-A,YG582-A,YG610-A,YG780-A,1,0,0,0,0,0,0,0,0,1,0.019999998552000027,0.055000000000000014,0,0,0,0,0.003000000000000001,0.012999999058800013,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1","crc32c":"76633d32","eventCount":164684,"md5":"4a0ded33c1f55c6ff52c90dbe3c4894d","experimentId":"591a3b441d725115208a6fda","panel":[{"index":1,"channel":"FSC-A"},{"index":7,"reagent":"CD3","channel":"Blue530-A"}],"annotations":[{"type":"any","value":"myfirstvalue","name":"annotation 1"},{"type":"any","value":"mysecondvalue","name":"annotation 2"},{"type":"any","value":2.12,"name":"annotation 2"}]}',
                status_code = 200,
                headers = list(`Content-Type` = "application/json")
              )
              return(response)
           },
           "GET" = {
              expect_equal(req$method, "GET")
              expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles/591a3b441d725115208a6fdc")
              response = httptest::fake_response(
                req$url,
                req$method,
                content='{"filename":"Specimen_001_A2_A02_MeOHperm(DL350neg).fcs","gridId":"593f3ae211005114b1bf03e2","hasFileInternalComp":true,"panelName":"Panel 1","__v":0,"_id":"591a3b441d725115208a6fdc","spillString":"8,Blue530-A,Vio450-A,Vio605-A,UV450-A,Red670-A,YG582-A,YG610-A,YG780-A,1,0,0,0,0,0,0,0,0,1,0.019999998552000027,0.055000000000000014,0,0,0,0,0.003000000000000001,0.012999999058800013,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1","crc32c":"76633d32","eventCount":164684,"md5":"4a0ded33c1f55c6ff52c90dbe3c4894d","experimentId":"591a3b441d725115208a6fda","panel":[{"index":1,"channel":"FSC-A"},{"index":7,"reagent":"CD3","channel":"Blue530-A"}],"annotations":[{"type":"any","value":"myfirstvalue","name":"annotation 1"}]}',
                status_code = 200,
                headers = list(`Content-Type` = "application/json")
              )
              return(response)
           },
           stop(sprintf("Unexpected request URL: %s", req$url))
           )
    },
    {
      setServer("https://my.server.com")
      annos = list(
        "annotation 2" = "myvalue",
        "annotation 3" = 2.12
      )
      resp = addAnnotation("591a3b441d725115208a6fda", "591a3b441d725115208a6fdc", annos)
    }
  )
})
