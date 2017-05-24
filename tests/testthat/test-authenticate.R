context("authenticate")

test_that("Correct HTTP request is made", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$method, "POST")
      expect_equal(req$url, "https://my.server.com/api/v1/signin")
      body = rawToChar(req$options$postfields)
      expect_equal(body, '{"username":"user1","password":"p@ssword"}')
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content='{"token":"s:abcdefgh.ijklmnop/pqr","userId":"592799bd14ac0ad59699cb77","admin":false,"flags":{}}',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      resp = authenticate("user1", "p@ssword")
      expect_equal(resp$admin, FALSE)
    }
  )
})
