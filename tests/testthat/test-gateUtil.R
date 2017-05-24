context("parsePopulationArgs")

test_that("returns the ID of a single matching population", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22name%22%29")
      expect_equal(req$method, "GET")
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7088","experimentId":"591a3b441d725115208a6fda","name":"name","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33b\\"]}","parentId":null,"terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      result = parsePopulationArgs(NULL, "name", "591a3b441d725115208a6fda")
      expect_equal(result, "591a3b5f1d725115208a7088")
    }
  )
})

test_that("passes through the parentPopulationId if provided", {
  {
    result = parsePopulationArgs("591a3b5f1d725115208a7088", NULL, "591a3b441d725115208a6fda")
    expect_equal(result, "591a3b5f1d725115208a7088")
  }
})

test_that("throws an error if multiple populations match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22name%22%29")
      expect_equal(req$method, "GET")
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7087","experimentId":"591a3b441d725115208a6fda","name":"name","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33a\\"]}","parentId":null,"terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7087"},
          {"_id":"591a3b5f1d725115208a7088","experimentId":"591a3b441d725115208a6fda","name":"name","gates":"{\\"$and\\":[\\"591a3b5961a8a2302d15a33b\\"]}","parentId":null,"terminalGateGid":"591a3b5961a8a2302d15a33a","__v":0,"id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parsePopulationArgs(NULL, "name", "591a3b441d725115208a6fda"), "More than one")
    }
  )
})

test_that("throws an error if both parentPopulationId and parentPopulation are provided", {
  {
    setServer("https://my.server.com")
    expect_error(
      parsePopulationArgs("591a3b441d725115208a6fdc", "name", "591a3b441d725115208a6fda"),
      "Please specify only one"
    )
  }
})

test_that("throws an error if no populations match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations?query=eq%28name%2C%20%22name%22%29")
      expect_equal(req$method, "GET")
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content = '[
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parsePopulationArgs(NULL, "name", "591a3b441d725115208a6fda"), "does not exist")
    }
  )
})

context("parseFcsFileArgs")

test_that("assigns the ID of a single matching fcsFile", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&fields=%2B_id")
      expect_equal(req$method, "GET")
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      result = parseFcsFileArgs(list(), TRUE, NULL, "name", "591a3b441d725115208a6fda")
      expect_equal(result$fcsFileId, jsonlite::unbox("591a3b5f1d725115208a7088"))
    }
  )
})

test_that("passes through the fcsFileId if provided", {
  {
    result = parseFcsFileArgs(list(), TRUE, "591a3b5f1d725115208a7088", NULL, "591a3b441d725115208a6fda")
    expect_equal(result$fcsFileId, jsonlite::unbox("591a3b5f1d725115208a7088"))
  }
})

test_that("throws an error if multiple fcsFiles match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&fields=%2B_id")
      expect_equal(req$method, "GET")
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content = '[
          {"_id":"591a3b5f1d725115208a7087"},
          {"_id":"591a3b5f1d725115208a7088"}
        ]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parseFcsFileArgs(list(), TRUE, NULL, "name", "591a3b441d725115208a6fda"), "More than one")
    }
  )
})

test_that("throws an error if both fcsFileId and fcsFile are provided", {
  {
    setServer("https://my.server.com")
    expect_error(
      parseFcsFileArgs(list(), TRUE, "591a3b441d725115208a6fdc", "name", "591a3b441d725115208a6fda"),
      "Please specify only one"
    )
  }
})

test_that("doesn't assign fcsFileId if not tailored", {
  {
    setServer("https://my.server.com")
    result = parseFcsFileArgs(list(), FALSE, NULL, "name", "591a3b441d725115208a6fda")
    expect_null(result$fcsFileId)
  }
})

test_that("doesn't assign fcsFileId if global tailored gate", {
  {
    setServer("https://my.server.com")
    result = parseFcsFileArgs(list(), TRUE, NULL, NULL, "591a3b441d725115208a6fda")
    expect_null(result$fcsFileId)
  }
})

test_that("throws an error if no fcsFiles match", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      expect_equal(req$url, "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/fcsfiles?query=eq%28filename%2C%20%22name%22%29&fields=%2B_id")
      expect_equal(req$method, "GET")
      response = httptest::fakeResponse(
        req$url,
        req$method,
        content = '[]',
        status_code = 200,
        headers = list(`Content-Type` = "application/json")
      )
      return(response)
    },
    {
      setServer("https://my.server.com")
      expect_error(parseFcsFileArgs(list(), TRUE, NULL, "name", "591a3b441d725115208a6fda"), "does not exist")
    }
  )
})

context("autoCreatePopulation")

test_that("it works given no parentPopulation or parentPopulationId", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations" = {
          expect_equal(req$method, "POST")
          body = rawToChar(req$options$postfields)
          expect_equal(body, '{"name":"my gate","gates":"{\\\"$and\\\":[\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":null}')
          response = httptest::fakeResponse(
            req$url,
            req$method,
            content = '{"experimentId":"591a3b441d725115208a6fda","name":"my gate","gates":"{\\\"$and\\\":[\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":null,"id":"592640aa298f1480900e10e4","_id":"592640aa298f1480900e10e4"}',
            status_code = 201,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      resp = autoCreatePopulation("591a3b441d725115208a6fda", "my gate", "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$name, "my gate")
      expect_equal(resp$gates, "{\"$and\":[\"592640a5a6a1d6256ec9b08a\"]}")
      expect_equal(resp$terminalGateGid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentId, NULL)
    }
  )
})

test_that("it works given a parentPopulation object; `gates` has empty `$and`", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations" = {
          expect_equal(req$method, "POST")
          body = rawToChar(req$options$postfields)
          expect_equal(body, '{"name":"my gate","gates":"{\\\"$and\\\":[\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba"}')
          response = httptest::fakeResponse(
            req$url,
            req$method,
            content = '{"experimentId":"591a3b441d725115208a6fda","name":"my gate","gates":"{\\\"$and\\\":[\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba","id":"592640aa298f1480900e10e4","_id":"592640aa298f1480900e10e4"}',
            status_code = 201,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      pop = list(
        `_id` = "591a3b441d725115208a6fba",
        gates = '{"$and":[]}',
        name = "popname"
      )
      resp = autoCreatePopulation("591a3b441d725115208a6fda", "my gate", "592640a5a6a1d6256ec9b08a", parentPopulation = pop)
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$name, "my gate")
      expect_equal(resp$gates, "{\"$and\":[\"592640a5a6a1d6256ec9b08a\"]}")
      expect_equal(resp$terminalGateGid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentId, "591a3b441d725115208a6fba")
    }
  )
})

test_that("it works given a parentPopulation object; `gates` has non-empty `$and`", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations" = {
          expect_equal(req$method, "POST")
          body = rawToChar(req$options$postfields)
          expect_equal(body, '{"name":"my gate","gates":"{\\\"$and\\\":[\\\"591a3b441d725115208a6fbf\\\",\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba"}')
          response = httptest::fakeResponse(
            req$url,
            req$method,
            content = '{"experimentId":"591a3b441d725115208a6fda","name":"my gate","gates":"{\\\"$and\\\":[\\\"591a3b441d725115208a6fbf\\\",\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba","id":"592640aa298f1480900e10e4","_id":"592640aa298f1480900e10e4"}',
            status_code = 201,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      pop = list(
        `_id` = "591a3b441d725115208a6fba",
        gates = '{"$and":["591a3b441d725115208a6fbf"]}',
        name = "popname"
      )
      resp = autoCreatePopulation("591a3b441d725115208a6fda", "my gate", "592640a5a6a1d6256ec9b08a", parentPopulation = pop)
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$name, "my gate")
      expect_equal(resp$gates, "{\"$and\":[\"591a3b441d725115208a6fbf\",\"592640a5a6a1d6256ec9b08a\"]}")
      expect_equal(resp$terminalGateGid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentId, "591a3b441d725115208a6fba")
    }
  )
})

test_that("it works given a parentPopulation object; `gates` has complex value", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations" = {
          expect_equal(req$method, "POST")
          body = rawToChar(req$options$postfields)
          expect_equal(body, '{"name":"my gate","gates":"{\\\"$and\\\":[{\\\"$or\\\":[\\\"591a3b441d725115208a6fbf\\\"]},\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba"}')
          response = httptest::fakeResponse(
            req$url,
            req$method,
            content = '{"experimentId":"591a3b441d725115208a6fda","name":"my gate","gates":"{\\\"$and\\\":[{\\\"$or\\\":[\\\"591a3b441d725115208a6fbf\\\"]},\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba","id":"592640aa298f1480900e10e4","_id":"592640aa298f1480900e10e4"}',
            status_code = 201,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      pop = list(
        `_id` = "591a3b441d725115208a6fba",
        gates = '{"$or":["591a3b441d725115208a6fbf"]}',
        name = "popname"
      )
      resp = autoCreatePopulation("591a3b441d725115208a6fda", "my gate", "592640a5a6a1d6256ec9b08a", parentPopulation = pop)
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$name, "my gate")
      expect_equal(resp$gates, "{\"$and\":[{\"$or\":[\"591a3b441d725115208a6fbf\"]},\"592640a5a6a1d6256ec9b08a\"]}")
      expect_equal(resp$terminalGateGid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentId, "591a3b441d725115208a6fba")
    }
  )
})

test_that("it works given a parentPopulationId", {
  with_mock(
    `httr::request_perform` = function(req, handle, refresh) {
      switch(req$url,
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations/591a3b441d725115208a6fba" = {
          expect_equal(req$method, "GET")
          response = httptest::fakeResponse(
            req$url,
            req$method,
            content = '{"experimentId":"591a3b441d725115208a6fda","name":"my gate","gates":"{\\\"$and\\\":[\\\"591a3b441d725115208a6fbf\\\"]}","terminalGateGid":"591a3b441d725115208a6fbf","parentId":null,"id":"591a3b441d725115208a6fba","_id":"591a3b441d725115208a6fba"}',
            status_code = 200,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        "https://my.server.com/api/v1/experiments/591a3b441d725115208a6fda/populations" = {
          expect_equal(req$method, "POST")
          body = rawToChar(req$options$postfields)
          expect_equal(body, '{"name":"my gate","gates":"{\\\"$and\\\":[\\\"591a3b441d725115208a6fbf\\\",\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba"}')
          response = httptest::fakeResponse(
            req$url,
            req$method,
            content = '{"experimentId":"591a3b441d725115208a6fda","name":"my gate","gates":"{\\\"$and\\\":[\\\"591a3b441d725115208a6fbf\\\",\\\"592640a5a6a1d6256ec9b08a\\\"]}","terminalGateGid":"592640a5a6a1d6256ec9b08a","parentId":"591a3b441d725115208a6fba","id":"592640aa298f1480900e10e4","_id":"592640aa298f1480900e10e4"}',
            status_code = 201,
            headers = list(`Content-Type` = "application/json")
          )
          return(response)
        },
        {
          stop(sprintf("Unexpected request URL: %s", req$url))
        }
      )
    },
    {
      setServer("https://my.server.com")
      resp = autoCreatePopulation("591a3b441d725115208a6fda", "my gate", "592640a5a6a1d6256ec9b08a", parentPopulationId = "591a3b441d725115208a6fba")
      expect_equal(resp$experimentId, "591a3b441d725115208a6fda")
      expect_equal(resp$`_id`, "592640aa298f1480900e10e4") # assigned server-side
      expect_equal(resp$name, "my gate")
      expect_equal(resp$gates, "{\"$and\":[\"591a3b441d725115208a6fbf\",\"592640a5a6a1d6256ec9b08a\"]}")
      expect_equal(resp$terminalGateGid, "592640a5a6a1d6256ec9b08a")
      expect_equal(resp$parentId, "591a3b441d725115208a6fba")
    }
  )
})
