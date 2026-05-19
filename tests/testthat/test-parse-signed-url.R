# Build httr2 response objects in-memory so we can drive parse_signed_url
# without touching the network.
fake_response <- function(body, status = 200L,
                          url = "https://example.test/x",
                          content_type = "application/json") {
  resp <- structure(
    list(
      method = "POST",
      url = url,
      status_code = status,
      headers = structure(
        list(`Content-Type` = content_type),
        class = c("httr2_headers", "list")
      ),
      body = charToRaw(body),
      request = NULL,
      cache = new.env(parent = emptyenv())
    ),
    class = "httr2_response"
  )
  resp
}

test_that("parse_signed_url extracts the canonical `signedUrl` field", {
  resp <- fake_response('{"signedUrl":"https://x/y.tif?sig=1","fileName":"y.tif"}')
  expect_equal(parse_signed_url(resp, "key"), "https://x/y.tif?sig=1")
})

test_that("parse_signed_url falls back through legacy field shapes", {
  expect_equal(parse_signed_url(fake_response('{"data":{"signedUrl":"u1"}}'), "k"), "u1")
  expect_equal(parse_signed_url(fake_response('{"data":{"url":"u2"}}'), "k"), "u2")
  expect_equal(parse_signed_url(fake_response('{"data":{"downloadUrl":"u3"}}'), "k"), "u3")
  expect_equal(parse_signed_url(fake_response('{"url":"u4"}'), "k"), "u4")
})

test_that("parse_signed_url returns NA and warns on an unknown shape", {
  expect_warning(
    out <- parse_signed_url(fake_response('{"weirdField":"foo"}'), "k"),
    "Unexpected"
  )
  expect_true(is.na(out))
})

test_that("parse_signed_url returns NA and warns on httr2 error conditions", {
  # Synthesise the kind of condition req_perform_parallel returns on 4xx
  err <- structure(
    list(
      message = "HTTP 400 Bad Request.",
      resp = fake_response("", status = 400L)
    ),
    class = c("httr2_http_400", "httr2_http", "httr2_error", "error", "condition")
  )
  expect_warning(
    out <- parse_signed_url(err, "key1"),
    "downloadResource failed for"
  )
  expect_true(is.na(out))
})
