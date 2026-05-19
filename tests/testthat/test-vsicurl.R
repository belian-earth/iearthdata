test_that("as_vsicurl encodes the URL as a query-string value", {
  raw <- "https://x.obs.cn/foo.tif?AccessKeyId=AK&Expires=1&Signature=abc"
  out <- as_vsicurl(raw)
  expect_match(out, "^/vsicurl\\?use_head=no&url=")
  # The signed URL must survive a single GDAL-side decode round-trip.
  decoded <- curl::curl_unescape(sub("^/vsicurl\\?use_head=no&url=", "", out))
  expect_equal(decoded, raw)
})

test_that("as_vsicurl double-encodes existing %xx so GDAL decodes back to the original", {
  # OBS signatures contain `%2F` literals; curl_escape encodes the `%`
  # to `%25`, and GDAL's one-shot decode recovers the original byte string.
  raw <- "https://x.obs.cn/k?Signature=abc%2Fdef"
  out <- as_vsicurl(raw)
  expect_match(out, "%252F", fixed = TRUE)
  decoded <- curl::curl_unescape(sub("^/vsicurl\\?use_head=no&url=", "", out))
  expect_equal(decoded, raw)
})

test_that("as_vsicurl passes NA and empty strings through unchanged", {
  expect_identical(as_vsicurl(NA_character_), NA_character_)
  expect_identical(as_vsicurl(""), "")
  expect_identical(
    as_vsicurl(c("https://a", NA, "")),
    c("/vsicurl?use_head=no&url=https%3A%2F%2Fa", NA_character_, "")
  )
})
