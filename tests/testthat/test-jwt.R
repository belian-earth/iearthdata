test_that("base64url_decode handles padding-stripped inputs", {
  raw <- charToRaw("hello world!")
  std <- openssl::base64_encode(raw)
  url <- gsub("=", "", chartr("+/", "-_", std), fixed = TRUE)
  expect_equal(rawToChar(base64url_decode(url)), "hello world!")
})

test_that("jwt_exp extracts the exp claim as a POSIXct", {
  payload_json <- '{"sub":"x","exp":1779000000}'
  enc <- function(s) {
    gsub("=", "", chartr("+/", "-_", openssl::base64_encode(charToRaw(s))), fixed = TRUE)
  }
  token <- paste(enc('{"alg":"HS256"}'), enc(payload_json), enc("sig"), sep = ".")
  out <- jwt_exp(token)
  expect_s3_class(out, "POSIXct")
  expect_equal(as.numeric(out), 1779000000)
})

test_that("jwt_exp returns NA on a non-JWT token", {
  expect_true(is.na(jwt_exp("not-a-jwt")))
  expect_true(is.na(jwt_exp("one.two")))
})
