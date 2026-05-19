# Test helpers. httptest2's `with_mock_dir()` records requests on first
# run and replays from disk on subsequent runs. The recordings live in
# tests/testthat/<dir>/. Re-record by deleting the directory and running
# the test suite once with network access.

skip_if_no_httptest2 <- function() {
  testthat::skip_if_not_installed("httptest2")
}

# Build a minimal auth list suitable for testing get_signed_url() without
# going through ie_login(). The token is a hand-rolled JWT with a far-
# future exp so ie_require_auth() does not bail.
fake_auth <- function() {
  header  <- openssl::base64_encode(charToRaw('{"alg":"HS256","typ":"JWT"}'))
  payload <- openssl::base64_encode(charToRaw(
    sprintf('{"sub":"test","exp":%d}', as.integer(Sys.time()) + 86400L)
  ))
  sig <- openssl::base64_encode(charToRaw("nope"))
  to_b64url <- function(x) gsub("=", "", chartr("+/", "-_", x), fixed = TRUE)
  list(
    token = paste(to_b64url(header), to_b64url(payload), to_b64url(sig), sep = "."),
    user_name = "tester",
    user_id = "999",
    email = "tester@example.com",
    expires_at = .POSIXct(as.integer(Sys.time()) + 86400L, tz = "UTC"),
    account = "tester"
  )
}
