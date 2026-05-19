# Login flow:
#   1. Resolve the RSA public key (cached or fetched from the live JS bundle).
#   2. Encrypt {account, password, rememberMe} with RSA PKCS#1 v1.5 (the
#      default of openssl::rsa_encrypt and the default of JSEncrypt in the
#      browser bundle).
#   3. Base64-encode the ciphertext and POST as {"key": "..."}.
#   4. Cache the returned JWT on disk in user_dir("cache") keyed by account
#      hash; reuse until exp - 60s.

#' Authenticate with the iEarth DataHub
#'
#' Credentials for the ESD dataset are issued by the author of that dataset;
#' see <https://github.com/shuangchencc/ESD>. Supply them via the
#' `IEARTHDATA_USER` / `IEARTHDATA_PASSWORD` environment variables or as
#' arguments to this function. The returned JWT is cached on disk in the
#' user cache directory so subsequent R sessions reuse it until it expires.
#'
#' @param user Account name or email. Defaults to `IEARTHDATA_USER`.
#' @param password Plaintext password. Defaults to `IEARTHDATA_PASSWORD`.
#' @param remember Sent as `rememberMe` in the login payload.
#' @param refresh If `TRUE`, ignore the on-disk cache and force a fresh login.
#'
#' @return Invisibly, a list with the cached auth state.
#' @export
ie_login <- function(user = Sys.getenv("IEARTHDATA_USER"),
                     password = Sys.getenv("IEARTHDATA_PASSWORD"),
                     remember = TRUE,
                     refresh = FALSE) {
  if (!nzchar(user)) {
    rlang::abort(c(
      "iEarth DataHub credentials missing.",
      i = "Set `IEARTHDATA_USER` and `IEARTHDATA_PASSWORD` or pass them as arguments.",
      i = "ESD credentials are documented at https://github.com/shuangchencc/ESD."
    ))
  }
  if (!nzchar(password)) {
    rlang::abort("Password is required (set `IEARTHDATA_PASSWORD` or pass `password = `).")
  }

  if (!refresh) {
    cached <- read_cached_token(user)
    if (!is.null(cached)) {
      .iearthdata$auth <- cached
      return(invisible(cached))
    }
  }

  pubkey <- iearthdata_public_key()
  payload <- write_json(list(
    account = user,
    password = password,
    rememberMe = isTRUE(remember)
  ))
  ciphertext <- openssl::rsa_encrypt(charToRaw(payload), pubkey)
  encoded <- openssl::base64_encode(ciphertext)

  resp <- req_auth() |>
    httr2::req_url_path_append("api/user/authenticate") |>
    req_body_json_yy(list(key = encoded)) |>
    perform_json()

  if (!isTRUE(resp$success)) {
    rlang::abort("Authentication failed (server returned success = false).")
  }

  d <- resp$data
  auth <- list(
    token = d$token,
    user_name = d$userName,
    user_id = d$userId,
    email = d$email,
    expires_at = jwt_exp(d$token),
    account = user
  )
  .iearthdata$auth <- auth
  write_cached_token(user, auth)
  invisible(auth)
}

#' Clear cached authentication state
#'
#' Removes the in-memory token and the on-disk cache file for the currently
#' authenticated account.
#'
#' @return Invisible `NULL`.
#' @export
ie_logout <- function() {
  acc <- .iearthdata$auth$account
  if (!is.null(acc)) unlink(token_cache_file(acc), force = TRUE)
  if (exists("auth", envir = .iearthdata, inherits = FALSE)) {
    rm("auth", envir = .iearthdata)
  }
  invisible(NULL)
}

ie_require_auth <- function() {
  if (is.null(.iearthdata$auth)) {
    rlang::abort("Not authenticated. Call `ie_login()` first.")
  }
  exp <- .iearthdata$auth$expires_at
  if (!is.null(exp) && !is.na(exp) && Sys.time() >= exp - 60) {
    rlang::abort("Authentication token has expired. Call `ie_login(refresh = TRUE)`.")
  }
  .iearthdata$auth
}

# JWT --------------------------------------------------------------------

jwt_exp <- function(token) {
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3) return(NA)
  payload <- tryCatch(
    parse_json(rawToChar(base64url_decode(parts[2]))),
    error = function(e) NULL
  )
  if (is.null(payload) || is.null(payload$exp)) return(NA)
  .POSIXct(as.numeric(payload$exp), tz = "UTC")
}

base64url_decode <- function(x) {
  x <- chartr("-_", "+/", x)
  pad <- nchar(x) %% 4L
  if (pad > 0L) x <- paste0(x, strrep("=", 4L - pad))
  openssl::base64_decode(x)
}

# Token cache ------------------------------------------------------------

token_cache_dir <- function() {
  d <- tools::R_user_dir("iearthdata", which = "cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

token_cache_file <- function(user) {
  file.path(token_cache_dir(), paste0("token-", account_hash(user), ".rds"))
}

account_hash <- function(user) {
  substring(as.character(openssl::sha256(user)), 1L, 16L)
}

read_cached_token <- function(user) {
  f <- token_cache_file(user)
  if (!file.exists(f)) return(NULL)
  auth <- tryCatch(readRDS(f), error = function(e) NULL)
  if (is.null(auth)) return(NULL)
  exp <- auth$expires_at
  if (is.null(exp) || is.na(exp) || Sys.time() >= exp - 60) {
    unlink(f, force = TRUE)
    return(NULL)
  }
  auth
}

write_cached_token <- function(user, auth) {
  saveRDS(auth, token_cache_file(user))
}
