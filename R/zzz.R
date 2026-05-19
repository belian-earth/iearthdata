# Package-private state. Holds:
#   $auth   : list(token, user_name, user_id, email, expires_at, account)
#   $pubkey : openssl pubkey object (cached after first fetch)
.iearthdata <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  user <- Sys.getenv("IEARTHDATA_USER")
  if (nzchar(user)) {
    cached <- tryCatch(read_cached_token(user), error = function(e) NULL)
    if (!is.null(cached)) {
      .iearthdata$auth <- cached
    }
  }
  invisible()
}
