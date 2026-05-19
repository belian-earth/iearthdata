# scratch_probe.R --------------------------------------------------------
# Single-purpose probe to find the correct objectKey shape for OBS.
# Requires an active session: run ie_login() first.
# Not part of the package build.

devtools::load_all()
stopifnot(!is.null(.iearthdata$auth))

auth <- .iearthdata$auth
one  <- res[1, ]                            # uses `res` from scratch.R §3

candidates <- c(
  paste("shared-dataset", one$path, one$file, sep = "/"),    # current
  paste(one$path, one$file, sep = "/"),                       # no prefix
  paste("ESD", one$path, one$file, sep = "/"),
  paste("SDC30_EBD", one$path, one$file, sep = "/"),
  paste("rs_sdc30_ebd2", one$path, one$file, sep = "/"),
  paste("64", one$path, one$file, sep = "/"),
  paste("shared-dataset", "ESD", one$path, one$file, sep = "/")
)

probe_key <- function(key) {
  r <- httr2::request("https://data-starcloud.pcl.ac.cn/starcloud/api/file/downloadResource") |>
    httr2::req_auth_bearer_token(auth$token) |>
    httr2::req_error(is_error = \(.) FALSE) |>
    httr2::req_body_raw(
      write_json(list(
        objectKey    = key,
        resourceId   = "64",
        userAccount  = auth$user_name,
        userId       = auth$user_id,
        country      = "",
        resourceType = "REMOTE_SENSING"
      )),
      type = "application/json"
    ) |>
    httr2::req_perform()
  status <- httr2::resp_status(r)
  body   <- httr2::resp_body_string(r)
  parsed <- tryCatch(parse_json(body), error = function(e) NULL)
  url    <- parsed$signedUrl %||% NA_character_

  # Quick verify: GET the first byte to see if the object exists
  fetch_status <- NA_integer_
  if (!is.na(url)) {
    g <- tryCatch(
      httr2::request(url) |>
        httr2::req_headers(Range = "bytes=0-0") |>
        httr2::req_error(is_error = \(.) FALSE) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (!is.null(g)) fetch_status <- httr2::resp_status(g)
  }
  cat(sprintf("%-65s api=%d  obs=%s\n", key, status, fetch_status))
}

for (k in candidates) probe_key(k)
