#' Get pre-signed download URLs for files
#'
#' Issues `POST /starcloud/api/file/downloadResource` for each file and
#' returns the pre-signed URLs. Requests are fanned out in parallel via
#' [httr2::req_perform_parallel()].
#'
#' The `objectKey` sent for each file follows the format the iEarth UI
#' constructs in chunk 813 of the SPA:
#' `shared-dataset/<dataset_type>/<catalog_path>/<filename>`, with
#' spaces in the dataset type replaced with underscores. For example,
#' an ESD tile is keyed as
#' `shared-dataset/SDC30_EBD/SDC30_EBD_V001/2020/<file>`. The literal
#' `"shared-dataset"` prefix is hardcoded in the SPA call site
#' (`rootPath: "shared-dataset"`); the dataset's `resourceId` is sent
#' separately in the body.
#'
#' @param files A tibble from [query_files()] / [esd_query()], or a
#'   character vector of fully-formed object keys (in which case
#'   `dataset_id` must be supplied so the resourceId can be looked up).
#' @param dataset_id Required when `files` is a character vector.
#' @param max_active Maximum concurrent requests.
#' @return A character vector of pre-signed URLs aligned with `files`.
#'   Failed requests yield `NA` with a `warning` carrying the HTTP
#'   status and (where available) the response body.
#' @export
get_signed_url <- function(files, dataset_id = NULL, max_active = 6L) {
  auth <- ie_require_auth()
  jobs <- build_sign_jobs(files, dataset_id)
  if (nrow(jobs) == 0L) return(character())

  reqs <- lapply(seq_len(nrow(jobs)), function(i) {
    build_sign_request(jobs$object_key[i], jobs$r_id[i], auth)
  })

  resps <- httr2::req_perform_parallel(
    reqs,
    max_active = max_active,
    on_error = "continue"
  )

  vapply(seq_along(resps), function(i) {
    parse_signed_url(resps[[i]], jobs$object_key[i])
  }, character(1))
}

build_sign_jobs <- function(files, dataset_id) {
  if (is.data.frame(files)) {
    needed <- c("file", "path", "r_id", "type")
    missing <- setdiff(needed, names(files))
    if (length(missing) > 0L) {
      rlang::abort(c(
        "`files` data frame is missing required columns.",
        x = paste("Missing:", paste(missing, collapse = ", ")),
        i = "Pass the tibble returned by `query_files()` or `esd_query()`."
      ))
    }
    tibble::tibble(
      object_key = make_object_key(files$type, files$path, files$file),
      r_id       = as.integer(files$r_id)
    )
  } else {
    if (is.null(dataset_id)) {
      rlang::abort("`dataset_id` must be supplied when `files` is a character vector.")
    }
    ds <- list_datasets()
    r_id <- ds$r_id[match(check_scalar_int(dataset_id, "dataset_id"), ds$id)]
    if (is.na(r_id)) rlang::abort("`dataset_id` not found in dataset catalogue.")
    tibble::tibble(
      object_key = as.character(files),
      r_id       = rep(r_id, length(files))
    )
  }
}

# Hardcoded in the SPA as `rootPath: "shared-dataset"`. Not derived from
# any dataset field.
ie_root_path <- "shared-dataset"

make_object_key <- function(type, path, file) {
  type <- gsub(" ", "_", type, fixed = TRUE)
  paste(ie_root_path, type, path, file, sep = "/")
}

build_sign_request <- function(object_key, r_id, auth) {
  body <- list(
    objectKey    = object_key,
    resourceId   = as.character(r_id),
    userAccount  = auth$user_name,
    userId       = auth$user_id,
    country      = Sys.getenv("IEARTHDATA_COUNTRY", ""),
    resourceType = "REMOTE_SENSING"
  )
  req_auth() |>
    httr2::req_url_path_append("api/file/downloadResource") |>
    httr2::req_auth_bearer_token(auth$token) |>
    req_body_json_yy(body)
}

# Response shape (per chunk 813 of the SPA, where the call is
# destructured as `{signedUrl, fileName}`):
#   { "signedUrl": "...", "fileName": "..." }
# Earlier shape guesses (`data.url`, `data.downloadUrl`, `url`) are kept
# as defensive fallbacks but should not be hit in normal operation.
parse_signed_url <- function(resp, object_key = NA_character_) {
  if (inherits(resp, "condition")) {
    detail <- extract_error_detail(resp)
    rlang::warn(c(
      sprintf("downloadResource failed for `%s`.", object_key),
      x = detail$message,
      i = if (!is.null(detail$body)) paste("Body:", detail$body)
    ))
    return(NA_character_)
  }

  body <- tryCatch(
    parse_json(httr2::resp_body_string(resp)),
    error = function(e) NULL
  )
  if (is.null(body)) {
    rlang::warn(sprintf("downloadResource returned an unparseable body for `%s`.", object_key))
    return(NA_character_)
  }

  url <- body$signedUrl %||%
         body$data$signedUrl %||%
         body$data$url %||%
         body$data$downloadUrl %||%
         body$url

  if (is.null(url) || !is.character(url) || length(url) != 1L) {
    rlang::warn(c(
      sprintf("Unexpected downloadResource response shape for `%s`.", object_key),
      i = paste("Body:", substr(write_json(body), 1, 400))
    ))
    return(NA_character_)
  }
  url
}

# Extract status + body from an httr2 error condition surfaced by
# `req_perform_parallel(on_error = "continue")`.
extract_error_detail <- function(cnd) {
  msg <- conditionMessage(cnd) %||% "request failed"
  body <- NULL
  resp <- cnd$resp
  if (!is.null(resp)) {
    status <- tryCatch(httr2::resp_status(resp), error = function(e) NA_integer_)
    body_str <- tryCatch(httr2::resp_body_string(resp), error = function(e) NULL)
    if (!is.null(body_str) && nzchar(body_str)) body <- substr(body_str, 1, 400)
    msg <- sprintf("HTTP %s: %s", status, msg)
  }
  list(message = msg, body = body)
}
