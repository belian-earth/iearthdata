# Base URLs and httr2 request builders for the two iEarth DataHub services.

ie_base_data <- "https://data-starcloud.pcl.ac.cn/aiforearth/api"
ie_base_auth <- "https://data-starcloud.pcl.ac.cn/starcloud"
ie_site_root <- "https://data-starcloud.pcl.ac.cn"

ie_user_agent <- function() {
  paste0("iearthdata-r/", utils::packageVersion("iearthdata"))
}

req_data <- function() {
  httr2::request(ie_base_data) |>
    httr2::req_user_agent(ie_user_agent()) |>
    httr2::req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    httr2::req_error(body = ie_resp_error_body)
}

req_auth <- function() {
  httr2::request(ie_base_auth) |>
    httr2::req_user_agent(ie_user_agent()) |>
    httr2::req_retry(max_tries = 3, backoff = \(i) 2^i) |>
    httr2::req_error(body = ie_resp_error_body)
}

ie_resp_error_body <- function(resp) {
  body <- tryCatch(parse_json(httr2::resp_body_string(resp)), error = function(e) NULL)
  if (is.list(body) && !is.null(body$error)) body$error else NULL
}

# yyjsonr-backed JSON helpers ------------------------------------------------

write_json <- function(x) {
  yyjsonr::write_json_str(x, opts = list(auto_unbox = TRUE))
}

parse_json <- function(s) {
  yyjsonr::read_json_str(s, opts = list(arr_of_objs_to_df = FALSE))
}

req_body_json_yy <- function(req, data) {
  httr2::req_body_raw(req, write_json(data), type = "application/json")
}

perform_json <- function(req) {
  resp <- httr2::req_perform(req)
  parse_json(httr2::resp_body_string(resp))
}
