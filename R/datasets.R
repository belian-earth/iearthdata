#' List all datasets available on the iEarth DataHub
#'
#' Returns the full catalogue (35 datasets at time of writing). The
#' upstream `type` query parameter is server-side ignored; filter the
#' returned tibble by the `type` column if needed.
#'
#' @return A tibble with columns `id`, `r_id`, `name`, `type`.
#' @export
list_datasets <- function() {
  resp <- req_data() |>
    httr2::req_url_path_append("data/getDataAssets") |>
    httr2::req_url_query(type = "All") |>
    perform_json()

  tibble::tibble(
    id   = vapply(resp, \(x) as.integer(x$id), integer(1)),
    r_id = vapply(resp, \(x) as.integer(x$r_id), integer(1)),
    name = vapply(resp, \(x) as.character(x$name %||% NA_character_), character(1)),
    type = vapply(resp, \(x) as.character(x$type %||% NA_character_), character(1))
  )
}

#' Get metadata for a single dataset
#'
#' @param id Dataset id from [list_datasets()].
#' @return A list with `name`, `description`, `citation`.
#' @export
get_dataset <- function(id) {
  id <- check_scalar_int(id, "id")
  resp <- req_data() |>
    httr2::req_url_path_append("data/getMataData") |>
    httr2::req_url_query(id = id) |>
    perform_json()
  list(
    name        = resp$name,
    description = resp$discription,
    citation    = resp$citations
  )
}

#' Get the catalog tree for a dataset
#'
#' @param id Dataset id from [list_datasets()] (use `id`, not `r_id`).
#' @return A list with `id`, `table`, `type`, `suffix`, `catalog` (the raw
#'   hierarchical tree).
#' @export
get_catalog <- function(id) {
  id <- check_scalar_int(id, "id")
  resp <- req_data() |>
    httr2::req_url_path_append("data/getCatalogTree") |>
    httr2::req_url_query(id = id) |>
    perform_json()
  list(
    id      = id,
    table   = resp$table,
    type    = resp$type,
    suffix  = resp$suffix,
    catalog = resp$catalog
  )
}

check_scalar_int <- function(x, name) {
  if (length(x) != 1L) {
    rlang::abort(sprintf("`%s` must be a length-1 integer.", name))
  }
  out <- suppressWarnings(as.integer(x))
  if (is.na(out)) rlang::abort(sprintf("`%s` must be coercible to integer.", name))
  out
}
