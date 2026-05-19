#' Query files in a dataset intersecting a bounding box or polygon
#'
#' Resolves the dataset's catalog tree, expands it to leaf-level paths
#' (optionally filtered by `time`), and issues a `polygonQuery` against
#' each leaf path. Results are combined into a single tibble with a
#' `wk_wkt` geometry column carrying each tile's bounding box.
#'
#' @param id Dataset id from [list_datasets()].
#' @param geometry A numeric bbox `c(xmin, ymin, xmax, ymax)` in WGS84,
#'   an `sf`/`sfc`/`bbox` object, a length-1 WKT POLYGON string, or any
#'   `wk`-handleable geometry. Finer geometries are coerced to their
#'   bounding box server-side; do local filtering on the returned tiles
#'   if a stricter intersection is needed.
#' @param time Optional vector of path-segment values to filter the
#'   catalog (typically years, e.g. `2018:2020`). When `NULL`, all
#'   leaf paths are queried.
#' @return A tibble with columns `gid`, `file`, `size`, `path`,
#'   `geometry` (wk_wkt), `dataset_id`, `r_id`, `suffix`, `table`.
#' @export
query_files <- function(id, geometry, time = NULL) {
  id <- check_scalar_int(id, "id")
  cat <- get_catalog(id)
  paths <- catalog_paths(cat$catalog, time = time)
  if (length(paths) == 0L) {
    rlang::warn("No catalog paths match the supplied `time` filter; returning 0 rows.")
    return(empty_query_result(id, cat))
  }
  wkt <- to_wkt_polygon(geometry)

  per_path <- lapply(paths, function(p) polygon_query_one(cat$table, p, wkt))
  combined <- vctrs::vec_rbind(!!!per_path)
  if (nrow(combined) == 0L) {
    return(empty_query_result(id, cat))
  }

  ds <- list_datasets()
  r_id <- ds$r_id[match(id, ds$id)]

  combined$dataset_id <- id
  combined$r_id       <- r_id
  combined$type       <- cat$type
  combined$suffix     <- cat$suffix
  combined$table      <- cat$table
  combined
}

polygon_query_one <- function(table, path, wkt) {
  resp <- req_data() |>
    httr2::req_url_path_append("data/polygonQuery") |>
    req_body_json_yy(list(params = list(
      polygon = wkt,
      table   = table,
      path    = path
    ))) |>
    perform_json()

  # The endpoint returns either a top-level JSON array (success) or a
  # single object with `code`/`error` (failure). yyjsonr parses arrays
  # of objects to a list of lists when `arr_of_objs_to_df = FALSE`.
  if (is.list(resp) && !is.null(resp$success) && isFALSE(resp$success)) {
    rlang::abort(c(
      sprintf("polygonQuery failed for path '%s'.", path),
      x = resp$error %||% "unknown error"
    ))
  }
  if (length(resp) == 0L) return(files_tibble())

  tibble::tibble(
    gid      = vapply(resp, \(x) as.integer(x$gid), integer(1)),
    file     = vapply(resp, \(x) as.character(x$file), character(1)),
    size     = vapply(resp, \(x) as.numeric(x$size), numeric(1)),
    path     = path,
    geometry = wk::wkt(
      vapply(resp, \(x) coords_to_wkt(x$coords), character(1)),
      crs = "OGC:CRS84"
    )
  )
}

files_tibble <- function() {
  tibble::tibble(
    gid      = integer(),
    file     = character(),
    size     = numeric(),
    path     = character(),
    geometry = wk::wkt(crs = "OGC:CRS84")
  )
}

empty_query_result <- function(id, cat) {
  out <- files_tibble()
  out$dataset_id <- integer()
  out$r_id       <- integer()
  out$type       <- character()
  out$suffix     <- character()
  out$table      <- character()
  out
}
