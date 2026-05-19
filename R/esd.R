#' Query ESD (SDC30 Embedded Seamless Data) tiles by bounding box and year
#'
#' Convenience wrapper around [query_files()] pinned to the ESD dataset
#' (id `64`, table `rs_sdc30_ebd2`). ESD's catalog is a flat
#' `SDC30_EBD_V001/<year>` structure spanning 2000-2024.
#'
#' @param bbox A bounding box (numeric `c(xmin, ymin, xmax, ymax)` in
#'   WGS84) or any geometry accepted by [query_files()].
#' @param years Integer vector of years to include. `NULL` returns all
#'   available years.
#' @param signed If `TRUE`, attaches pre-signed download URLs as a `url`
#'   column. Requires a prior [ie_login()].
#' @return A tibble of intersecting tiles. See [query_files()] for the
#'   column schema; with `signed = TRUE` an additional `url` column is
#'   appended.
#' @export
esd_query <- function(bbox, years = NULL, signed = FALSE) {
  out <- query_files(id = 64L, geometry = bbox, time = years)
  if (isTRUE(signed) && nrow(out) > 0L) {
    out$url <- get_signed_url(out)
  }
  out
}
