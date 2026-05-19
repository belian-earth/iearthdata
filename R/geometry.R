# Geometry helpers built on `wk`. The iEarth `polygonQuery` endpoint
# accepts WKT POLYGON in WGS84 lon/lat. Most callers will pass a numeric
# bbox `c(xmin, ymin, xmax, ymax)` or an `sf`/`sfc` object; anything
# `wk_handle()`-able is also accepted.

to_wkt_polygon <- function(geom) {
  if (is.character(geom) && length(geom) == 1L && grepl("^POLYGON", geom, ignore.case = TRUE)) {
    return(geom)
  }
  if (is.numeric(geom) && length(geom) == 4L && is.null(dim(geom))) {
    return(bbox_to_wkt(unname(geom)))
  }
  if (inherits(geom, "bbox")) {
    return(bbox_to_wkt(unname(c(geom[["xmin"]], geom[["ymin"]],
                                geom[["xmax"]], geom[["ymax"]]))))
  }
  # Anything else: take the bbox of the geometry via wk and emit a POLYGON.
  # This keeps the spatial query cheap; intersecting tiles for a finer
  # geometry can be filtered locally on the returned tile coords.
  # wk::wk_bbox() returns a `wk_rct` vctrs record; unclass to access
  # the xmin/ymin/xmax/ymax fields.
  bb <- unclass(wk::wk_bbox(geom))
  bbox_to_wkt(c(bb$xmin, bb$ymin, bb$xmax, bb$ymax))
}

bbox_to_wkt <- function(b) {
  sprintf(
    "POLYGON((%.6f %.6f, %.6f %.6f, %.6f %.6f, %.6f %.6f, %.6f %.6f))",
    b[1], b[2],
    b[3], b[2],
    b[3], b[4],
    b[1], b[4],
    b[1], b[2]
  )
}

# polygonQuery returns each tile bbox as a flat 10-element ring in the
# order [xmax,ymin, xmax,ymax, xmin,ymax, xmin,ymin, xmax,ymin].
coords_to_wkt <- function(coords) {
  coords <- as.numeric(coords)
  if (length(coords) != 10L) return(NA_character_)
  sprintf(
    "POLYGON((%.6f %.6f, %.6f %.6f, %.6f %.6f, %.6f %.6f, %.6f %.6f))",
    coords[1], coords[2],
    coords[3], coords[4],
    coords[5], coords[6],
    coords[7], coords[8],
    coords[9], coords[10]
  )
}
