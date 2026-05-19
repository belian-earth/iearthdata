test_that("bbox_to_wkt emits a closed-ring POLYGON in lon/lat order", {
  wkt <- bbox_to_wkt(c(116, 39, 117, 40))
  expect_match(wkt, "^POLYGON\\(\\(")
  # First and last coordinate identical (closed ring)
  coords <- regmatches(wkt, regexec("POLYGON\\(\\((.*)\\)\\)", wkt))[[1]][2]
  pts <- strsplit(coords, ", ", fixed = TRUE)[[1]]
  expect_length(pts, 5L)
  expect_equal(pts[1], pts[5])
  # First point is (xmin, ymin)
  expect_match(pts[1], "^116\\.0+ 39\\.0+$")
})

test_that("to_wkt_polygon accepts numeric, bbox, WKT, and wk-handleable", {
  # numeric c(xmin, ymin, xmax, ymax)
  expect_match(to_wkt_polygon(c(0, 0, 1, 1)), "^POLYGON")
  # length-1 WKT string pass-through
  expect_equal(
    to_wkt_polygon("POLYGON((0 0,1 0,1 1,0 1,0 0))"),
    "POLYGON((0 0,1 0,1 1,0 1,0 0))"
  )
  # wk_xy/wk geometry — takes bbox
  if (requireNamespace("wk", quietly = TRUE)) {
    pts <- wk::xy(c(0, 1), c(0, 1))
    expect_match(to_wkt_polygon(pts), "^POLYGON\\(\\(0\\.")
  }
})

test_that("coords_to_wkt round-trips the API tile ring format", {
  # The API returns [xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin, xmax, ymin]
  ring <- c(117.09, 38.77, 117.09, 39.75, 115.84, 39.75, 115.84, 38.77, 117.09, 38.77)
  wkt <- coords_to_wkt(ring)
  expect_match(wkt, "^POLYGON")
  # Should produce 5 coordinate pairs (closed ring)
  pairs <- strsplit(regmatches(wkt, regexec("POLYGON\\(\\((.*)\\)\\)", wkt))[[1]][2], ", ", fixed = TRUE)[[1]]
  expect_length(pairs, 5L)
  expect_equal(pairs[1], pairs[5])
})

test_that("coords_to_wkt rejects wrong-length input with NA, not error", {
  expect_true(is.na(coords_to_wkt(c(1, 2, 3))))
})
