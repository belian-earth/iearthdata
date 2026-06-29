# httptest2 records HTTP responses on first run (when network is
# available) and replays them on subsequent runs. The recordings live in
# tests/testthat/iearth/. Re-record by deleting that directory and
# running the suite with network access.

skip_if_no_httptest2()

httptest2::with_mock_dir("iearth", {

  test_that("list_datasets returns a tibble keyed by id", {
    ds <- list_datasets()
    expect_s3_class(ds, "tbl_df")
    expect_named(ds, c("id", "r_id", "name", "type"), ignore.order = TRUE)
    expect_gt(nrow(ds), 30L)
    # ESD must be present
    expect_true(64L %in% ds$id)
    esd <- ds[ds$id == 64L, ]
    expect_equal(esd$r_id, 64L)
    expect_equal(esd$type, "SDC30_EBD")
  })

  test_that("get_catalog(64) returns the ESD table and year leaves", {
    cat <- get_catalog(64L)
    expect_equal(cat$id, 64L)
    expect_equal(cat$table, "rs_sdc30_ebd2")
    expect_equal(cat$suffix, ".tif")
    expect_equal(cat$type, "SDC30_EBD")
    paths <- catalog_paths(cat$catalog)
    expect_true(all(grepl("^SDC30_EBD_V001/[0-9]{4}$", paths)))
    expect_true("SDC30_EBD_V001/2020" %in% paths)
  })

  test_that("query_files returns tiles intersecting a Beijing bbox for 2020", {
    res <- query_files(64L, geometry = c(116, 39, 117, 40), time = 2020L)
    expect_s3_class(res, "tbl_df")
    expect_named(
      res,
      c("gid", "file", "size", "path", "geometry",
        "dataset_id", "r_id", "type", "suffix", "table"),
      ignore.order = TRUE
    )
    expect_gt(nrow(res), 0L)
    expect_true(all(grepl("\\.tif$", res$file)))
    expect_true(all(res$path == "SDC30_EBD_V001/2020"))
    expect_equal(unique(res$type), "SDC30_EBD")
    expect_s3_class(res$geometry, "wk_wkt")
  })

  test_that("esd_query adds a year column parsed from the path", {
    a <- esd_query(c(116, 39, 117, 40), years = 2020L)
    b <- query_files(64L, geometry = c(116, 39, 117, 40), time = 2020L)
    # esd_query extends query_files with a `year` column.
    expect_equal(a[setdiff(names(a), "year")], b)
    expect_type(a$year, "integer")
    expect_equal(unique(a$year), 2020L)
  })

})
