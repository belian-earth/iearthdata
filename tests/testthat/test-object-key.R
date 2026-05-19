test_that("make_object_key follows shared-dataset/<type>/<path>/<file>", {
  expect_equal(
    make_object_key("SDC30_EBD", "SDC30_EBD_V001/2020", "tile.tif"),
    "shared-dataset/SDC30_EBD/SDC30_EBD_V001/2020/tile.tif"
  )
})

test_that("make_object_key underscores spaces in the dataset type", {
  expect_equal(
    make_object_key("Land cover", "GLC10/2017", "tile.tif"),
    "shared-dataset/Land_cover/GLC10/2017/tile.tif"
  )
})

test_that("build_sign_jobs rejects a tibble missing required columns", {
  expect_error(
    build_sign_jobs(tibble::tibble(file = "x", path = "y"), dataset_id = NULL),
    "missing required columns"
  )
})
