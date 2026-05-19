test_that("catalog_paths walks 2-level and 3-level trees to leaves", {
  esd <- list(list(
    label = "SDC30_EBD_V001",
    children = list(list(label = "2020"), list(label = "2021"), list(label = "2022"))
  ))
  expect_equal(
    catalog_paths(esd),
    c("SDC30_EBD_V001/2020", "SDC30_EBD_V001/2021", "SDC30_EBD_V001/2022")
  )

  modis <- list(list(
    label = "MODISwater2001-2022",
    children = list(
      list(label = "2001", children = list(list(label = "h00v08"), list(label = "h01v08"))),
      list(label = "2002", children = list(list(label = "h00v08")))
    )
  ))
  expect_equal(
    catalog_paths(modis),
    c(
      "MODISwater2001-2022/2001/h00v08",
      "MODISwater2001-2022/2001/h01v08",
      "MODISwater2001-2022/2002/h00v08"
    )
  )
})

test_that("time filter matches year as full path segment, not substring", {
  cat <- list(list(label = "ROOT2020", children = list(
    list(label = "2020"),
    list(label = "2021")
  )))
  # `2020` must not match the root segment `ROOT2020`
  expect_equal(catalog_paths(cat, time = 2020), "ROOT2020/2020")
  expect_equal(catalog_paths(cat, time = c(2020, 2021)),
               c("ROOT2020/2020", "ROOT2020/2021"))
  expect_length(catalog_paths(cat, time = 1999), 0L)
})
