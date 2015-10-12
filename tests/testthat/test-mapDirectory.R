require(reltest)

# mapDirectory_1 ----------------------------------------------------------

context("mapDirectory_1")

test_that("mapDirectory::default", {
  expect_is(
    res <- withCorrectWorkingDir(
      mapDirectory(dir = "data/mapDirectory_1")
    ),
    "list"
  )
  expect_true(length(res) > 0)
  expect_true(names(res) == "data")
})

test_that2("mapDirectory::include_root", {
  expect_is(res <- mapDirectory(dir = "data/mapDirectory_1",
    include_root = FALSE), "list")
  expect_true(length(res) > 0)
  expect_true(names(res) != "data")
})


# mapDirectory_2 ----------------------------------------------------------

context("mapDirectory_2")

test_that("mapDirectory_2::include_files", {
  expect_identical(
    withCorrectWorkingDir(
      res <- mapDirectory(dir = "data/mapDirectory_2",
        include_files = FALSE)
    ),
    list(data = list(mapDirectory_2 = list(R = list(scripts = list()))))
  )
})

# Relative ----------------------------------------------------------------

context("mapDirectory_2::relative")

test_that("mapDirectory_2::relative", {
  expect_identical(
    withCorrectWorkingDir(
      res <- mapDirectory(dir = "data/mapDirectory_2",
        include_files = FALSE, relative = TRUE)
    ),
    list(. = list(R = list(scripts = list())))
  )
})

