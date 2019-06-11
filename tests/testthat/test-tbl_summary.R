context("test-tbl_summary")

test_that("tbl_summary creates output without error/warning (no by var)", {
  expect_error(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x, sort = list(..categorical.. = "frequency"))),
    NA
  )
  expect_warning(
    purrr::map(list(mtcars, iris), ~ tbl_summary(.x)),
    NA
  )
})


test_that("tbl_summary creates output without error/warning (with by var)", {
  expect_error(
    tbl_summary(mtcars, by = "am"),
    NA
  )
  expect_warning(
    tbl_summary(mtcars, by = "am"),
    NA
  )
})


test_that("tbl_summary throws errors/messages with bad 'sort = ' specifications", {
  expect_message(
    tbl_summary(mtcars, sort = list(not_a_var = "frequency")),
    "*"
  )
  expect_error(
    tbl_summary(mtcars, sort = list(..categorical.. = c("frequency", "two"))),
    "*"
  )
  expect_error(
    tbl_summary(mtcars, sort = list(..categorical.. = "freq5555uency")),
    "*"
  )
})

test_that("tbl_summary omits date var", {
  expect_message(
    tbl_summary(sp500),
    "Column(s)*"
  )
})

test_that("tbl_summary returns errors with bad inputs", {
  expect_error(
    tbl_summary(tibble::tibble()),
    "*"
  )
  expect_error(
    tbl_summary(tibble::tibble(t = integer())),
    "*"
  )
  expect_error(
    tbl_summary(list(test = 5)),
    "*"
  )
  expect_error(
    tbl_summary(gastric, by = "THIS_IS_NOT_A_VARIABLE"),
    "*"
  )
  expect_error(
    tbl_summary(gastric, by = "response"),
    "*"
  )
  expect_error(
    tbl_summary(gastric, type = "response"),
    "*"
  )
  expect_error(
    tbl_summary(gastric, value = "0"),
    "*"
  )
  expect_error(
    tbl_summary(gastric, label = "Age"),
    "*"
  )
  expect_error(
    tbl_summary(gastric, statistic = "{mean}"),
    "*"
  )
  expect_error(
    tbl_summary(gastric, digits = 0),
    "*"
  )
  expect_error(
    tbl_summary(gastric, digits = list(age = -5)),
    "*"
  )
  expect_error(
    tbl_summary(gastric, sort = list(grade = "frequ55555ency")),
    "*"
  )
})
