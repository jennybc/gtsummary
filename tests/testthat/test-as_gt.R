context("test-as_gt")

test_that("tbl_summary", {
  expect_error(tbl_summary(trial) %>% as_gt(), NA)
  expect_warning(tbl_summary(trial) %>% as_gt(), NA)
})

test_that("tbl_regression", {
  expect_error(lm(marker ~ age, trial) %>% tbl_regression() %>% as_gt(), NA)
  expect_warning(lm(marker ~ age, trial) %>% tbl_regression() %>% as_gt(), NA)
})

test_that("tbl_uvregression", {
  expect_error(trial %>% tbl_uvregression(method = lm, y = age) %>% as_gt(), NA)
  expect_warning(trial %>% tbl_uvregression(method = lm, y = age) %>% as_gt(), NA)
})
