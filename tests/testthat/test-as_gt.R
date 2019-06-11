context("test-as_gt")

test_that("tbl_summary", {
  expect_error(tbl_summary(gastric) %>% as_gt(), NA)
  expect_warning(tbl_summary(gastric) %>% as_gt(), NA)
})

test_that("tbl_regression", {
  expect_error(lm(albumin ~ age, gastric) %>% tbl_regression() %>% as_gt(), NA)
  expect_warning(lm(albumin ~ age, gastric) %>% tbl_regression() %>% as_gt(), NA)
})

test_that("tbl_uvregression", {
  expect_error(gastric %>% tbl_uvregression(method = lm, y = age) %>% as_gt(), NA)
  expect_warning(gastric %>% tbl_uvregression(method = lm, y = age) %>% as_gt(), NA)
})
