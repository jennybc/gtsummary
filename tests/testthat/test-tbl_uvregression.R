context("test-tbl_uvregression")
library(survival)
library(lme4)


test_that("lm: no errors/warnings with standard use", {
  expect_error(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = mpg
    ), NA)
  expect_warning(mtcars %>%
    tbl_uvregression(
      method = lm,
      y = mpg
    ), NA)
})

test_that("coxph: no errors/warnings with standard use", {
  expect_error(lung %>%
    tbl_uvregression(
      method = coxph,
      y = Surv(time, status)
    ), NA)
  expect_warning(lung %>%
    tbl_uvregression(
      method = coxph,
      y = Surv(time, status)
    ), NA)
})


test_that("glmer: no errors/warnings with standard use", {
  expect_error(
    mtcars %>%
      dplyr::select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial),
        label = "cyl" ~ "No. Cylinders",
        hide_n = TRUE
      ), NA
  )
  expect_warning(
    mtcars %>%
      dplyr::select("am", "gear", "hp", "cyl") %>%
      tbl_uvregression(
        method = glmer,
        y = am,
        formula = "{y} ~ {x} + (1 | gear)",
        method.args = list(family = binomial)
      ), NA
  )
})

test_that("tbl_regression creates errors with bad inputs", {
  expect_error(
    tbl_uvregression(
      data = mtcars,
      method = coxph,
      y = Surv(time, status),
      pvalue_fun = mtcars
    ),
    "*"
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      estimate_fun = mtcars
    ),
    "*"
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      label = "Labels! YAY"
    ),
    "*"
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      label = list("Age")
    ),
    "*"
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      label = list("age" ~ c("Age", "Two"))
    ),
    "*"
  )
  expect_error(
    tbl_uvregression(
      data = list(lung),
      method = coxph,
      y = Surv(time, status)
    ),
    "*"
  )
  expect_error(
    tbl_uvregression(
      data = lung,
      method = coxph,
      y = Surv(time, status),
      formula = "y ~ x"
    ),
    "*"
  )
})
