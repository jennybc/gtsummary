# Results from a simulated study of chemotherapy vs
# chemotherapy and radiation in gastric cancer

set.seed(8976)
n <- 200
gastric <-
  tibble::tibble(
    trt = sample(c("No Radiation", "Radiation"), n, replace = TRUE),
    age = rnorm(n, mean = 50, sd = 15) %>% as.integer(),
    albumin = rgamma(n, 1, 1) * 1.5 %>% round(digits = 3),
    stage = sample(c("T1", "T2", "T3", "T4"), size = n, replace = TRUE) %>% factor(),
    grade = sample(c("I", "II", "III"), size = n, replace = TRUE) %>% factor(),
    response_prob =
      ((trt == "Radiation") - 0.2 * as.numeric(stage) - 0.1 * as.numeric(grade) - 0.1 * albumin) %>% {
        1 / (1 + exp(-.))
      },
    response = runif(n) < response_prob,
    ttdeath_true =
      exp(1 + 0.2 * response +
        -0.1 * as.numeric(stage) +
        -0.1 * as.numeric(grade) +
        rnorm(n, sd = 0.5)) * 12,
    death = ifelse(ttdeath_true <= 24, 1L, 0L),
    ttdeath = pmin(ttdeath_true, 24) %>% round(digits = 2)
  ) %>%
  dplyr::mutate(
    age = ifelse(runif(n) < 0.95, age, NA_real_),
    albumin = ifelse(runif(n) < 0.95, albumin, NA_real_),
    response = ifelse(runif(n) < 0.95, response, NA_integer_)
  ) %>%
  dplyr::select(-dplyr::one_of("response_prob", "ttdeath_true"))
summary(gastric)

attr(gastric$trt, "label") <- "Treatment"
attr(gastric$age, "label") <- "Age, yrs"
attr(gastric$albumin, "label") <- "Albumin Level"
attr(gastric$stage, "label") <- "T Stage"
attr(gastric$grade, "label") <- "Grade"
attr(gastric$response, "label") <- "Tumor Response"
attr(gastric$death, "label") <- "Patient Died"
attr(gastric$ttdeath, "label") <- "Months to Death/Censor"

usethis::use_data(gastric, overwrite = TRUE)
