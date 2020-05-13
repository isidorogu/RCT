context("Pruebas ntile_label")

#' @importFrom magrittr %>%

# Base completa para TODOS los tests 
data_original<-data.frame(key = c(1:1000), 
                          inc_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = 250), 
                          age_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), times = 250), 
                          inc = rnorm(n = 1000, mean = 8000, sd = 4000), 
                          age = rpois(n = 1000, lambda = 34), 
                          log_outcome = rnorm(n = 1000, mean = 8, sd = 1.4), 
                          treat = rep(c(rep(c(0,1,2,3), times = 10), rep(c(0,1,2,3), each = 10)), each = 25)) 


test_that("ntile returns factor with n-classes", {
  expect_is(ntile_label(data_original$inc, n = 4), "factor")
  expect_equal(length(ntile_label(data_original$inc, n = 4)), nrow(data_original))
  expect_equal(length(levels(ntile_label(data_original$inc, n = 4))), 4)
  })


