context("Pruebas summary_statistics")

#' @importFrom magrittr %>%

# Base completa para TODOS los tests 
data_original<-data.frame(key = c(1:1000), 
                          inc_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = 250), 
                          age_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), times = 250), 
                          inc = rnorm(n = 1000, mean = 8000, sd = 4000), 
                          age = rpois(n = 1000, lambda = 34), 
                          log_outcome = rnorm(n = 1000, mean = 8, sd = 1.4), 
                          treat = rep(c(rep(c(0,1,2,3), times = 10), rep(c(0,1,2,3), each = 10)), each = 25)) 

# 1. Dimensiones 
sum_table<-summary_statistics(data_original)
a<-ncol(data_original %>% dplyr::select_if(is.numeric))

test_that("summary_statistic returns data.frame w dimension", {
  expect_is(sum_table, "data.frame")
  expect_equal(nrow(sum_table), a)
  expect_equal(ncol(sum_table), 12)

})


