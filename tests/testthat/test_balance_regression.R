context("Pruebas balance_regression")


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
b_reg<-balance_regression(data_original %>% 
                         dplyr::select(inc, age, log_outcome, treat), 
                       treatment = "treat")

test_that("balance_regression returns list w/right dimension", {
  expect_is(b_reg, "list")
  expect_equal(length(b_reg), 2)
  expect_equal(ncol(b_reg$regression_tables), 15)
  expect_equal(nrow(b_reg$regression_tables), 4)
  expect_equal(ncol(b_reg$F_test), 4)
  expect_equal(nrow(b_reg$F_test), 6)
  
})


# 2. Nombres
test_that("Correct names balance_regression" , {
  expect_equal(names(b_reg$F_test), c("estadistico", "Msj1", "Msj2", "Msj3"))
  expect_equal(sum(stringr::str_detect(string = names(b_reg$regression_tables), pattern = "^term")), 3)
  expect_equal(sum(stringr::str_detect(string = names(b_reg$regression_tables), pattern = "^estimate")), 3)
  expect_equal(sum(stringr::str_detect(string = names(b_reg$regression_tables), pattern = "^std.error")), 3)
  expect_equal(sum(stringr::str_detect(string = names(b_reg$regression_tables), pattern = "^statistic")), 3)
  expect_equal(sum(stringr::str_detect(string = names(b_reg$regression_tables), pattern = "^p.value")), 3)
})
