context("Pruebas balance_test")

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
b_table<-balance_table(data_original %>% 
                     dplyr::select(inc, age, log_outcome, treat), 
                   treatment = "treat")

test_that("balance_table retruns tibble w/right dimension", {
expect_is(b_table, "data.frame")
expect_equal(ncol(b_table), 8)
expect_equal(nrow(b_table), 3)
})


# 2. Nombres
test_that("Correct number of Means and p_values " , {
expect_equal(sum(stringr::str_detect(string = names(b_table), pattern = "^p_value")), 3)
expect_equal(sum(stringr::str_detect(string = names(b_table), pattern = "^Media")), 4)
})
