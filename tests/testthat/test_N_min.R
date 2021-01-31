context("Pruebas N_min")

#' @importFrom magrittr %>%

# Base completa para TODOS los tests 
data_original<-data.frame(key = c(1:1000), 
                          inc_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = 250), 
                          age_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), times = 250), 
                          inc = rnorm(n = 1000, mean = 8000, sd = 4000), 
                          age = rpois(n = 1000, lambda = 34), 
                          log_outcome = rnorm(n = 1000, mean = 8, sd = 1.4),
                          bin_outcome = rbinom(n = 1000, size = 1, prob = 0.3))
                          
# 1. Dimensiones 
a<-N_min(data_original$log_outcome,
           tau_min = 0.01, share_control = seq(0.05, 0.95, 0.05))

b<-N_min(outcome_var = data_original %>% dplyr::select(log_outcome),
           tau_min =  0.01, share_control = seq(0.05, 0.95, 0.05))


test_that("dimensions N_min", {
  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_equal(ncol(a), 7)
  expect_equal(ncol(b), 7)
  expect_equal(nrow(a), 19)
  expect_equal(nrow(b), 19)
  expect_equal(a,b)
  
})




# Errores
test_that("Errors in tau_min or outcome", {
expect_error(N_min("c", 0.01, share_control = seq(0.05,0.95, 0.05)))
expect_error(N_min(diamonds$depth, tau_min = -1, share_control = seq(0.05, 0.95, 0.05)))
})


