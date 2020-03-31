context("Pruebas tau_min/tau_min_probability")

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
a<-tau_min(data_original$log_outcome,
           N = nrow(data_original), share_control = seq(0.05, 0.95, 0.05))

b<-tau_min(data_original %>% dplyr::select(log_outcome),
           N = nrow(data_original), share_control = seq(0.05, 0.95, 0.05))

e<-tau_min_probability(prior = 0.5, N = nrow(data_original), share_control =  seq(0.05, 0.95, 0.05))


test_that("dimensions tau_min", {
  expect_is(a, "data.frame")
  expect_is(b, "data.frame")
  expect_equal(ncol(a), 9)
  expect_equal(ncol(b), 9)
  expect_equal(ncol(e), 9)
  expect_equal(nrow(a), 19)
  expect_equal(nrow(b), 19)
  expect_equal(nrow(e), 19)
  expect_equal(a,b)
  
})




# Errores
test_that("Errors in N or outcome", {
expect_error(tau_min_probability("c", nrow(diamonds), share_control = seq(0.05,0.95, 0.05)))
expect_error(tau_min(diamonds$depth, N = 1.5, share_control = seq(0.05, 0.95, 0.05)))
})


