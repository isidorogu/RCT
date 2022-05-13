context("Pruebas impact_eval")

#' @importFrom magrittr %>%

# Base completa para TODOS los tests 
data_original<-data.frame(key = c(1:1000), 
                          inc_quartile = rep(c("Q2", "Q1", "Q4", "Q3"), each = 250), 
                          age_quartile = rep(c("Q2", "Q1", "Q4", "Q3"), times = 250), 
                          inc = rnorm(n = 1000, mean = 8000, sd = 4000), 
                          age = rpois(n = 1000, lambda = 34), 
                          log_outcome = rnorm(n = 1000, mean = 8, sd = 1.4), 
                          treat = rep(c(rep(c(0,1,2,3), times = 10), rep(c(0,1,2,3), each = 10)), each = 25)) 


# 1. Dimensiones 
imp_eval<-impact_eval(data = data_original, 
                      endogenous_vars = "log_outcome", treatment = "treat")

test_that("impact_eval correct dimensions tests", {
  expect_is(imp_eval, "list")
  expect_equal(length(imp_eval), 1)
  expect_equal(ncol(imp_eval$log_outcome), 5)
  expect_equal(nrow(imp_eval$log_outcome), 4)

  
})


# 2. Nombres
test_that("Correct names impact_eval" , {
  expect_equal(names(imp_eval$log_outcome), c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(sum(stringr::str_detect(unique(imp_eval$log_outcome$term), pattern = "Intercept")), 1)
})



####
# Dos variables

imp_eval2<-impact_eval(data = data_original, 
                      endogenous_vars = c("log_outcome", "inc"), treatment = "treat")

test_that("impact_eval2 correct dimensions tests", {
  expect_is(imp_eval2, "list")
  expect_equal(length(imp_eval2), 2)
  expect_equal(ncol(imp_eval2$log_outcome), 5)
  expect_equal(ncol(imp_eval2$inc), 5)
  expect_equal(nrow(imp_eval2$log_outcome), 4)
  expect_equal(nrow(imp_eval2$inc), 4)

})

# 2. Nombres
test_that("Correct names impact_eval2" , {
  expect_equal(names(imp_eval2$log_outcome), c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(sum(stringr::str_detect(unique(imp_eval2$log_outcome$term), pattern = "Intercept")), 1)
  expect_equal(sum(stringr::str_detect(unique(imp_eval2$inc$term), pattern = "Intercept")), 1)
})




# Con heterogeneidades, fixed effects y controles 
imp_eval3<-impact_eval(data = data_original, 
                      endogenous_vars = "log_outcome", 
                      treatment = "treat", 
                      heterogenous_vars = "inc_quartile", 
                      fixed_effect_vars = "age_quartile", control_vars = "inc")

test_that("impact_eval correct dimensions tests", {
  expect_is(imp_eval3, "list")
  expect_equal(length(imp_eval3), 2)
  expect_equal(ncol(imp_eval3$log_outcome), 5)
  expect_equal(nrow(imp_eval3$log_outcome), 4)
  expect_equal(ncol(imp_eval3$log_outcome_inc_quartile), 6)
  expect_equal(nrow(imp_eval3$log_outcome_inc_quartile), 16)
  
  
})


# 2. Nombres
test_that("Correct names impact_eval" , {
  expect_equal(names(imp_eval3$log_outcome), c("term", "estimate", "std.error", "statistic", "p.value"))
  expect_equal(sum(stringr::str_detect(unique(imp_eval3$log_outcome$term), pattern = "Intercept")), 0)
  expect_equal(sum(stringr::str_detect(unique(imp_eval3$log_outcome_inc_quartile$term), pattern = "Intercept")), 0)
  
})


# Heterogeneidades: dos variables het 
imp_eval4<-impact_eval(data = data_original, 
                       endogenous_vars = "log_outcome", 
                       treatment = "treat", 
                       heterogenous_vars = c("inc_quartile", "age_quartile"))

imp_eval5<-impact_eval(data = data_original %>% dplyr::filter(inc_quartile == 'Q1'),
                       endogenous_vars = 'log_outcome', 
                       treatment =  'treat')


test_that("Correct aligment het_var values with estimates" , {
  expect_equal(unique(imp_eval4$log_outcome_inc_quartile$inc_quartile), c("Q1", "Q2", "Q3", "Q4"))
  expect_equal(imp_eval4$log_outcome_inc_quartile$estimate[1:4], imp_eval5$log_outcome$estimate[1:4])

})


