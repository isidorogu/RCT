context("Pruebas treatment assign")

#' @importFrom magrittr %>%
#' @importFrom ggplot2 vars

# Base completa para TODOS los tests 
data_original<-data.frame(key = c(1:1000), 
                          inc_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), each = 250), 
                          age_quartile = rep(c("Q1", "Q2", "Q3", "Q4"), times = 250), 
                          inc = rnorm(n = 1000, mean = 8000, sd = 4000), 
                          age = rpois(n = 1000, lambda = 34), 
                          log_outcome = rnorm(n = 1000, mean = 8, sd = 1.4), 
                          treat = rep(c(rep(c(0,1,2,3), times = 10), rep(c(0,1,2,3), each = 10)), each = 25)) 


# 1. Dimensiones 
assign_1_var<-treatment_assign(data = data_original, 
                               share_control = 0.1, 
                               n_t = 3, 
                               strata_varlist = vars(inc_quartile), 
                               missfits = "global", 
                               seed = 1990, key = "key")

test_that("treatment_assign correct dimensions tests", {
  expect_is(assign_1_var, "list")
  expect_equal(length(assign_1_var), 2)
  expect_equal(ncol(assign_1_var$summary_strata), 4)
  expect_equal(nrow(assign_1_var$summary_strata), 4)
  expect_equal(ncol(assign_1_var$data), 4)
  expect_equal(nrow(assign_1_var$data), nrow(data_original))
  expect_equal(length(unique(assign_1_var$data$treat)), 4)
  
  
})


# 2. Nombres
test_that("Correct names treatment_assign" , {
  expect_equal(names(assign_1_var$summary_strata), c("strata", "inc_quartile", "n_strata", "n_missfits"))
  expect_equal(names(assign_1_var$data), c("key", "strata", "treat", "missfit"))
  expect_equal(nrow(assign_1_var$data %>% dplyr::filter(is.na(treat))), 0)
  })



####
# Dos variables

assign_2_var<-treatment_assign(data = data_original, 
                               share_control = 0.1, 
                               n_t = 3, 
                               strata_varlist = vars(inc_quartile, age_quartile), 
                               missfits = "global", 
                               seed = 1990, key = "key")

test_that("treatment_assign2 correct dimensions tests", {
  expect_is(assign_2_var, "list")
  expect_equal(length(assign_2_var), 2)
  expect_equal(ncol(assign_2_var$summary_strata), 5)
  expect_equal(nrow(assign_2_var$summary_strata), 16)
  expect_equal(ncol(assign_2_var$data), 4)
  expect_equal(nrow(assign_2_var$data), nrow(data_original))
  expect_equal(length(unique(assign_2_var$data$treat)), 4)
  
  
})


# 2. Nombres
test_that("Correct names treatment_assign2" , {
  expect_equal(names(assign_2_var$summary_strata), c("strata", "inc_quartile", "age_quartile" , "n_strata", "n_missfits"))
  expect_equal(names(assign_2_var$data), c("key", "strata", "treat", "missfit"))
  expect_equal(nrow(assign_2_var$data %>% dplyr::filter(is.na(treat))), 0)
})



##
# Dos variables , NA
assign_3_var<-treatment_assign(data = data_original, 
                               share_control = 0.1, 
                               n_t = 3, 
                               strata_varlist = vars(inc_quartile, age_quartile), 
                               missfits = "NA", 
                               seed = 1990, key = "key")

test_that("treatment_assign3 correct dimensions tests", {
  expect_is(assign_3_var, "list")
  expect_equal(length(assign_3_var), 2)
  expect_equal(ncol(assign_3_var$summary_strata), 5)
  expect_equal(nrow(assign_3_var$summary_strata), 16)
  expect_equal(ncol(assign_3_var$data), 4)
  expect_equal(nrow(assign_3_var$data), nrow(data_original))
  expect_equal(length(unique(assign_3_var$data$treat)), 5)
  
})


# 2. Nombres
test_that("Correct names treatment_assign3" , {
  expect_equal(names(assign_3_var$summary_strata), c("strata", "inc_quartile", "age_quartile" , "n_strata", "n_missfits"))
  expect_equal(names(assign_3_var$data), c("key", "strata", "treat", "missfit"))
  expect_equal(nrow(assign_3_var$data %>% dplyr::filter(is.na(treat)) ), sum(assign_3_var$data$missfit))
})



