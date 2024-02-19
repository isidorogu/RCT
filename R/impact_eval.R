#' Impact Evaluation of Treatment Effects
#' @param data A data.frame, tibble or data.table
#' @param endogenous_vars Vector of Y's on which treatment effects will be evaluated
#' @param treatment Variable indicating the treatment status
#' @param heterogenous_vars Vector of variables for which you wish to assess treatment distributions/heterogeneities.
#' @param cluster_vars Vector of variables to cluster the standard errors. Default is without clustered std errors
#' @param fixed_effect_vars Vector of variables to add as fixed effects. Default is without fixed effects
#' @param control_vars Vector of variables to control for in the evaluation. Default is without controls
#' @return impact_eval() returns a list of regression tables. The names of the list are the same as the endogenous variables.
#' for heterogeneities the names are endogenous_var_heterogenous_var
#' @examples 
#' data <- data.frame(y_1 = rnorm(n = 100, mean = 100, sd = 15), 
#'                   y_2 = rnorm(n = 100, mean = 8, sd = 2), 
#'                   treat = rep(c(0,1,2,3), each = 25), 
#'                   heterogenous_var1 = rep(c("X_Q1", "X_Q2", "X_Q3", "X_Q4"), times = 25),
#'                   cluster_var1 = rep(c(1:5), times = 20), 
#'                   fixed_effect_var1 = rep(c(1,2), times = 50),
#'                   control_var1 = rnorm(n = 100, mean = 20, sd = 1))
#'
#' evaluation<-impact_eval(data = data, 
#'                        endogenous_vars = c("y_1", "y_2"), 
#'                        treatment = "treat", 
#'                        heterogenous_vars = c("heterogenous_var1"), 
#'                        cluster_vars = "cluster_var1", fixed_effect_vars = c("fixed_effect_var1"), 
#'                        control_vars = c("control_var1"))
#' @details This function carries out the evaluation of treatment effects on endogenous variables. 
#' It automatically runs the regressions of all the endogenous_vars supplied & all the combinations 
#' of endogenous_vars and heterogenous_vars. Additionally, the function has the option of include 
#' fixed_effects, controls and cluster variables for clustered std errors.

#' @export
#' @importFrom magrittr %>%
impact_eval <- function(data, endogenous_vars, treatment, 
                         heterogenous_vars, 
                         fixed_effect_vars = NULL, control_vars, cluster_var) {
  
  # Defining type of standard errors based & cluster_arg on the presence of cluster vars 
  if (missing(cluster_var)) {
    se_type = "classical"
    clusters = NULL 
  } else {
    se_type = "stata"
    clusters = rlang::expr(!!rlang::sym(cluster_var))
  }
  
  
  # Defining formula vector based on the presence of control vars 
  if (missing(control_vars)) {
    formula_sin_y<-"{.}~factor({treatment})"
    formulas<-purrr::map_chr(endogenous_vars  , ~glue::glue(formula_sin_y))
    
  } else {
    controles_formula<- stringr::str_c(control_vars, collapse = "+")
    formula_sin_y<-stringr::str_c("{.}~factor({treatment}) + ", controles_formula)
    
    formulas<-purrr::map_chr(endogenous_vars  , ~glue::glue(formula_sin_y))
  }
  
  # Defining fixed effects arg expression; lm_robust doesn't support missing_arg
  if(length(fixed_effect_vars)>2) {
    stop("More than 2 fixed effect variables not supported")
  } else if (length(fixed_effect_vars) ==2 ){
    fixed_effects = rlang::expr(~!!rlang::sym(fixed_effect_vars[1])+!!rlang::sym(fixed_effect_vars[2]))
  } else if (length(fixed_effect_vars ==1)) { 
    fixed_effects = rlang::expr(!!rlang::sym(fixed_effect_vars))
  } else { 
      fixed_effects = NULL} 
  
  
  if (is.null(fixed_effects)) {
    ITT <- purrr::map(formulas,  
                      function(x) estimatr::lm_robust(formula =  stats::as.formula(x), 
                                                      se_type = se_type,
                                                      clusters = base::eval(clusters),
                                                      data = data) %>% 
                        broom::tidy(.) %>% 
                        dplyr::select(outcome, term, estimate, std.error, statistic, p.value))
    
    base::names(ITT)<-endogenous_vars
    
    
  } else {

    ITT <- purrr::map(formulas,  
                      function(x) estimatr::lm_robust(formula =  stats::as.formula(x), 
                                           fixed_effects = base::eval(fixed_effects),
                                           se_type = se_type, 
                                           clusters = base::eval(clusters),
                                           data = data) %>% 
                        broom::tidy(.) %>% 
                        dplyr::select(outcome, term, estimate, std.error, statistic, p.value))
    
    base::names(ITT)<-endogenous_vars
  }
  
  if (missing(heterogenous_vars)) { 
    
    return(ITT)
  } else {
    # Sorting by the heterogenous vars to match group by and vectors 
    data <- data %>% dplyr::arrange(!!!rlang::syms(heterogenous_vars))
    
    # Creating combinations of endogenous and het vars 
    matrix<-tidyr::expand_grid(heterogenous_vars, endogenous_vars)
    . <- NULL
    fit <- NULL
    endogenous_vars_final <- base::as.character(matrix$endogenous_vars)
    heterogenous_vars_final <- base::as.character(matrix$heterogenous_vars)
    formulas_het <- purrr::map_chr(endogenous_vars_final, 
                                   ~glue::glue(formula_sin_y))
    
    if (is.null(fixed_effects)) {
      ITT_het<-purrr::map2(heterogenous_vars_final, formulas_het,
                           function(x, y) data %>%
                             dplyr::group_by(!!rlang::sym(x)) %>% 
                             dplyr::do(fit = estimatr::lm_robust(stats::as.formula(y),
                                                                 clusters = base::eval(clusters),
                                                                 se_type = se_type, 
                                                                 data = .)) ) %>%
        purrr::map(., function(x) purrr::map_dfr(x$fit, broom::tidy)) 
      
      
    } else {  
    # Running regressions for all combinations+ broom
    ITT_het<-purrr::map2(heterogenous_vars_final, formulas_het,
                         function(x, y) data %>%
                           dplyr::group_by(!!rlang::sym(x)) %>% 
                           dplyr::do(fit = estimatr::lm_robust(stats::as.formula(y),
                                                               fixed_effects = base::eval(fixed_effects),
                                                               clusters = base::eval(clusters),
                                                               se_type = se_type, 
                                                               data = .)) ) %>%
      purrr::map(., function(x) purrr::map_dfr(x$fit, broom::tidy)) 
    }

    # Only keeping what we want per datafram
    ITT_het<-purrr::map(ITT_het, function(x) x %>%
      dplyr::select(outcome, term, estimate, std.error, statistic, p.value))
    
    # Creating the het_var vector within every dataframe
    ITT_het<-purrr::map2(heterogenous_vars_final, ITT_het, function(x,y){
      valores_het <-base::unique(dplyr::pull(data, !!rlang::sym(x)))
      y<- y %>% dplyr::mutate(`:=`(!!x, rep(valores_het, each = nrow(y)/length(valores_het)))) %>%
        dplyr::select(!!x, dplyr::everything())
   })
    
    # Names
    names(ITT_het)<-stringr::str_c(endogenous_vars_final, heterogenous_vars_final, sep = "_")
    
    
  }
  res_final<-c(ITT, ITT_het)
  return(res_final)
}
