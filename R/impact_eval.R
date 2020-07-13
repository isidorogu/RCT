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
#' fixed_effects, contols and cluster variables for clustered std errors.

#' @export
#' @importFrom magrittr %>%
impact_eval <- function(data, endogenous_vars, treatment, 
                         heterogenous_vars, cluster_vars = "0", 
                         fixed_effect_vars = "0", control_vars) {
  
  # Poniendo como sumas los fixed effects, cluster std errors, controles 
  cluster_formula<- stringr::str_c(cluster_vars, collapse = "+" )
  fixed_effect_formula<- stringr::str_c(fixed_effect_vars, collapse = "+")
  
  if (missing(control_vars)) {
    
    formula_sin_y <-stringr::str_c("{.}~factor({treatment}) ",
                                   " | ", 
                                   fixed_effect_formula, 
                                   " | ", 
                                   " 0 | ", 
                                   cluster_formula)
    
    formulas<-purrr::map_chr(endogenous_vars  , ~glue::glue(formula_sin_y))
    
    ITT <- purrr::map(formulas,  ~lfe::felm(stats::as.formula(.), data = data ) %>% broom::tidy(.) )
    
    base::names(ITT)<-endogenous_vars
    
    
  } else {
    
    controles_formula<- stringr::str_c(control_vars, collapse = "+")
    
   formula_sin_y <-stringr::str_c("{.}~factor({treatment}) + ", 
                       controles_formula, 
                       " | ", 
                       fixed_effect_formula, 
                       " | ", 
                       " 0 | ", 
                       cluster_formula)
  
  formulas<-purrr::map_chr(endogenous_vars  , ~glue::glue(formula_sin_y))

  ITT <- purrr::map(formulas,  ~lfe::felm(stats::as.formula(.), data = data ) %>% broom::tidy(.) )
  
  base::names(ITT)<-endogenous_vars
  
  }
  
  if (missing(heterogenous_vars)) { 
    
    return(ITT)
  } else {
      

  # Heterogeneidades: mejor con formulas
    
    matrix<-base::expand.grid(heterogenous_vars, endogenous_vars)
    
    Var1<-NULL
    Var2<-NULL
    .<-NULL
    fit<-NULL
    matrix<-
      matrix %>%
      dplyr::rename(variables_heterogeneas = Var1,
                    variables_endogenas = Var2)
    
    endogenous_vars_final<-base::as.character(matrix$variables_endogenas)
    heterogenous_vars_final<-base::as.character(matrix$variables_heterogeneas)
    

  formulas_het<-purrr::map_chr(endogenous_vars_final  , ~glue::glue(formula_sin_y))

  if (utils::packageVersion("broom") > "0.5.6") {
    
    ITT_het<-purrr::map2(heterogenous_vars_final, formulas_het,
                            function(x, y) data %>%
                              dplyr::group_by(!!rlang::sym(x)) %>% 
                              dplyr::do(fit = lfe::felm(stats::as.formula(y),
                                                        data = .)) ) %>%
      purrr::map(., function(x) purrr::map_dfr(x$fit, broom::tidy))
    
    ITT_het<-purrr::map2(heterogenous_vars_final, ITT_het, 
                         function(x,y) { 
                           
                           valores_het<-base::unique(dplyr::pull(data, !!rlang::sym(x) ) ) 
                           
                           y<-
                             y %>% 
                             dplyr::mutate(!!x  := rep(valores_het, each = nrow(y)/length(valores_het) ) ) %>%
                             dplyr::select(!!x, dplyr::everything())
                         }
    )
    
  
    base::names(ITT_het)<-stringr::str_c(endogenous_vars_final, heterogenous_vars_final, sep = "_")
  
    regresiones_final<-c(ITT, ITT_het)
    
    return(regresiones_final)
  }
  else {
    
    ITT_het<-purrr::map2(heterogenous_vars_final, formulas_het,
                         function(x, y) data %>%
                           dplyr::group_by(!!rlang::sym(x)) %>% 
                           dplyr::do(fit = lfe::felm(stats::as.formula(y),
                                                     data = .)) %>% broom::tidy(., fit)) 
    
    base::names(ITT_het)<-stringr::str_c(endogenous_vars_final, heterogenous_vars_final, sep = "_")
    
    regresiones_final<-c(ITT, ITT_het)
    
    return(regresiones_final)
    
  }
}

}
