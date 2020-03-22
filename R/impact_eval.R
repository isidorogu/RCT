# Impact Evaluation: This macro function divides itself in two: 
# 1. Carries out impact evaluation of RCT: impact_eval
# 2. Graphs the impact evaluation results: impact_eval_graph


# 1. Carries out impact evaluation of RCT: impact_eval
# Description: This function carries out the impact evaluation of a RCT, including heterogenous effects
# Arguments: data, endogenous_vars, heterogenous_vars, cluster_var, treatment, fixed_effect_var, control_vars


impact_eval <- function(data, endogenous_vars, treatment, 
                         heterogenous_vars, cluster_vars = "0", 
                         fixed_effect_vars = "0", control_vars = "0") {
  
  # Poniendo como sumas los fixed effects, cluster std errors, controles 
  controles_formula<- stringr::str_c(control_vars, collapse = "+")
  cluster_formula<- stringr::str_c(cluster_vars, collapse = "+" )
  fixed_effect_formula<- stringr::str_c(fixed_effect_vars, collapse = "+")
  
  formula_sin_y <-str_c("{.}~factor({treatment}) + ", 
                       controles_formula, 
                       " | ", 
                       fixed_effect_formula, 
                       " | ", 
                       " 0 | ", 
                       cluster_formula)
  
  formulas<-map_chr(endogenous_vars  , ~glue::glue(formula_sin_y))

  ITT <- map(formulas,  ~lfe::felm(as.formula(.), data = data ) %>% tidy(.) )
  
  names(ITT)<-endogenous_vars
  
  if (missing(heterogenous_vars)) { 
    
    return(ITT)
  } else {
      

  # Heterogeneidades: mejor con formulas
    
    matrix<-expand.grid(heterogenous_vars, endogenous_vars)
    
    matrix<-
      matrix %>%
      dplyr::rename(variables_heterogeneas = Var1,
                    variables_endogenas = Var2)
    
    endogenous_vars_final<-as.character(matrix$variables_endogenas)
    heterogenous_vars_final<-as.character(matrix$variables_heterogeneas)
    

  formulas_het<-map_chr(endogenous_vars_final  , ~glue::glue(formula_sin_y))

  ITT_het<-map2(heterogenous_vars_final, formulas_het,
                          function(x, y) data %>%
                            group_by(!!sym(x)) %>% do(fit = lfe::felm(as.formula(y),
                                                                      data = .)) %>% broom::tidy(. , fit) )

  names(ITT_het)<-str_c(endogenous_vars_final, heterogenous_vars_final, sep = "_")

  regresiones_final<-c(ITT, ITT_het)
  
}

}
