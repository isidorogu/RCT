#' Creates balance table for the X variables across treatment status
#' @param data A data.frame, tibble or data.table
#' @param treatment a string with treatment status column
#' @return A tibble with Mean_value of each treatment status and p_values
#' @examples 
#' data <-data.frame(x = c(1:5), treatment = c(0,1,0,1,0))
#' balance_table(data, "treatment")
#' @details This function performs t.test(X~treatment) for each X column in data. Every value of 
#' treatment i.e 1,2,3,...N is compared against control value (0) or the first value of the treatment
#' column. For instance, If treatment column has values of (0,1,2,3), balance_table will return: 
#' the mean value of each treatment (for all X's), and the p_values of the t.test of (1,2,3) against
#' treatment = 0.  


balance_table<-function(data, treatment) {
  
  data <- data %>% arrange(!!sym(treatment))
  
  valores_trat <- unique(dplyr::pull(data, !!sym(treatment)))
  
  trats<-valores_trat[2:length(valores_trat)]
  
  bal_tables<-purrr::map(trats, function (x)
    data %>%
      dplyr::filter(!!sym(treatment) == valores_trat[1] | !!sym(treatment) ==  !!x))
  
  bal_tables<-purrr::map(bal_tables, function (x) x %>%
                    tidyr::pivot_longer(names_to = "variables", values_to = "value", -treatment) )
  
  
  # Creo por separado la primera para poner la media de control 

  bal_tables<-purrr::map(bal_tables, function(x) x %>% 
                    dplyr::group_by(variables) %>% 
                    dplyr::summarise(Media_control = t.test(value~treat)$estimate[1],
                              Media_trat = t.test(value~treat)$estimate[2], 
                              p_value = t.test(value~treat)$p.value))
  
  valores_trat<-as.character(valores_trat[2:length(valores_trat)])
  
  bal_tables <- purrr::map2_dfc(.x = bal_tables, .y = valores_trat,
                         function(x,y) rename_all(x, ~str_c(., y)))
  
  #Quedandome solo con una de variables y media_control
  medias_control <- names(bal_tables %>% dplyr::select(contains("control")) )
  variables_nombres <- names(bal_tables %>% dplyr::select(contains("variables")))
  medias_trat <- names(bal_tables %>% dplyr::select(contains("trat")))
  p_values <-names(bal_tables %>% dplyr::select(contains("p_value")))
  
  bal_tables<-
    bal_tables %>% 
    dplyr::select(variables_nombres[1], medias_control[1], medias_trat, p_values)
    
  
  
  
}


