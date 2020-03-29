#' Creates balance table for the X variables across treatment status
#' @param data A data.frame, tibble or data.table
#' @param treatment a string with treatment status column
#' @return A tibble with Mean_value of each treatment status and p_values
#' @examples 
#' data <-data.frame(x1 = rnorm(n = 100, mean = 100, sd = 15), 
#'                   x2=rnorm(n = 100, mean = 65), 
#'                   treatment = rep(c(0,1,2,3,4), each = 20))
#' balance_table(data, "treatment")
#' @details This function performs t.test(X~treatment) for each X column in data. Every value of 
#' treatment i.e 1,2,3,...N is compared against control value (0) or the first value of the treatment
#' column. For instance, If treatment column has values of (0,1,2,3), balance_table will return: 
#' the mean value of each treatment (for all X's), and the p_values of the t.test of (1,2,3) against
#' treatment = 0.  

#' @export
#' @importFrom magrittr %>%
balance_table<-function(data, treatment) {
  
    variables<-NULL 
    
    data <- data %>% dplyr::arrange(!!rlang::sym(treatment))
    
    valores_trat <- base::unique(dplyr::pull(data, !!rlang::sym(treatment)))
    
    trats<-valores_trat[2:base::length(valores_trat)]
    
    bal_tables<-purrr::map(trats, function (x)
      data %>%
        dplyr::filter(!!rlang::sym(treatment) == valores_trat[1] | !!rlang::sym(treatment) ==  !!x))
    
    
    
    bal_tables<-purrr::map(bal_tables, function (x) x %>%
                             tidyr::pivot_longer(names_to = "variables", values_to = "value", -treatment) )
    
    
    # Creo por separado la primera para poner la media de control 
    
    bal_tables<-purrr::map(bal_tables, function(x) x %>% 
                             dplyr::group_by(variables) %>% 
                             dplyr::summarise(Media_control = stats::t.test(value~!!rlang::sym(treatment))$estimate[1],
                                              Media_trat = stats::t.test(value~!!rlang::sym(treatment))$estimate[2], 
                                              p_value = stats::t.test(value~!!rlang::sym(treatment))$p.value))
    
    valores_trat<-base::as.character(valores_trat[2:base::length(valores_trat)])
    
    bal_tables <- purrr::map2_dfc(.x = bal_tables, .y = valores_trat,
                                  function(x,y) dplyr::rename_all(x, ~stringr::str_c(., y)))
    
    #Quedandome solo con una de variables y media_control
    medias_control <- base::names(bal_tables %>% dplyr::select(dplyr::contains("control")) )
    variables_nombres <- base::names(bal_tables %>% dplyr::select(dplyr::contains("variables")))
    medias_trat <- base::names(bal_tables %>% dplyr::select(dplyr::contains("trat")))
    p_values <-base::names(bal_tables %>% dplyr::select(dplyr::contains("p_value")))
    
    bal_tables<-
      bal_tables %>% 
      dplyr::select(tidyselect::all_of(variables_nombres[1]), 
                    tidyselect::all_of(medias_control[1]), 
                    tidyselect::all_of(medias_trat), 
                    tidyselect::all_of(p_values))
    
}


