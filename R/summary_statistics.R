##########################################################################################
# Summary statistics of data
# Isidoro Garcia Urquieta
# Description: This function returns the summary statistics of all the numerics variables
#              in the data provided.
#########################################################################################

summary_statistics<- function(data, probs = c(0, 0.05, 0.1, 0.25, 0.5,
                                                   0.75, 0.9, 0.95, 1), na.rm = T) {

    base <- data %>% dplyr::ungroup()

    variables_numericas <- base %>% dplyr::select_if(is.numeric)

    estadisticas_descriptivas <- purrr::map_dfc(.x = variables_numericas,
                                                .f = function(x) quantile(x, probs = probs, na.rm = na.rm))
    
    estadisticas_descriptivas$statistic<-as.character(probs) 
    
    estadisticas_descriptivas <-
        estadisticas_descriptivas %>% 
        select(statistic, everything())

    medias <- purrr::map_dfc(.x = variables_numericas,
                             .f = ~mean(., na.rm = na.rm) )
    
    medias$statistic<-"mean"

    medias<-
        medias %>%
        select(statistic, everything())


    n_s <- purrr::map_dfc(.x = variables_numericas,
                             .f = ~sum(!is.na(.)) )
    
    n_s$statistic<-"n"
    
    n_s<-
        n_s %>%
        select(statistic, everything())
    
    estadisticas_descriptivas<-dplyr::bind_rows(medias, n_s ,estadisticas_descriptivas)
    
    estadisticas_descriptivas<-
        estadisticas_descriptivas %>% 
        pivot_longer(-statistic, names_to = "variable") %>%
        pivot_wider(names_from = "statistic")
    
    

    return(estadisticas_descriptivas)



}


