#' summary_statistics() Creates summary statistics table of all numeric variables in data 
#' @param data A data.frame, tibble or data.table
#' @param probs The quantiles to compute. Default is c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
#' @param na.rm whether to exclude NA's from calculations
#' @return A tibble with the Mean, N (not NA) and probs selects for each numeric column
#' @examples 
#' data <-data.frame(x = c(1:5), y = c(100, 200, 300, 410, 540), z = rep("c", 5))
#' summary_statistics(data)
#' @details This function computes the selected quantiles, mean and N values of all the numeric columns of data. 

#' @export
#' @importFrom magrittr %>%
summary_statistics<- function(data, probs = c(0, 0.05, 0.1, 0.25, 0.5,
                                                   0.75, 0.9, 0.95, 1), na.rm = T) {

    base <- data %>% dplyr::ungroup()

    variables_numericas <- base %>% dplyr::select_if(is.numeric)
    
    variables_numericas<- variables_numericas %>% dplyr::mutate_if(is.integer, ~as.double(.))
    
    statistic<-NULL

    estadisticas_descriptivas <- purrr::map_dfc(.x = variables_numericas,
                                                .f = function(x) stats::quantile(x, probs = probs, na.rm = na.rm))
    
    estadisticas_descriptivas$statistic<-base::as.character(probs) 
    
    estadisticas_descriptivas <-
        dplyr::select(estadisticas_descriptivas, statistic, dplyr::everything())

    medias <- purrr::map_dfc(.x = variables_numericas,
                             .f = ~mean(., na.rm = na.rm) )
    
    medias$statistic<-"mean"

    medias<-
        medias %>%
        dplyr::select(statistic, dplyr::everything())


    n_s <- purrr::map_dfc(.x = variables_numericas,
                             .f = ~sum(!is.na(.)) )
    
    n_s$statistic<-"n"
    
    n_s<-
        n_s %>%
        dplyr::select(statistic, dplyr::everything())
    
    estadisticas_descriptivas<-dplyr::bind_rows(medias, n_s ,estadisticas_descriptivas)
    
    estadisticas_descriptivas<-
        estadisticas_descriptivas %>% 
        tidyr::pivot_longer(-statistic, names_to = "variable") %>%
        tidyr::pivot_wider(names_from = "statistic")
    
    

    return(estadisticas_descriptivas)



}


