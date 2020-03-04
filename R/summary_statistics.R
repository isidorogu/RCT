##########################################################################################
# Summary statistics of data
# Isidoro Garcia Urquieta
# Description: This function returns the summary statistics of all the numerics variables
#              in the data provided.
#########################################################################################

estadisticas_descriptivas <- function(data, probs = c(0, 0.05, 0.1, 0.25, 0.5,
                                                   0.75, 0.9, 0.95, 1), na.rm = T) {

    base <- data %>% dplyr::ungroup()

    variables_numericas <- base %>% dplyr::select_if(is.numeric)

    estadisticas_descriptivas <- purrr::map_dfc(.x = variables_numericas,
                                                .f = function(x) quantile(x, probs = probs))

    estadisticas_descriptivas$probs<-as.character(probs)

    estadisticas_descriptivas<-
        estadisticas_descriptivas %>%
        dplyr::select(probs, everything())

    medias <- purrr::map_dfc(.x = variables_numericas,
                             .f = ~mean(., na.rm = na.rm))

    medias$probs<-"mean"

    medias <-
        medias %>%
        dplyr::select(probs, everything())

    estadisticas_descriptivas<-dplyr::bind_rows(estadisticas_descriptivas, medias)


    return(estadisticas_descriptivas)



}


