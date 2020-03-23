#' Robust treatment assignment by strata/blocks
#' @param data A data.frame, tibble or data.table
#' @param share_control share of the observations assigned to control group
#' @param n_t Number of treatments groups
#' @param strata_varlist vector of categorical variables to form the strata/blocks for random assignment. 
#' Should be in the form of vars(var1, var2, ...)
#' @param misfits How to handle the misfits. Default is "global". See Carril (2016) for details.
#' @param share_ti The share of each treatment group. If NULL (Default), each treatment group will 
#' have equal share.
#' @param seed A number used to set.seed(). 
#' @param key The key identifier column of data. 
#' @return A list: "data" = the data with key, treat, strata, misfit column., 
#' "summary_strata" = A summary tibble with the membership of each strata and its size.
#' @examples 
#' assigment<-treatment_assign(data = diamonds, share_control = 0.1, n_t = 3,
#'                             strata_varlist = vars(cut, color), missfits = "strata", 
#'                             seed = 1990, key = "z")
#' list2env(assigment, envir = .GlobalEnv)
#' table(data$treat, useNA = "ifany")
#' prop.table(table(data$treat, useNA = "ifany"))
#' @details This function creates a variable that indicates the treatment status. The random 
#' assignment is made by strata/blocks. It can handle equal or unequal treatment shares. 
#' Finally, it has three methods availables to handle misfits (same as randtreat in STATA):
#' "global": assigning the observations that couldn't be randomly assigned globally, 
#' "strata": assigning the observations that couldn't be randomly assigned by strata,
#' "NA": set the the treat observations that couldn't be randomly assigned to NA.

treatment_assign <- function(data,
                             share_control,
                             n_t = 2,
                             strata_varlist,
                             misfits = c("global", "NA", "strata"),
                             seed = 1990,
                             share_ti = rep(1/n_t - share_control/n_t, times = n_t),
                             key) {
    
    
    # Arrange by key
    data <- data %>% dplyr::arrange(!!sym(key))
    
    # Creating a random variable
    set.seed(seed)
    data$random <- runif(nrow(data))
    
    #Strata size
    data <-
        data %>%
        dplyr::group_by(!!!strata_varlist) %>%
        dplyr::mutate(n_strata := n())
    
    # Strata id
    data$strata<-dplyr::group_indices(data, data$n_strata)
    
    # Row number on each strata
    data <- data %>%
        dplyr::group_by(strata) %>%
        dplyr::arrange(strata, random) %>%
        dplyr::mutate(strata_index = dplyr::row_number())
    
    
    # Missfits 
    
    # Minimo comun multiplo de los denominadores de share_ti y share_control 
    # share control 
    denominator_control<-as.numeric(stringr::str_split(attr(MASS::fractions(share_control), "fracs"), pattern = "/")[[1]][2])
    denominator_treat<-as.numeric(stringr::str_split(attr(MASS::fractions(share_ti), "fracs"), pattern = "/")[[1]][2])
    
    divisor<-pracma::Lcm(denominator_control, denominator_treat)
    
    
    data <- 
        data %>%
        dplyr::group_by(strata) %>% 
        dplyr::mutate(missfit_strata = if_else(n_strata %% divisor == 0 , 0, 1), 
                      num_missfits = n_strata %% divisor, 
                      row_missfit = max(strata_index) - num_missfits, 
                      missfit = if_else(strata_index <= row_missfit, 0 , 1))
    
    # Strata summary
    resumen_strata <-
        data %>%
        group_by(strata, !!!strata_varlist) %>%
        summarise(n_strata := mean(n_strata), 
                  n_missfits := sum(missfit))
    
    
    # Primero asignamos sin missfits 
    
    data_missfits<- data %>% dplyr::filter(missfit == 1)
    data <-data %>% dplyr::filter(missfit == 0)
    
    
    #Strata size, recalculado
    data <-
        data %>%
        dplyr::group_by(strata) %>%
        dplyr::mutate(n_strata := n())
    
    
    # Treatment sequence, if equal
    group_sequence  <- c(0,
                         share_control,
                         share_ti)
    
    group_sequence<-cumsum(group_sequence)
    
    data <- data.table::as.data.table(data)
    
    for (i in 1:(n_t+2)) {
        
        data <- data[strata_index/n_strata <= group_sequence[i + 1] &
                         strata_index/n_strata > group_sequence[i], `:=`(treat, i - 1)]
        
        
        
    }
    
    # Missfits treatment
    
    if(missfits == "NA") { 
        
        data <-bind_rows(data, data_missfits)
        


    } else if (missfits == "global") {
            

        data_to_assign <- data_missfits %>% ungroup()
        
        data_to_assign$random2<-runif(n = nrow(data_to_assign))
        
        
        data_to_assign <-
            data_to_assign %>%
            dplyr::arrange(random2) %>%
            dplyr::mutate(index = dplyr::row_number())
        
        data_to_assign <- data.table::as.data.table(data_to_assign)
        

        for (i in 1:(n_t+2)) {
            
            data_to_assign <- data_to_assign[index/nrow(data_to_assign) <= group_sequence[i + 1] &
                                            index/nrow(data_to_assign) > group_sequence[i], 
                                            `:=`(treat, i - 1)]
            
        }
        
        data_to_assign$random2<-NULL
        data_to_assign$index<-NULL
        
        data<-dplyr::bind_rows(data, data_to_assign)
        

    }  else { 
        
        # En cada strata reasigno a los missfits 
        data_to_assign<-data_missfits
        data_to_assign$random2<-runif(n = nrow(data_to_assign))
        
        data_to_assign<-
            data_to_assign %>% 
            dplyr::group_by(strata) %>% 
            dplyr::mutate(n_strata_missfit = n())
        
        data_to_assign <-
            data_to_assign %>%
            dplyr::group_by(strata) %>%
            dplyr::arrange(strata, random2) %>%
            dplyr::mutate(index = dplyr::row_number())
        
        data_to_assign <-data.table::as.data.table(data_to_assign)
        
        
        for (i in 1:(n_t+2)) {
            
            data_to_assign <- data_to_assign[index/n_strata_missfit <= group_sequence[i + 1] &
                                                index/n_strata_missfit > group_sequence[i], 
                                            `:=`(treat, i - 1)]
            
        }
        

        data_to_assign$random2<-NULL
        data_to_assign$index<-NULL
        
        data<-dplyr::bind_rows(data, data_to_assign)
        
        }
    
    

    objetos<-list("summary_strata" = resumen_strata, "data" = data %>% dplyr::select(key, strata, treat, missfit))

    return(objetos)
    
    
}



#' Rank observations in n groups, with labels
#' @param var The variable wished to be ntile_label 
#' @param n rank the variable in n groups 
#' @param digits How many digits to include in the label 
#' @return A ordered factor vector of each n group. The value has the form of [min(n_i) - max(n_i)]
#' @examples 
#' ntile_label(var = diamonds$price, n = 10)
#' diamantes<- diamantes %>% mutate(price_deciles = ntile_label(price, 10))
#' @details n_tile_label is very similar to ntile from dplyr. But n_tile_label creates
#' the n groups and then labels them. For each group i, the value of the ntile_label is 
#' [min(i) - max(i)].

ntile_label <- function(var, n, digits = 0) {
    
    secuencia<-seq(0, 1, by = 1/n)
    cuantiles <- round(quantile(var, secuencia, na.rm = T), digits = digits)
    
    cuantiles2<-round(cuantiles[2:length(cuantiles)], digits = digits)
    label<-str_c("[", head(cuantiles, -1), " a ", cuantiles2, "]")
    
    referencia<-dplyr::tibble(grupos = seq(1, n), 
                       label = label)
    
    data<-dplyr::tibble(var = var, 
                 grupos = dplyr::ntile(var, n))
    
    data <-
        dplyr::left_join(data, referencia, by  = "grupos")
    
    data <-
        data %>% 
        mutate(label = fct_reorder(.f = label, .x = var))
    
    return(data$label)                      
}
