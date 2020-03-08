# Random Assignment: This macro function divides itself in 3 functions:
# 1. Create Treatment status var: treatment_status_var
# 2. Create strata variable: rct_strata
# 3. Create cuartiles, ntile with label: rct_ntile


# 1. Create Treatment status var: treatment_status_var
# Description: This function creates a variable that indicates the treatment status
# Arguments: treat_var, share_control, n treatment groups, strata, missfits, seed, equal
# miss fits: NA,
#           strata (missfits allocated to strata randomly),
#           global (assigning missfits to treatment). estratos pequenos y missfits 



# Equal fractions, los misfits son el modulo de n_strata / n_trats.
# Unequal fractions,

treatment_assign <- function(data,
                             share_control,
                             n_t = 2,
                             strata_varlist,
                             missfits = c("global", "NA", "strata"),
                             seed = 1990,
                             share_ti = 1/n_t - share_control/n_t,
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
    seq_treat<-seq(1, n_t)
    group_sequence  <- c(0,
                         share_control,
                         share_control + seq_treat * (1-share_control)/n_t)
    
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

