#' N_min() computes the minimum population needed to detect difference between control group and each treatment, given a target minimum detectable effect
#' @param outcome_var the variable for which you wish to test the impact of treatment 
#' @param tau_min the target detectable effect (in outcome_var units)
#' @param significance The level of significance of the test Pr(Reject H_0 | H_0 False). Default is 0.05
#' @param power The level of power of the test (1 - Pr(Reject H_0 | H_0 True) ). Default is 0.8
#' @param share_control The share of observations in N assigned to control. This argument allows for sequences (i.e. seq(0,1,0.1))
#' @param n_groups Number of groups (control + # treatment groups)
#' @return A tibble with the share_control and N observations in control group (N_control), 
#' the share and N of each treatment c(share_ti, N_ti), 
#' total share of treatment rows and N treated (share_treat, N_treat), N, 
#' the minimum detectable difference between control and all treatments together (tau_min_global),
#' the minimum detectable difference between control and each treatment (tau_min_each_treat)
#' @examples 
#'data <- data.frame(y_1 = rbinom(n = 100, size = 1, prob = 0.3), 
#'                   y_2 = rnorm(n = 100, mean = 8, sd = 2))
#' N_min(data$y_1, tau_min = 0.01, share_control = seq(0,1,0.1), n_groups = 3)
#' @details This function calculates the minimum experiment's population needed in order to detect at least a difference of tau_min 
#' statistically significantly. This is between any two given groups (e.g. control vs each treatment), given 
#' the outcome variable, power and significance
#' @export
#' @importFrom magrittr %>%
N_min <- function(outcome_var,
                    tau_min,
                    power = 0.8,
                    significance = 0.05,
                    share_control, n_groups = 2) {
    if (!(power > 0 & power < 1 & significance > 0 & significance < 1)) {
        stop("power and significan must be numbers between 0 and 1")
    }
    else {
        
        if (tau_min<0){
            stop("tau_min must be positive")
        }
        else if ("data.frame" %in% class(outcome_var) ){
            
            outcome_var <- dplyr::pull(outcome_var)
            
            share_ti <- (1-share_control)/(n_groups - 1)
            
            share_control_ti <- share_control/(share_control + share_ti)
            
            variance <- stats::var(outcome_var, na.rm = T)
            
            estadistico <- (stats::qnorm(power) + stats::qnorm(1 - significance/2))^2
            
            N_min_global <-  (estadistico*variance)/(tau_min^2 * share_control * (1 - share_control))
            
            
            N_min_each_trat <- N_min_global*(tau_min^2 * share_control * (1 - share_control))/
                (tau_min^2 * share_control_ti * (1 - share_control_ti))
            
            
            N_min_DT <- data.frame('N_min_global' = N_min_global,
                                   'N_min_each_treat' = N_min_each_trat,
                                   'N_min_each_treat_total' = (n_groups - 1)*N_min_each_trat,
                                   'share_control' = share_control,
                                   'share_ti' = share_ti,
                                   'share_treat' = share_ti*(n_groups-1),
                                   'tau_min' = tau_min)
            
            
            return(N_min_DT)
            
        }
        else if (length(outcome_var)==1){
        variance<-outcome_var*(1-outcome_var)
        
        share_ti <- (1-share_control)/(n_groups - 1)
        
        share_control_ti <- share_control/(share_control + share_ti)
        
        estadistico <- (stats::qnorm(power) + stats::qnorm(1 - significance/2))^2
        
        N_min_global <-  (estadistico*variance)/(tau_min^2 * share_control * (1 - share_control))
        
       
        N_min_each_trat <- N_min_global*(tau_min^2 * share_control * (1 - share_control))/
            (tau_min^2 * share_control_ti * (1 - share_control_ti))
        
        
        N_min_DT <- data.frame('N_min_global' = N_min_global,
                               'N_min_each_treat' = N_min_each_trat,
                               'N_min_each_treat_total' = (n_groups - 1)*N_min_each_trat,
                               'share_control' = share_control,
                               'share_ti' = share_ti,
                               'share_treat' = share_ti*(n_groups-1),
                               'tau_min' = tau_min)
        
        
        return(N_min_DT)
        
    }    

    else {
        share_ti <- (1-share_control)/(n_groups - 1)
        
        share_control_ti <- share_control/(share_control + share_ti)
        
        variance <- stats::var(outcome_var, na.rm = T)
        
        estadistico <- (stats::qnorm(power) + stats::qnorm(1 - significance/2))^2
        
        N_min_global <-  (estadistico*variance)/(tau_min^2 * share_control * (1 - share_control))
        

        N_min_each_trat <- N_min_global*(tau_min^2 * share_control * (1 - share_control))/
            (tau_min^2 * share_control_ti * (1 - share_control_ti))
        
        
        N_min_DT <- data.frame('N_min_global' = N_min_global,
                               'N_min_each_treat' = N_min_each_trat,
                               'N_min_each_treat_total' = (n_groups - 1)*N_min_each_trat,
                               'share_control' = share_control,
                               'share_ti' = share_ti,
                               'share_treat' = share_ti*(n_groups-1),
                               'tau_min' = tau_min)
        
        
        return(N_min_DT)
        
    }
        
    }
    
}
    
    
        
