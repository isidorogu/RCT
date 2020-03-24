#' Computation of the minimum detectable difference between control group and each treatment
#' @param outcome_var the variable for which you wish to test the impact of treatment 
#' @param N number of observations in the RCT, usually nrow(data)
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
#' tau_min(outcome_var = data$Y, N = nrow(data), share_control = seq(0,1,0.1), n_groups = 3)
#' @details This function calculates the minimum difference that could show significant 
#' E[Y(1)-Y(0)] = tau, between any two given groups (e.g. control vs each treatment), given the 
#' population size (N), the outcome variable, power and significance

#' @export
tau_min <- function(outcome_var,
                    N,
                    power = 0.8,
                    significance = 0.05,
                    share_control, n_groups = 2) {
    if (!(power > 0 & power < 1 & significance > 0 & significance < 1)) {
        stop("power and significan must be numbers between 0 and 1")
    }
    else {

    if (!("integer" %in% class(N))){
        stop("N must be integer")
    }
    else if (N < 0) {
        stop("N must be postive")
    }
    else {

    if ("data.frame" %in% class(outcome_var)) {

        if(ncol(outcome_var)>1) {

            stop("outcome_var must be a vector")

        }

        else if ("numeric" %in% class(pull(outcome_var))) {

            outcome_var <- dplyr::pull(outcome_var)

            share_ti <- (1-share_control)/(n_groups - 1)

            N_each_treat_comp <- N*share_control+N*share_ti

            share_control_ti <- share_control/(share_control + share_ti)

            variance <- stats::var(outcome_var, na.rm = T)

            estadistico <- (stats::qnorm(power) + stats::qnorm(1 - significance/2))^2

            tau_min_global <-  (estadistico*variance)/(N * share_control * (1 - share_control))

            tau_min_each_trat <- tau_min_global*(N * share_control * (1 - share_control))/
                (N_each_treat_comp * share_control_ti * (1 - share_control_ti))


            tau_min_DT <- data.frame(share_control = share_control,
                                 share_ti = share_ti,
                                 share_treat = share_ti*(n_groups-1),
                                 N = N,
                                 N_control = N*share_control,
                                 N_treat = (n_groups-1)*share_ti*N,
                                 N_ti = share_ti*N,
                                 tau_min_global = sqrt(tau_min_global),
                                 tau_min_each_treat = sqrt(tau_min_each_trat))


            return(tau_min_DT)

        } else if (!("numeric" %in% class(pull(outcome_var)))) {
            stop("outcome_var must be numeric")
        }
    } else if ("numeric" %in% class(outcome_var)) {
        share_ti <- (1-share_control)/(n_groups - 1)

        N_each_treat_comp <- N*share_control+N*share_ti

        share_control_ti <- share_control/(share_control + share_ti)

        variance <- stats::var(outcome_var, na.rm = T)

        estadistico  <- (stats::qnorm(power) + stats::qnorm(1 - significance/2))^2

        tau_min_global <-  (estadistico*variance)/(N * share_control * (1 - share_control))

        tau_min_each_trat <- tau_min_global*(N * share_control * (1 - share_control))/
            (N_each_treat_comp * share_control_ti * (1 - share_control_ti))


        tau_min_DT <- data.frame(share_control = share_control,
                             share_ti = share_ti,
                             share_treat = share_ti*(n_groups-1),
                             N = N,
                             N_control = N*share_control,
                             N_treat = (n_groups-1)*share_ti*N,
                             N_ti = share_ti*N,
                             tau_min_global = sqrt(tau_min_global),
                             tau_min_each_treat = sqrt(tau_min_each_trat))


        return(tau_min_DT)

    } else {

        stop("outcome_var is not numeric" )

        }
    }
}

}

#' Computation of the minimum detectable difference between control group and each treatment for a dicotomical variable
#' @param prior Pr(Y=1). 
#' @param N number of observations in the RCT, usually nrow(data)
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
#' tau_min(prior = 0.05, N = nrow(data), share_control = seq(0,1,0.1), n_groups = 3)
#' @details This function calculates the minimum difference that could show significant 
#' Pr[Y(1)-Y(0)] = tau, between any two given groups (e.g. control vs each treatment), given the 
#' population size (N), the outcome variable, power and significance

#' @export
tau_min_probability <- function(prior,
                                N,
                                power = 0.8,
                                significance = 0.05,
                                share_control,
                                n_groups = 2) {
    if (!(prior>0 & prior <1)){
        stop("prior must be a number between 0 and 1")
    } else {

    share_ti = (1-share_control)/(n_groups - 1)

    N_each_treat_comp = N*share_control+N*share_ti

    share_control_ti = share_control/(share_control + share_ti)

    variance = prior*(1-prior)

    estadistico  = (stats::qnorm(power) + stats::qnorm(1 - significance/2))^2

    tau_min_global =  (estadistico*variance)/(N * share_control * (1 - share_control))

    tau_min_each_trat = tau_min_global*(N * share_control * (1 - share_control))/
        (N_each_treat_comp * share_control_ti * (1 - share_control_ti))


    tau_min_DT <- tibble(share_control = share_control,
                         share_ti = share_ti,
                         share_treat = share_ti*(n_groups-1),
                         N = N,
                         N_control = N*share_control,
                         N_treat = (n_groups-1)*share_ti*N,
                         N_ti = share_ti*N,
                         tau_min_global = sqrt(tau_min_global),
                         tau_min_each_treat = sqrt(tau_min_each_trat))


    return(tau_min_DT)
    }
    }




