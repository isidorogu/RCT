# Computation of the minimum detectable difference between control group and each treatment
# Isidoro Garcia Urquieta
# Description: This function calculates the minimum difference that could show significant
#              between any two given groups (e.g. control vs each treatment), given the
#              population size (N), the outcome variable and Type I and Type II probabilities

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

            outcome_var <- pull(outcome_var)

            share_ti <- (1-share_control)/(n_groups - 1)

            N_each_treat_comp <- N*share_control+N*share_ti

            share_control_ti <- share_control/(share_control + share_ti)

            variance <- var(outcome_var, na.rm = T)

            estadistico <- (qnorm(power) + qnorm(1 - significance/2))^2

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

        variance <- var(outcome_var, na.rm = T)

        estadistico  <- (qnorm(power) + qnorm(1 - significance/2))^2

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

    estadistico  = (qnorm(power) + qnorm(1 - significance/2))^2

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
}



