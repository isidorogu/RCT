#' Runs a LPM of treatment status against all covariates (treatment~X'B). 
#' @param data A data.frame, tibble or data.table
#' @param treatment a string with treatment status column
#' @return A list: "regression_tables" = regression output of treatment~X'B, "F_test" = table with the F tests of each regression
#' @examples 
#' data <-data.frame(x = c(1:5), treatment = c(0,1,0,1,0))
#' balance_regression(data, "treatment")
#' @details This functions runs a Linear Probability model of each treatment group & control on all the 
#' columns in data. For instance, if treatment column has values of (0,1,2), balance_regression will run two 
#' models: 1) LPM(treatment(0,1)~X'b) and 2) LPM(treatment(0,2)~X'b). The value are the regression tables and 
#' details of the F_test of these models.
#' @export
#' @importFrom magrittr %>%
balance_regression <- function(data, treatment) { 
    
    estadistico<-NULL
    
    data <- data %>% dplyr::arrange(!!rlang::sym(treatment))
    
    valores_trat <- base::unique(dplyr::pull(data, !!rlang::sym(treatment)))
    
    trats<-valores_trat[2:base::length(valores_trat)]
    
    bal_tables<-purrr::map(trats, function (x)
        data %>%
            dplyr::filter(!!rlang::sym(treatment) == valores_trat[1] | !!rlang::sym(treatment) ==  !!x))
    
    formula<-glue::glue("{treatment}~ .")
    regressions <- purrr::map(bal_tables, ~stats::lm(as.formula(formula), data = .))
    
    nombres<-stringr::str_c("Msj", trats)
    
    names(regressions)<-nombres
    
    regression_tables<-purrr::map_dfc(regressions, function(x) broom::tidy(x))

    f<-purrr::map_dfr(regressions, function(x) base::summary(x)$fstatistic)
    p_values<-purrr::map_dbl(f, function(x) stats::pf(x[1], x[2], x[3], lower.tail = F))
    f_star <- purrr::map_dbl(f, function(x) stats::qf(p = 0.05, df1 = x[2], df2 = x[3]))
    
    f <- dplyr::bind_rows(f, f_star, p_values)
    
    r_cuadrada <- purrr::map_dbl(regressions, function(x) base::summary(x)$r.squared)
    
    f <-dplyr::bind_rows(f, r_cuadrada)

    f <-
        f %>%
        dplyr::mutate(estadistico = c("F-statistic", "k", "n-k-1", "F_critical", "p_value", "R cuadrada")) %>%
        dplyr::select(estadistico, dplyr::everything())


    objetos <- base::list("regression_tables" = regression_tables, "F_test" = f)

    return(objetos)
    
    }





