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

balance_regression <- function(data, treatment) { 
    
    data <- data %>% dplyr::arrange(!!sym(treatment))
    
    valores_trat <- unique(dplyr::pull(data, !!sym(treatment)))
    
    trats<-valores_trat[2:length(valores_trat)]
    
    bal_tables<-purrr::map(trats, function (x)
        data %>%
            dplyr::filter(!!sym(treatment) == valores_trat[1] | !!sym(treatment) ==  !!x))
    
    formula<-glue("{treatment}~ .")
    regressions <- map(bal_tables, ~lm(as.formula(formula), data = .))
    
    nombres<-str_c("Msj", trats)
    
    names(regressions)<-nombres
    
    regression_tables<-map_dfc(regressions, function(x) x %>% broom::tidy(.))

    f<-map_dfr(regressions, function(x) summary(x)$fstatistic)
    p_values<-map_dbl(f, function(x) pf(x[1], x[2], x[3], lower.tail = F))
    f_star <- map_dbl(f, function(x) qf(p = 0.05, df1 = x[2], df2 = x[3]))
    
    f <- bind_rows(f, f_star, p_values)
    
    r_cuadrada <- map_dbl(regressions, function(x) summary(x)$r.squared)
    
    f <-bind_rows(f, r_cuadrada)

    f <-
        f %>%
        mutate(estadistico = c("F-statistic", "k", "n-k-1", "F_critical", "p_value", "R cuadrada")) %>%
        select(estadistico, everything())


    objetos <- list("regression_tables" = regression_tables, "F_test" = f)

    return(objetos)
    
    }





