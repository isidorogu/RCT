# balance_regression: Runs a LPM on trat~X'B
# Description: This function runs a regression to assess which variables a relevant to determine
#              treatment status. It is specially useful for RCTs with high dimensional data.
# Arguments: data, treatment 

balance_regression <- function(data, treatment) { 
    
    valores_trat <- unique(dplyr::pull(data, !!sym(treatment)))
    
    trats<-valores_trat[2:length(valores_trat)]
    
    bal_tables<-purrr::map(trats, function (x)
        data %>%
            dplyr::filter(!!sym(treatment) == valores_trat[1] | !!sym(treatment) ==  !!x))
    
#    regressions<-map(bal_tables, function(x) x %>% do(fit = lm(!!sym(treatment) ~ ., data = x)))
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





