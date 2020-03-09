# Elementos para la funcion

# outcome_var: debe ser un vector numerico. YA
# N: debe ser un numero, positivo
# significance, power: numero entre 0 y 1
# share_control: numero entre cero y 1
# n_groups: numero entero > 2


# Prueba 1 de la funcion
library(tidyverse)
diamonds<-diamonds

# outcome
a<-tau_min(diamonds %>% select(depth),
                 N = nrow(diamonds), share_control = seq(0.05, 0.95, 0.05))

b<-tau_min(diamonds$depth,
           N = nrow(diamonds), share_control = seq(0.05, 0.95, 0.05))


# Numerico
c<-tau_min_probability("c", nrow(diamonds), share_control = seq(0.05,0.95, 0.05))

d<-tau_min(diamonds$depth,
           N = 1.5, share_control = seq(0.05, 0.95, 0.05))


e<-tau_min(diamonds$depth, N = nrow(diamonds), power = 0.9)


#############################


estadisticas_descriptivas(data = diamonds)
estadisticas_descriptivas(data = diamonds, na.rm = F)


###########################

diamonds<-diamonds


# Una variable estratificadora, equal shares, global
assigment<-treatment_assign(data = diamonds, 
                            share_control = 0.1, 
                            n_t = 3, 
                            strata_varlist = vars(cut), 
                            missfits = "global", 
                            seed = 1990, 
                            key = "z")



list2env(assigment, envir = .GlobalEnv)
table(data$treat, useNA = "ifany")
prop.table(table(data$treat, useNA = "ifany"))

# Una variable estratificadora, equal shares, strata
rm(assigment, data, summary_strata)

assigment<-treatment_assign(data = diamonds, 
                            share_control = 0.1, 
                            n_t = 3, 
                            strata_varlist = vars(cut, color), 
                            missfits = "strata", 
                            seed = 1990, 
                            key = "z")



list2env(assigment, envir = .GlobalEnv)
table(data$treat, useNA = "ifany")
prop.table(table(data$treat, useNA = "ifany"))


# dos variables, NA, equal shares
rm(assigment, data, summary_strata)

assigment<-treatment_assign(data = diamonds, 
                            share_control = 0.1, 
                            n_t = 3, 
                            strata_varlist = vars(cut, color), 
                            missfits = "NA", 
                            seed = 1990, 
                            key = "z")



list2env(assigment, envir = .GlobalEnv)
table(data$treat, useNA = "ifany")
prop.table(table(data$treat, useNA = "ifany"))



# Unequal fractions 

rm(assigment, data, summary_strata)

assigment<-treatment_assign(data = diamonds, 
                            share_control = 0.1, 
                            n_t = 3, 
                            strata_varlist = vars(cut, color), 
                            share_ti = c(0.4, 0.2, 0.3),
                            missfits = "global", 
                            seed = 1990, 
                            key = "z")



list2env(assigment, envir = .GlobalEnv)
table(data$treat, useNA = "ifany")
prop.table(table(data$treat, useNA = "ifany"))



####################
# ntile_label
####################

ntile_label(var = diamonds$price, n = 10)

diamantes<-diamonds

diamantes<-
  diamantes %>%
  mutate(price_deciles = ntile_label(price, 10))


diamantes %>% 
  group_by(price_deciles) %>% 
  summarise(min = min(price), 
            max = max(price))



##############
# Balance table 
##############

library(tidyverse)
library(data.table)

diamonds<-bind_cols(diamonds, data %>% select(strata, treat, missfit))

#a<-balance_table(data = diamantes %>% select(price, x, y, z, treat), treatment = "treat")
a<-balance_table(data = diamonds %>% select(price, x, y, z, treat), treatment = "treat")

rm(a, b, bal1, diamantes, tabla_0_1)
rm(trats)



######################
# Balance regressions 
######################
b<-balance_regression(data = diamonds, treatment = "treat")
list2env(b, envir = .GlobalEnv)

