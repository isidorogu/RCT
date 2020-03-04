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
