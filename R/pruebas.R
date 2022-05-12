library(fixest)
library(RCT)
library(tidyverse)
library(broom)


#################
# Prueba Basica 
################
data <- data.frame(y_1 = rnorm(n = 100, mean = 100, sd = 15), 
                   y_2 = rnorm(n = 100, mean = 8, sd = 2), 
                   treat = rep(c(0,1,2,3), each = 25), 
                   heterogenous_var1 = rep(c("X_Q1", "X_Q2", "X_Q3", "X_Q4"), times = 25),
                   cluster_var1 = rep(c(1:5), times = 20), 
                   fixed_effect_var1 = rep(c(1,2), times = 50),
                   control_var1 = rnorm(n = 100, mean = 20, sd = 1))


evaluation<-impact_eval(data = data, 
                        endogenous_vars = c("y_1", "y_2"), 
                        treatment = "treat", 
                        heterogenous_vars = c("heterogenous_var1"), 
                        cluster_vars = "cluster_var1", fixed_effect_vars = c("fixed_effect_var1"), 
                        control_vars = c("control_var1"))


# Corro fixest 
new<-feols(.[c("y_1", "y_2")] ~ .[c("control_var1")]+factor(treat), data)


# Extraccion de resultados : Usar broom::tidy 
new

new$y_1
a<-coeftable(new$y_1)
class(a)
colnames(a)
rownames(a)
?coeftable
pvalue(new$y_1)
as.tibble(a)



a<-bind_cols('var' = rownames(a), as.tibble(a))

tidy(new$y_1)


##############
# Prueba con efectos fijos 
#############
new_fe<-feols(.[c('y_1', 'y_2')] ~ factor(treat) + .[c('control_var1')] | .[c('fixed_effect_var1')], 
              data = data)

evaluation$y_1
tidy(new_fe$y_1)


# Dos efectos fijos 
data<-
  data %>% 
  mutate(fixed_effect_var2 = rep(c(1,2,3,4),  times= 25 ))

new_fe<-feols(.[c('y_1', 'y_2')] ~ factor(treat) + .[c('control_var1')] | 
                .[c('fixed_effect_var1', 'fixed_effect_var2')], 
              data = data)

tidy(new_fe$y_1)

#############
# Cluster std errors
##############
new_fe_cluster<-feols(.[c('y_1', 'y_2')] ~ factor(treat) + .[c('control_var1')] | .[c('fixed_effect_var1')], 
              cluster= 'cluster_var1', 
              data = data)

evaluation$y_1
tidy(new_fe_cluster$y_1)
evaluation$y_2
tidy(new_fe_cluster$y_2)


#############
# heterogenous variables 
#############

# Anado otra het_var para ejemplo multiple 
data$heterogenous_var2<-rep(c("X_Q1", "X_Q2"), each = 50)



# Multiples heterogeneidades 
het_vars<-c('heterogenous_var1', 'heterogenous_var2')

data_list<-data %>% split(.$'heterogenous_var1')

data_lists<-map(het_vars, function(x) data %>% split(data[[x]]))

names(data_lists)<-het_vars

final<-map(data_lists, function(x) { 
  map(x, function(y) {
    z<-feols(.[c('y_1', 'y_2')] ~ factor(treat) + .[c('control_var1')] | .[c('fixed_effect_var1')], 
          cluster= 'cluster_var1', 
          data = y)
   z<-map(z, ~tidy(.))
  })
  })

# valores en tablas 

  
# Una opcion
final2<-map(final, ~reduce(., bind_rows))
a<-cross(final2)


# Aplicar valores de het_vars 




# Group by do 




