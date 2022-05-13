# RCT
RCT helps you focus on the statistics of the randomized control trials, rather than the heavy programming lifting. Any social scientist should use RCT when conducting Field Experiments.

RCT helps you in the whole process of designing and evaluating a RCT. 

Let the steps of the design be a RCT: 

### 1. Get data in which you want to randomly assign treatment

This library has a function called **summary_statistics** to know the distribution of all covariates in data. 

### 2. Decide the share of observations that will go to control group 

The function **tau_min** calculates the minimum detectable treatment effect given power, significance level, outcome variable and number of observations.  This function computes this for any share of control observations. 

The function **N_min** calculates the minimum population needed to identify a target tau_min

### 3. Decide which variables to use for strata building

Prior to random assignment, one has to decide which **categorical** variables to build blocks. Hence, the blocks or strata are the group that combine every categorical variable. The cardinality of this groups are all the possible combinations of the chose categorical variables. 

To build categorical variables in a powerful way, function **n_tile_label** divides variables in the decided n groups, putting label of the range of each category to the variable. 

### 4. Random Assignment 

Once we have the blocking variables, we need to assign treatment status **within** each strata. Function treatment_assign performs such random assignment for any given number of treatment groups. Furthermore, it handles misfits. 

Misfits are defined as observations within each strata that are not really randomly assigned because when dividing the size of each strata N_strata to each treatment share, there are some remainder observations. 

For instance, let the following example: 

N_strata = 10 
share_control = $\frac{1}{3}$
share_treat_1 = $\frac{1}{3}$
share_treat_2 = $\frac{1}{3}$

First 3 units are assigned to control, second 3 units are assigned to treat 1 and the last 3 unit are assigned to treat 2. As you already notice, the last observation is the remainder. This is a misfit. Misfits alter the successful random assignment because they are not. In the example, this observation is assigned to treat 2 non-randomly. 

The function **treatment_assign** handles misfits in three ways. 

- "NA" assigns the misfits to NAs, leaving the experiment with only the pure assigned observations. 

- "global" puts together the misfits of each strata into a single group and then assigns them randomly

- "strata" assigns misfits to treatment within each strata

### 5 Impact evaluation 

After running a RCT, the social scientist wants to know the ATE for one or several variables and the distribution of this impact within the blocking variables to check for Heterogenous Treatment Effects. Additionally, if the experiment lasted for more than one period and panel-data is available, one must cluster the standard errors by each i unit and control for period fixed effects. Finally, if by chance one o more covariates are not balance, one would like to control for them. 

Function **impact_eval** does all this jobs in one single command. It runs all the ATE regressions for each endogenous variable, all the combinations of endogenous variables*heterogenous variables. 

For each combination the model run is:

$$Y_i = \alpha + \tau treatment + \epsilon $$

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/isidorogu/RCT.svg?branch=master)](https://travis-ci.org/isidorogu/RCT)
[![R-CMD-check](https://github.com/isidorogu/RCT/workflows/R-CMD-check/badge.svg)](https://github.com/isidorogu/RCT/actions)
<!-- badges: end -->
