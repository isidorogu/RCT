# RCT
RCT is a package that helps you focus on the statistics of the randomized control trials, rather than the heavy programming lifting. 

Let the steps of the design be a RCT: 

### 1. Get data in which you want to randomly assign treatment

This library has a function called **summary_statistics** to know the distribution of all covariates in data. 

### 2. Decide the share of observations that will go to treatment 

Suppose we have N observations in data. We want to know how many observations we need to assign to control that will enable a detection of impact of treatment statistically. This process is based upon (Athey and Imbens (2016)).

Lets start with the statistics to assess if the impact is significant:

$$T = \frac{\bar Y_1 - \bar Y_0 - \tau}{\sqrt{ \sigma^2(\frac{1}{N_T}+\frac{1}{N_c})} } \sim N(0,1)  $$

This means the T-statistic is distributed: 

$$T = \mathcal{N}(\frac{\tau}{\sqrt{ \sigma^2(\frac{1}{N_T}+\frac{1}{N_c})}}, 1) $$

Now, the probability the null will be reject at the $\alpha$ level is: 

$$Pr(|T| > \Phi^{-1}(1-\frac{\alpha}{2})) = \Phi (-\Phi^{-1}(1-\frac{\alpha}{2}) + \frac{\tau}{\sqrt{ \sigma^2(\frac{1}{N_T}+\frac{1}{N_c})}}) $$

We want the probability of rejecting the null hypothesis when it is false to be equal to $\beta$, the power. 

$$\beta = \Phi (-\Phi^{-1}(1-\frac{\alpha}{2}) + \frac{\tau}{\sqrt{ \sigma^2(\frac{1}{N_T}+\frac{1}{N_c})}}) $$
So, applying $\Phi^{-1}$ to both sides:

$$\Phi^{-1}(\beta) = -\Phi^{-1}(1-\frac{\alpha}{2}) + \frac{\tau}{\sqrt{ \sigma^2(\frac{1}{N_T}+\frac{1}{N_c})}} $$

Now, multiplying by $\frac{\sqrt N}{\sqrt N}$

Finally, $share control = \frac{N_c}{N}$ and  $1-sharecontrol = \frac{N_T}{N}$: 

$$\Phi^{-1}(\beta) = -\Phi^{-1}(1-\frac{\alpha}{2}) + \frac{\tau \sqrt N}{\sqrt{ \sigma^2(\frac{1}{1-sharecontrol}+\frac{1}{sharecontrol})}} $$

$$\Phi^{-1}(\beta) = -\Phi^{-1}(1-\frac{\alpha}{2}) + \frac{\tau \sqrt{ N sharecontrol(1-sharecontrol)}}{ \sigma} $$

$$\tau = \frac {[\Phi^{-1}(\beta) + \Phi^{-1}(1-\frac{\alpha}{2})]\sigma}{\sqrt{ N sharecontrol(1-sharecontrol)}}$$
Where $\tau$ is the minimum detectable effect. Note that, given N, $\sigma$, $\beta$ and $\alpha$, the minimum detectable effect is lowest when share control = 0.5. 

Finally, note that this formula applies when comparing each treatment to control. It only has to adjust N = N control + N treati. The function **tau_min** calculares $\tau$ globally (i.e. all treatment vs control) and for each treatment. 

### 3. Decide which variables to use for strata building

Prior to random assigment, one has to decide which **categorical** variables to build blocks. Hence, the blocks or strata are the group that combine every categorical variable. The cardinality of this groups are all the possible combinations of the chose categorial variables. 

To build categorical variables in a powerful way, function **n_tile_label** divides variables in the decided n groups, putting label of the range of each category to the variable. 

### 4. Random Assignment 

Once we have the blocking variables, we need to assign treatment status **within** each strata. Function treatment_assign performs such random assignment for any given number of treatment groups. Furthermore, it handles misfits. 

Misfits are defined as observations within each strata that are not really randomly assigned because when dividing the size of each strata N_strata to each treatment share, there are some remainder observations. 

For instance, let the following example: 

N_strata = 10 
share_control = $\frac{1}{3}$
share_treat_1 = $\frac{1}{3}$
share_treat_2 = $\frac{1}{3}$

First 3 units are assigned to control, second 3 units are assigned to treat 1 and the last 3 unit are assigned to treat 2. As you already notice, the last observation is the remainder. This is a misfit. Misfits alter the succesful random assignment because they are not. In the example, this observation is assigned to treat 2 non-randomly. 

The function **treatment_assign** handles misfits in three ways. 

- "NA" assigns the misfits to NAs, leaving the experiment with only the pure assigned observations. 

- "global" puts together the misfits of each strata into a single group and then assigns them randomly

- "strata" assigns misfits to treatment within each strata

### 5 Impact evaluation 

After running a RCT, the social scientist wants to know the ATE for one or several variables and the distribution of this impact within the blocking variables to check for Heterogenous Treatment Effects. Aditionally, if the experiment lasted for more than one period and panel-data is available, one must cluster the standard errors by each i unit and control for period fixed effects. Finally, if by chance one o more covariates are not balance, one would like to control for them. 

Function **impact_eval** does all this jobs in one single command. It runs all the ATE regressions for each endogenous variable, all the combinations of endogenous variables*heterogenous variables. 

For each combination the model run is:

$$Y_i = \alpha + \tau treatment + \epsilon $$

