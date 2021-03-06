% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/N_min.R
\name{N_min}
\alias{N_min}
\title{N_min() computes the minimum population needed to detect difference between control group and each treatment, given a target minimum detectable effect}
\usage{
N_min(
  outcome_var,
  tau_min,
  power = 0.8,
  significance = 0.05,
  share_control,
  n_groups = 2
)
}
\arguments{
\item{outcome_var}{the variable for which you wish to test the impact of treatment}

\item{tau_min}{the target detectable effect (in outcome_var units)}

\item{power}{The level of power of the test (1 - Pr(Reject H_0 | H_0 True) ). Default is 0.8}

\item{significance}{The level of significance of the test Pr(Reject H_0 | H_0 False). Default is 0.05}

\item{share_control}{The share of observations in N assigned to control. This argument allows for sequences (i.e. seq(0,1,0.1))}

\item{n_groups}{Number of groups (control + # treatment groups)}
}
\value{
A tibble with the share_control and N observations in control group (N_control), 
the share and N of each treatment c(share_ti, N_ti), 
total share of treatment rows and N treated (share_treat, N_treat), N, 
the minimum detectable difference between control and all treatments together (tau_min_global),
the minimum detectable difference between control and each treatment (tau_min_each_treat)
}
\description{
N_min() computes the minimum population needed to detect difference between control group and each treatment, given a target minimum detectable effect
}
\details{
This function calculates the minimum experiment's population needed in order to detect at least a difference of tau_min 
statistically significantly. This is between any two given groups (e.g. control vs each treatment), given 
the outcome variable, power and significance
}
\examples{
data <- data.frame(y_1 = rbinom(n = 100, size = 1, prob = 0.3), 
                  y_2 = rnorm(n = 100, mean = 8, sd = 2))
N_min(data$y_1, tau_min = 0.01, share_control = seq(0,1,0.1), n_groups = 3)
}
