% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power.R
\name{plot_compare_power}
\alias{plot_compare_power}
\title{Plot a comparison of different design powers}
\usage{
plot_compare_power(...)
}
\arguments{
\item{...}{Any number of data frame containing power results obtained from
the \code{cbc_power()} function, separated by commas.}
}
\value{
A plot comparing the power curves of different designs.
}
\description{
This function creates a ggplot2 object comparing the power curves of
different designs. Each design is color coded and each facet (sub plot)
is a model coefficient.
}
\examples{
\dontrun{
library(cbcTools)

# Generate all possible profiles
profiles <- cbc_profiles(
  price     = c(1, 1.5, 2, 2.5, 3),
  type      = c("Fuji", "Gala", "Honeycrisp"),
  freshness = c('Poor', 'Average', 'Excellent')
)

# Make designs to compare: full factorial vs bayesian d-efficient
design_random <- cbc_design(
  profiles = profiles,
  n_resp = 100, n_alts = 3, n_q = 6
)
# Same priors will be used in bayesian design and simulated choices
priors <- list(
  price     = -0.1,
  type      = c(0.1, 0.2),
  freshness = c(0.1, 0.2)
)
design_bayesian <- cbc_design(
  profiles  = profiles,
  n_resp = 100, n_alts = 3, n_q = 6, n_start = 1, method = "CEA",
  priors = priors, parallel = FALSE
)

# Obtain power for each design by simulating choices
power_random <- design_random |>
cbc_choices(obsID = "obsID", priors = priors) |>
  cbc_power(
    pars = c("price", "type", "freshness"),
    outcome = "choice", obsID = "obsID", nbreaks = 5, n_q = 6, n_cores = 2
  )
power_bayesian <- design_bayesian |>
  cbc_choices(obsID = "obsID", priors = priors) |>
  cbc_power(
    pars = c("price", "type", "freshness"),
    outcome = "choice", obsID = "obsID", nbreaks = 5, n_q = 6, n_cores = 2
  )

# Compare power of each design
plot_compare_power(power_bayesian, power_random)
}
}
