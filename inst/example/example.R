library(cbcTools)

# A simple conjoint experiment about apples

# Define the attributes and levels
levels <- list(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c("Fuji", "Gala", "Honeycrisp"),
  freshness = c("Excellent", "Average", "Poor")
)

# Generate all all possible profiles
profiles <- cbc_profiles(levels)

# Make a randomized survey design
design_rand <- cbc_design(
  profiles = profiles,
  n_resp   = 300, # Number of respondents
  n_alts   = 3, # Number of alternatives per question
  n_q      = 6 # Number of questions per respondent
)

# Make a randomized survey design with a "no choice" option
design_rand_nochoice <- cbc_design(
  profiles  = profiles,
  n_resp    = 300, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  no_choice = TRUE
)

# Make randomized labeled survey design with each "type" appearing in each
# choice question
design_rand_labeled <- cbc_design(
  profiles  = profiles,
  n_resp    = 300, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  label     = "type"
)

# Make randomized labeled survey design with each "type" appearing in each
# choice question and with a "no choice" option
design_rand_labeled_nochoice <- cbc_design(
  profiles  = profiles,
  n_resp    = 300, # Number of respondents
  n_alts    = 3, # Number of alternatives per question
  n_q       = 6, # Number of questions per respondent
  no_choice = TRUE,
  label     = "type"
)

# Simulate random choices for a survey design
data_rand <- cbc_choices(
  design = design_rand,
  obsID  = "obsID"
)

# Simulate choices based on a utility model with the following parameters:
#   - 1 continuous "price" parameter
#   - 2 categorical parameters for "type" (first level is reference)
#   - 2 categorical parameters for "freshness" (first level is reference)
data_prior <- cbc_choices(
  design = design_rand,
  obsID = "obsID",
  priors = list(
    price     = 0.1,
    type      = c(0.1, 0.2),
    freshness = c(0.1, -0.2)
  )
)

# Simulate choices based on a utility model with the following parameters:
#   - 1 continuous "price" parameter
#   - 2 categorical parameters for "type" (first level is reference)
#   - 2 categorical parameters for "freshness" (first level is reference)
#   - 2 interaction parameters between "price" and "type"
data_prior_int <- cbc_choices(
  design = design_rand,
  obsID = "obsID",
  priors = list(
    price = 0.1,
    type = c(0.1, 0.2),
    freshness = c(0.1, -0.2),
    `price*type` = c(0.1, 0.5)
  )
)

# Simulate choices based on a utility model with the following parameters:
#   - 1 continuous "price" parameter
#   - 2 random normal discrete parameters for "type" (first level is reference)
#   - 2 categorical parameters for "freshness" (first level is reference)
data_prior_mixed <- cbc_choices(
  design = design_rand,
  obsID = "obsID",
  priors = list(
    price = 0.1,
    type = randN(mu = c(0.1, 0.2), sigma = c(0.5, 1)),
    freshness = c(0.1, -0.2)
  )
)




# # Estimate models with different sample sizes
# models <- estimateModels(
#     nbreaks = 10,
#     data    = data,
#     pars    = c("price", "type", "freshness"),
#     outcome = "choice",
#     obsID   = "obsID"
# )
#
# # Extract coefficients and standard errors from models
# results <- getModelResults(models)
# head(results)
# tail(results)
#
# # View summary of standard errors for each sample size
# library(ggplot2)
#
# ggplot(results) +
#   geom_hline(yintercept = 0.05, color = "red", linetype = 2) +
#   geom_point(aes(x = sampleSize, y = se, color = coef)) +
#   expand_limits(y = 0) +
#   theme_bw()
#
# # Compare estimated coefficients to true parameters for each sample size
# truePars <- data.frame(
#   coef = c(
#     "price", "typeGala", "typeHoneycrisp", "typePink Lady",
#     "typeRed Delicious", "freshnessExcellent", "freshnessPoor"),
#   est_true = c(0.1, 0.1, 0.2, 0.3, 0.4, 0.1, -0.1))
#
# compare <- merge(results, truePars, by = "coef")
# compare$diff = abs(compare$est_true - compare$est)
#
# ggplot(compare) +
#   geom_point(aes(x = sampleSize, y = diff)) +
#   facet_wrap(vars(coef)) +
#   theme_bw()
