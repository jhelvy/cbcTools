library(cbcTools)

# A simple conjoint experiment about apples

# Define the attributes and levels
levels <- list(
  price     = seq(1, 4, 0.5), # $ per pound
  type      = c('Fuji', 'Gala', 'Honeycrisp', 'Pink Lady', 'Red Delicious'),
  freshness = c('Excellent', 'Average', 'Poor')
)

# Generate all profiles with all attributes dummy-coded
profiles <- cbc_profiles(levels)

# Generate all profiles with price as a continuous variable
profiles <- cbc_profiles(levels, coding = c("C", "D", "D"))

# Make a fully random conjoint survey
survey <- cbc_design(
  profiles = profiles,
  n_resp   = 300, # Number of respondents
  n_alts   = 3,   # Number of alternatives per question
  n_q      = 6   # Number of questions per respondent
)

# Make a fully random conjoint survey with a "no choice" option
survey <- cbc_design(
  profiles  = profiles,
  n_resp    = 300, # Number of respondents
  n_alts    = 3,   # Number of alternatives per question
  n_q       = 6,   # Number of questions per respondent
  no_choice = TRUE
)

# # Make a D-efficient conjoint survey
# survey <- cbc_design(
#   profiles = profiles,
#   n_resp   = 300, # Number of respondents
#   n_alts   = 3,   # Number of alternatives per question
#   n_q      = 6,    # Number of questions per respondent
#   d_eff    = TRUE
# )







# # Make a labeled survey with each "type" appearing in each choice question
# survey_labeled <- makeSurvey(
#     doe       = doe,  # Design of experiment
#     nResp     = 2000, # Total number of respondents (upper bound)
#     nAltsPerQ = 3,    # Number of alternatives per question
#     nQPerResp = 6,    # Number of questions per respondent
#     group     = "type"
# )



# Simulate random choices for the survey
data <- cbc_choices(
    survey = survey,
    obsID  = "obsID"
)

# Simulate choices based on a utility model with the following parameters:
#   - 1 continuous "price" parameter
#   - 4 discrete parameters for "type"
#   - 2 discrete parameters for "freshness"
data <- cbc_choices(
    survey = survey,
    obsID  = "obsID",
    pars = list(
        price     = 0.1,
        type      = c(0.1, 0.2, 0.3, 0.4),
        freshness = c(0.1, -0.1))
)

# Simulate choices based on a utility model with the following parameters:
#   - 1 continuous "price" parameter
#   - 4 discrete parameters for "type"
#   - 2 random normal discrete parameters for "freshness"
#   - 2 interaction parameters between "price" and "freshness"
data <- cbc_choices(
    survey = survey,
    obsID  = "obsID",
    pars = list(
        price     = 0.1,
        type      = c(0.1, 0.2, 0.3, 0.4),
        freshness = randN(mu = c(0.1, -0.1), sigma = c(1, 2)),
        `price*freshness` = c(1, 2))
)
#
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
