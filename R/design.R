#' Make a choice-based conjoint survey design
#'
#' This function creates a data frame containing a choice-based conjoint survey
#' design where each row is an alternative. Generate a variety of survey
#' designs, including random design, D-efficient designs, and Bayesian
#' D-efficient designs as well as designs with "no choice" options, blocking,
#' and "labeled" designs (also known as "alternative specific" designs).
#'
#' @keywords experiment design mnl mxl mixed logit logitr idefix DoE.base
#' @param profiles A data frame in which each row is a possible profile
#'   generated using the `cbc_profiles()` function.
#' @param n_alts Number of alternatives per choice question.
#' @param n_q Number of questions per respondent.
#' @param n_blocks Number of blocks used D-efficient or Bayesian D-efficient
#'   designs. Max allowable is one block per respondent. Defaults to `1`,
#'   meaning every respondent sees the same choice set.
#' @param n_start A numeric value indicating the number of random start designs
#'   to use in obtaining a D-efficient or Bayesian D-efficient design.
#'   The default is `1`. Increasing `n_start` can result in a more efficient
#'   design at the expense of increased computational time.
#' @param no_choice Include a "no choice" option in the choice sets? Defaults to
#'   `FALSE`. If `TRUE`, the total number of alternatives per question will be
#'   one more than the provided `n_alts` argument.
#' @param label The name of the variable to use in a "labeled" design (also
#'   called an "alternative-specific" design) such that each set of alternatives
#'   contains one of each of the levels in the `label` attribute. If used, the
#'   `n_alts` argument will be ignored as its value is defined by the unique
#'   number of levels in the `label` attribute Defaults to `NULL`.
#' @param method Choose the design method to use. Options include `"random"`
#'   and `"sequential"`. Defaults to `"random"`. See details below for complete
#'   description of each method.
#' @param randomize Logical. If `TRUE`, both the choice question order and the
#'   order of the alternatives within a given choice question will be
#'   randomized across each respondent. Does not apply to design created using
#'   the `"random"` method. Defaults to `TRUE`.
#' @param priors A list of one or more assumed prior parameters used to generate
#'   a D-efficient or Bayesian D-efficient design. Defaults to `NULL`
#' @param prior_no_choice Prior utility value for the "no choice" alternative.
#'   Only required if `no_choice = TRUE`. Defaults to `NULL`.
#' @param max_iter A numeric value indicating the maximum number allowed
#'   iterations when searching for a D-efficient or Bayesian D-efficient design.
#'   The default is 50.
#'
#' @details The `method` argument determines the design method used. Options
#'   are:
#'
#' - `"random"`: Creates a design by randomly sampling from profiles
#' - `"sequential"`: Creates a D-efficient design using sequential improvement. Bayesian D-efficient designs can be obtained by specifying a prior model with the `priors` argument that include a covariance matrix.
#'
#'   All methods ensure that the two following criteria are met:
#'
#'   1. No two profiles are the same within any one choice set.
#'   2. No two choice sets are the same within any one respondent.
#'
#'   The table below summarizes method compatibility with other design options:
#'
#'   Method        | No choice? | Labeled designs? | Restricted profiles? | Blocking?
#'   --------------|------------|------------------|----------------------|----------
#'   `"random"`    | Yes        | Yes              | Yes                  | No
#'   `"sequential"` | No         | Yes              | Yes                  | Yes
#'
#' The `"sequential"` method creates a design by sequentially improving D-efficiency:
#' 1. Start with a random design
#' 2. Compute initial D-error
#' 3. For each question, alternative, and attribute:
#'    - Try all possible level changes to the attribute.
#'    - Keep changes that improve the D-error.
#' 4. Repeat until no further improvements or max iterations are reached
#'
#' @return The returned `design` data frame contains a choice-based conjoint
#' survey design where each row is an alternative. It includes the following
#' columns:
#'
#' - `profileID`: Identifies the profile in `profiles`.
#' - `blockID`: If blocking is used, identifies each unique block.
#' - `respID`: Identifies each survey respondent.
#' - `qID`: Identifies the choice question answered by the respondent.
#' - `altID`: Identifies the alternative in any one choice observation.
#' - `obsID`: Identifies each unique choice observation across all respondents.
#'
#' @export
#' @examples
#' library(cbcTools)
#'
#' # Generate all possible profiles
#' profiles <- cbc_profiles(
#'   price     = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
#'   type      = c("Fuji", "Gala", "Honeycrisp"),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Make a random survey design (default)
#' design_random <- cbc_design(
#'   profiles = profiles,
#'   n_alts   = 3,   # Number of alternatives per question
#'   n_q      = 6    # Number of questions per respondent
#' )
#'
#' # Create priors
#' priors <- cbc_priors(
#'   price = -0.5,
#'   type = c(0.2, 0.3),
#'   freshness = c(0.4, 0.8)
#' )
#'
#' # Make a D-efficient design with priors using the "sequential" method
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_alts = 3,
#'   n_q = 6,
#'   method = "sequential",
#'   priors = priors
#' )
cbc_design <- function(
  profiles,
  n_alts,
  n_q,
  n_blocks = 1,
  n_start = 5,
  no_choice = FALSE,
  label = NULL,
  method = "random",
  randomize_questions = TRUE,
  randomize_alts = TRUE,
  priors = NULL,
  prior_no_choice = NULL,
  max_iter = 50,
  parallel = FALSE
) {
    profiles_restricted <- nrow(expand.grid(get_profile_list(profiles))) > nrow(profiles)

    check_inputs_design(
        profiles,
        n_alts,
        n_q,
        n_blocks,
        n_start,
        no_choice,
        label,
        method,
        randomize_questions,
        randomize_alts,
        priors,
        prior_no_choice,
        max_iter,
        parallel
    )

    profiles <- as.data.frame(profiles)

    # Overrides ----

    # Override n_start for "random" method
    if ((method == "random") & (n_start > 1)) {
        n_start <- 1
    }

    # Override randomize_alts for labeled designs
    if (!is.null(label) & randomize_alts & method != 'random') {
      message(
        "Alternative order randomization is disabled for labeled designs.\n",
        "Setting randomize_alts <- FALSE\n"
      )
      randomize_alts <- FALSE
    }

    # Override n_blocks and priors for "random" method
    if (method == "random") {
        if (!is.null(priors)) {
            message(
                'priors are ignored for designs using the "random" method.\n',
                "Setting prior <- NULL\n"
            )
            priors <- NULL
        }
    }

    if (method == 'sequential') {
      result <- make_design_sequential(
        profiles, n_blocks, n_alts, n_q, n_blocks,
        priors, max_iter, label, n_start
      )
      design <- result$design
      d_error <- result$d_error
    } else {
      design <- make_random_survey(
        profiles, n_blocks, n_resp = n_blocks, n_alts, n_q, label
      )
      d_error <- cbc_d_error(design)
    }

    if (no_choice) {
      design <- add_no_choice(design, n_alts)
    }

    # Create return object
    result <- list(
      design = design,
      method = method,
      n_q = n_q,
      n_alts = n_alts,
      n_blocks = n_blocks,
      no_choice = no_choice,
      label = label,
      d_error = d_error,
      priors = priors,
      profiles = profiles
    )

    # Set class and return
    class(result) <- c("cbc_design", "list")
    return(result)
}

# General helpers ----

get_profile_list <- function(profiles) {
  profile_lvls <- profiles[, 2:ncol(profiles)]
  varnames <- names(profile_lvls)
  type_ids <- get_type_ids(profiles)
  profile_list <- list()
  for (i in seq_len(ncol(profile_lvls))) {
    if (type_ids$discrete[i]) {
      profile_list[[i]] <- levels(profile_lvls[,i])
    } else {
      profile_list[[i]] <- unique(profile_lvls[,i])
    }
  }
  names(profile_list) <- varnames
  return(profile_list)
}

get_type_ids <- function(profiles) {
  types <- get_col_types(profiles[, 2:ncol(profiles)])
  ids <- list()
  ids$discrete <- types %in% c("factor", "character")
  ids$continuous <- !ids$discrete
  return(ids)
}

join_profiles <- function(design, profiles) {

  # Preserve row order

  design$row_order <- seq(nrow(design))

  # Before joining profiles, ensure that all the data types are the same
  # as in profiles, otherwise join won't work properly

  type_ids <- get_type_ids(profiles)

  # Convert numeric columns to actual numbers
  for (id in which(type_ids$continuous)) {
    design[,id] <- as.numeric(as.character(design[,id]))
  }

  # Convert character types to factors and set same levels as profiles
  for (id in which(type_ids$discrete)) {
    design[,id] <- factor(design[,id], levels = levels(profiles[,id+1]))
  }

  # Join on profileIDs, then reorder to retain design order
  varnames <- names(profiles[, 2:ncol(profiles)])
  design <- merge(design, profiles, by = varnames, all.x = TRUE, sort = FALSE)
  design <- design[order(design$row_order),]
  design$row_order <- NULL
  if ('blockID' %in% names(design)) { varnames <- c(varnames, 'blockID') }
  design <- design[c('profileID', varnames)]
  return(design)
}

add_metadata <- function(design, n_resp, n_alts, n_q) {
  design$respID <- rep(seq(n_resp), each = n_alts * n_q)
  design$qID    <- rep(seq(n_q), each = n_alts)
  design$altID  <- rep(seq(n_alts), n_resp * n_q)
  design$obsID  <- rep(seq(n_resp * n_q), each = n_alts)
  return(design)
}

get_dup_obs <- function(design, n_alts) {
  # Identify duplicate profiles for each observation (each choice set)
  counts <- tapply(
    design$profileID, design$obsID,
    FUN = function(x) length(unique(x))
  )
  dup_ids <- which(counts != n_alts)
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
}

get_dup_resp <- function(design, n_resp, n_q) {
  # Identify duplicate choice sets for each respondent
  dup_ids <- unlist(lapply(
    1:n_resp,
    function(x) dup_obs_by_resp(design[which(design$respID == x),])
  ))
  dup_rows <- which(design$obsID %in% dup_ids)
  return(dup_rows)
}

dup_obs_by_resp <- function(df) {
  profiles_list <- tapply(
    df$profileID, df$obsID,
    FUN = function(x) sort(x)
  )
  # Convert vector list to a data frame to check for duplicates
  dupe_df <- do.call(rbind, profiles_list)
  dup_ids <- which(duplicated(dupe_df))
  if (length(dup_ids) > 0) {
    return(as.numeric(names(dup_ids)))
  }
  return(NULL)
}

add_no_choice <- function(design, n_alts) {
  # Must dummy code categorical variables to include an outside good
  design <- dummy_code(design)
  # Create outside good rows
  design_og <- design[which(design$altID == 1), ]
  design_og[,!names(design_og) %in% c("respID", "qID", "altID", "obsID")] <- 0
  design_og$altID <- n_alts + 1
  design_og$no_choice <- 1
  # Insert outside good rows into design
  design$no_choice <- 0
  design <- rbind(design, design_og)
  design <- design[order(design$obsID), ]
  return(design)
}

dummy_code <- function(design) {
  types <- get_col_types(design)
  nonnumeric <- names(types[!types %in% c("integer", "numeric")])
  if (length(nonnumeric) > 0) {
    design <- fastDummies::dummy_cols(design, nonnumeric)
    design[, nonnumeric] <- NULL
  }
  return(design)
}

get_col_types <- function(data) {
  types <- lapply(data, class)
  test <- function(x) { x[1] }
  return(unlist(lapply(types, test)))
}

reorder_cols <- function(design) {
  design <- as.data.frame(design)[, c(get_id_names(), get_var_names(design))]
  return(design)
}

# Choice sets ----

make_random_sets <- function(profiles, n_alts) {
  n_q <- nrow(profiles)
  design <- sample_random_sets(profiles, n_alts, n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  dup_rows_obs <- get_dup_obs(design, n_alts)
  dup_rows_resp <- get_dup_resp(design, n_resp = 1, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    design <- sample_random_sets(profiles, n_alts, n_q)
    dup_rows_obs <- get_dup_obs(design, n_alts)
    dup_rows_resp <- get_dup_resp(design, n_resp = 1, n_q)
  }
  return(design)
}

make_random_sets_by_block <- function(profiles, n_alts, n_blocks) {
  # Make choice sets for each set of profileIDs
  profiles <- split(profiles, profiles$blockID)
  choice_sets <- list()
  for (i in 1:n_blocks) {
    choice_sets[[i]] <- make_random_sets(profiles[[i]], n_alts)
  }
  choice_sets <- do.call(rbind, choice_sets)
  return(choice_sets)
}

sample_random_sets <- function(profiles, n_alts, n_q) {
  # Make a randomized copy of the profiles for each alternative
  sets <- lapply(seq(n_alts), function(x) profiles[order(stats::runif(n_q)),])
  sets <- lapply(sets, function(x) {
    x$order <- seq(nrow(x))
    return(x)
  })
  sets <- do.call(rbind, sets)
  sets <- sets[order(sets$order),]
  sets <- add_metadata(sets, n_resp = 1, n_alts, n_q)
  sets$order <- NULL
  return(sets)
}

# Random Design ----

# Sample from profiles with replacement to create randomized choice sets

make_random_survey <- function(
    profiles, n_blocks, n_resp, n_alts, n_q, label
) {
  if (is.null(label)) {
    survey <- survey_rand_sample(profiles, n_resp, n_alts, n_q)
  } else {
    survey <- survey_rand_sample_label(profiles, n_resp, n_alts, n_q, label)
  }
  survey <- set_block_ids(survey, n_blocks)
  survey <- reorder_cols(survey)
  row.names(survey) <- NULL
  return(survey)
}

survey_rand_sample <- function(profiles, n_resp, n_alts, n_q) {
  survey <- sample_profiles(profiles, size = n_resp * n_alts * n_q)
  survey <- add_metadata(survey, n_resp, n_alts, n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  dup_rows_obs <- get_dup_obs(survey, n_alts)
  dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    # cat('Number dupe rows by obs:', length(dup_rows_obs), '\n')
    # cat('Number dupe rows by resp:', length(dup_rows_resp), '\n')
    dup_rows <- unique(c(dup_rows_obs, dup_rows_resp))
    new_rows <- sample_profiles(profiles, size = length(dup_rows))
    survey[dup_rows, 1:ncol(new_rows)] <- new_rows
    # Recalculate duplicates
    dup_rows_obs <- get_dup_obs(survey, n_alts)
    dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  }
  return(survey)
}

survey_rand_sample_label <- function(profiles, n_resp, n_alts, n_q, label) {
  n_alts <- override_label_alts(profiles, label, n_alts)
  # Randomize rows by label
  labels <- split(profiles, profiles[label])
  survey <- sample_profiles_by_group(labels, size = n_resp * n_q)
  # Replace rows with duplicated profiles in each obsID or
  # duplicated choice sets in each respID
  survey <- add_metadata(survey, n_resp, n_alts, n_q)
  dup_rows_obs <- get_dup_obs(survey, n_alts)
  dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  while ((length(dup_rows_obs) > 0) | (length(dup_rows_resp) > 0)) {
    # cat('Number dupe rows by obs:', length(dup_rows_obs), '\n')
    # cat('Number dupe rows by resp:', length(dup_rows_resp), '\n')
    dup_rows <- unique(c(dup_rows_obs, dup_rows_resp))
    new_rows <- sample_profiles_by_group(labels, size = length(dup_rows) / n_alts)
    survey[dup_rows, 1:ncol(new_rows)] <- new_rows
    # Recalculate duplicates
    dup_rows_obs <- get_dup_obs(survey, n_alts)
    dup_rows_resp <- get_dup_resp(survey, n_resp, n_q)
  }
  return(survey)
}

sample_profiles <- function(profiles, size) {
  return(profiles[sample(
    x = seq_len(nrow(profiles)), size = size, replace = TRUE), ]
  )
}

sample_profiles_by_group <- function(labels, size) {
  design <- lapply(labels, function(x) sample_profiles(x, size = size))
  design <- lapply(design, function(x) add_label_id(x))
  design <- do.call(rbind, design)
  design <- design[order(design$labelID), ]
  design$labelID <- NULL
  return(design)
}

add_label_id <- function(design) {
  design$labelID <- seq(nrow(design))
  return(design)
}

override_label_alts <- function(profiles, label, n_alts) {
  n_levels <- length(unique(profiles[, label]))
  if (n_levels != n_alts) {
    warning(
      "The supplied 'n_alts' argument is being ignored and set to ", n_levels,
      " to match the number of unique levels in the ", label,
      " variable.\n"
    )
    # Over-ride user-provided n_alts as it is determined by the label
    n_alts <- n_levels
  }
  return(n_alts)
}

# Sequential D-Efficient Design ----

make_design_sequential <- function(
    profiles,
    n_resp,
    n_alts,
    n_q,
    n_blocks,
    priors,
    max_iter,
    label,
    n_start = 1
) {
    # Set up parallel processing
    n_cores <- set_num_cores(NULL)
    message("Running ", n_start, " design searches using ", n_cores, " cores...")

    # Create list of different random starting designs
    start_designs <- lapply(1:n_start, function(i) {
        design <- make_random_survey(
          profiles, n_blocks, n_resp = n_blocks, n_alts, n_q, label
        )
        design$qID <- design$obsID
        return(design)
    })

    n_questions <- n_q * n_blocks
    varNames <- get_var_names(start_designs[[1]])

    # Run optimization in parallel
    # Different parallel operation for Windows vs Mac
    if (Sys.info()[['sysname']] == 'Windows') {
        cl <- parallel::makeCluster(n_cores, "PSOCK")
        # Export necessary functions to cluster
        parallel::clusterExport(cl, c(
            "optimize_design", "compute_info_matrix", "get_X_matrix",
            "logit", "logit_draws", "get_eligible_profiles"
        ), envir = environment())

        results <- suppressMessages(suppressWarnings(
            parallel::parLapply(
                cl = cl,
                seq_along(start_designs),
                function(i) {
                  result <- optimize_design(
                    start_designs[[i]], profiles, priors, varNames,
                    n_questions, n_alts, max_iter, n_blocks, label
                  )
                  result$start_number <- i
                  return(result)
                }
            )
        ))
        parallel::stopCluster(cl)
    } else {
        results <- suppressMessages(suppressWarnings(
            parallel::mclapply(
              seq_along(start_designs),
              function(i) {
                result <- optimize_design(
                  start_designs[[i]], profiles, priors, varNames,
                  n_questions, n_alts, max_iter, n_blocks, label
                )
                result$start_number <- i
                return(result)
              },
              mc.cores = n_cores
            )
        ))
    }

    # Find best design based on D-error
    d_errors <- sapply(results, function(x) x$d_error)
    best_index <- which.min(d_errors)
    best_result <- results[[best_index]]

    # Merge designs
    design <- best_result$design
    d_error <- best_result$d_error
    design <- set_block_ids(design, n_blocks)

    # Print summary of all starts
    message("\nD-error results from all starts:")
    sorted_results <- sort(d_errors)
    for (i in seq_along(sorted_results)) {
        idx <- which(d_errors == sorted_results[i])[1]
        message(sprintf(
            "Start %d: %.6f %s",
            results[[idx]]$start_number,
            sorted_results[i],
            if(idx == best_index) "  (Best)" else ""
        ))
    }
    return(results[[best_index]])
}

#' Display attribute levels and dummy coding for a CBC design
#'
#' Shows how categorical variables will be dummy coded and what each coefficient
#' represents in the utility function.
#'
#' @param design A data frame containing a choice experiment design created
#'   by `cbc_design()`
#' @param exclude Optional character vector of attribute names to exclude
#' @return Invisibly returns a list containing the coding information, but primarily
#'   prints formatted information to the console
#' @export
#' @examples
#' # Create profiles
#' profiles <- cbc_profiles(
#'   price     = seq(1, 5, 0.5),
#'   type      = c('Fuji', 'Gala', 'Honeycrisp'),
#'   freshness = c('Poor', 'Average', 'Excellent')
#' )
#'
#' # Generate design
#' design <- cbc_design(
#'   profiles = profiles,
#'   n_resp = 100,
#'   n_alts = 3,
#'   n_q = 6
#' )
#'
#' # View attribute levels and coding
#' cbc_levels(design)
cbc_levels <- function(design, exclude = NULL) {
    # Get attribute columns (excluding metadata)
    attr_cols <- get_var_names(design)

    if (!is.null(exclude)) {
        attr_cols <- setdiff(attr_cols, exclude)
    }

    # Process each attribute
    attr_info <- list()

    cat("CBC Design Attribute Information:\n")
    cat("===============================\n\n")

    for (attr in attr_cols) {
        values <- design[[attr]]
        if (is.numeric(values)) {
            # Continuous variable
            cat(sprintf("%-12s: Continuous variable\n", attr))
            cat(sprintf("              Range: %.2f to %.2f\n",
                        min(values), max(values)))
            cat("              Coefficient represents effect of one-unit change\n\n")

            attr_info[[attr]] <- list(
                type = "continuous",
                range = range(values)
            )

        } else {
            # Categorical variable
            levels <- unique(sort(as.character(values)))
            n_levels <- length(levels)
            base_level <- levels[1]
            coded_levels <- levels[-1]

            cat(sprintf("%-12s: Categorical variable (%d levels)\n", attr, n_levels))
            cat("              Base level:", base_level, "\n")
            for (i in seq_along(coded_levels)) {
                cat(sprintf("              β%-2d: %s\n",
                            i, coded_levels[i]))
            }
            cat("\n")

            attr_info[[attr]] <- list(
                type = "categorical",
                base_level = base_level,
                coded_levels = coded_levels
            )
        }
    }

    # Example prior specification
    cat("Example prior specification:\n")
    cat("------------------------\n")
    cat("priors <- cbc_priors(\n")

    for (attr in attr_cols) {
        if (attr_info[[attr]]$type == "continuous") {
            cat(sprintf("    %-12s = 0,  # Effect of one-unit change\n", attr))
        } else {
            coefs <- rep("0", length(attr_info[[attr]]$coded_levels))
            cat(sprintf("    %-12s = c(%s),  # vs %s\n",
                        attr,
                        paste(coefs, collapse = ", "),
                        attr_info[[attr]]$base_level))
        }
    }

    cat("    # Add sd = list(...) for random parameters\n")
    cat(")\n")

    invisible(attr_info)
}

set_block_ids <- function(design, n_blocks) {
  if (n_blocks > 1) {
    design$blockID <- rep(seq(n_blocks), each = nrow(design) / n_blocks)
  } else {
    design$blockID <- 1
  }
  return(design)
}
