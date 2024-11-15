#' Create a survey from a conjoint design
#'
#' This function takes a conjoint design and replicates it across a specified
#' number of respondents to create a complete survey design.
#'
#' @param design A cbc_design object created by `cbc_design()`. If the
#'   `"random"` method was used to create the design, then the design will
#'   be ignored and a fully randomized survey will be generated where each
#'   respondent sees a unique, random design.
#' @param n_resp Number of survey respondents
#' @param randomize_questions Logical. Whether to randomize question order for each respondent
#' @param randomize_alts Logical. Whether to randomize alternative order within questions
#' @return A data frame with class cbc_survey containing the complete survey design
#' @export
cbc_survey <- function(
    design,
    n_resp,
    randomize_questions = TRUE,
    randomize_alts = TRUE
) {
  if (!inherits(design, "cbc_design")) {
    stop("design must be a cbc_design object created by cbc_design()")
  }

  # Extract the design components
  design_df <- design$design
  n_blocks <- design$n_blocks
  n_q <- design$n_q
  n_alts <- design$n_alts
  no_choice <- design$no_choice
  label <- design$label
  method <- design$method
  profiles <- design$random

  if (method == 'random') {
    survey <- make_random_survey(
      profiles, n_blocks, n_resp, n_alts, n_q, no_choice, label
    )
    if (no_choice) { survey <- add_no_choice(survey, n_alts) }
  } else {
    # Create the survey using repeat_sets of the design
    survey <- repeat_sets(
      design_df, n_resp, n_alts, n_q, n_blocks,
      randomize_questions, randomize_alts
    )
  }

  # Add class and return
  class(survey) <- c("cbc_survey", "data.frame")
  return(survey)
}

repeat_sets <- function(
    choice_sets,
    n_resp,
    n_alts,
    n_q,
    n_blocks,
    randomize_questions,
    randomize_alts
) {
  # Repeat choice sets to match number of respondents
  if (n_blocks > 1) {
    choice_sets <- split(choice_sets, choice_sets$blockID)
    n_resp_block <- ceiling(n_resp / n_blocks)
    n_reps <- ceiling(n_resp_block / (nrow(choice_sets[[1]]) / n_alts / n_q))
    design <- list()
    for (i in seq_len(n_blocks)) {
      set <- choice_sets[[i]]
      temp <- set[rep(seq_len(nrow(set)), n_reps), ]
      design[[i]] <- temp[1:(n_resp_block*n_q*n_alts), ]
    }
    design <- do.call(rbind, design)
  } else {
    design <- choice_sets[rep(seq_len(nrow(choice_sets)), n_resp), ]
  }
  design <- design[1:(n_resp*n_q*n_alts), ]
  design <- add_metadata(design, n_resp, n_alts, n_q)

  # Randomize question and/or alternative order if requested
  if (randomize_questions | randomize_alts) {
    design <- randomize_design(
      design, n_resp, n_alts, n_q, n_blocks,
      randomize_questions, randomize_alts
    )
  }

  return(design)
}

randomize_design <- function(
    design, n_resp, n_alts, n_q, n_blocks,
    randomize_questions, randomize_alts
) {
  # Split design by respondent
  resp_designs <- split(design, design$respID)

  # For each respondent
  for (r in seq_along(resp_designs)) {
    resp_design <- resp_designs[[r]]

    # Randomize question order if requested
    if (randomize_questions) {
      new_q_order <- sample(1:n_q)

      # Create mapping from old to new question order
      q_map <- data.frame(
        old_qID = seq(n_q),
        new_qID = new_q_order
      )

      # Update question IDs
      resp_design$qID <- q_map$new_qID[match(resp_design$qID, q_map$old_qID)]
    }

    # Randomize alternative order if requested
    if (randomize_alts) {
      for (q in 1:n_q) {
        q_rows <- which(resp_design$qID == q)
        new_alt_order <- sample(1:n_alts)
        resp_design$altID[q_rows] <- new_alt_order
      }
    }

    resp_designs[[r]] <- resp_design
  }

  # Recombine designs and update obsID
  design <- do.call(rbind, resp_designs)
  design <- design[order(design$respID, design$qID, design$altID), ]
  design$obsID <- rep(seq(n_resp * n_q), each = n_alts)
  row.names(design) <- NULL

  return(design)
}


