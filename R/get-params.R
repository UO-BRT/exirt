#' Get item difficulties from a single model or list of models
#'
#' @inheritParams get_person_estimates
#' @export
get_item_diffs <- function(model_ob, single_df = TRUE) {
  if (length(model_ob) == 2 & names(model_ob)[1] == "model") {
    return(pull_item_diffs(model_ob))
  }
  item_diffs <- lapply(model_ob, pull_item_diffs)
  if (single_df) {
    return(bind_dfs(item_diffs))
  }
  item_diffs
}

#' Get field test item IDs from a single model or list of models
#'
#' @param model_ob The fitted model or list of fitted models
#' @export
get_ft_items <- function(model_ob) {
  if (length(model_ob) == 2 & names(model_ob)[1] == "model") {
    return(model_ob[[2]])
  }
  ft <- lapply(model_ob, `[[`, "field_test_items")
  keep <- !vapply(ft, function(x) nrow(x) == 0, FUN.VALUE = logical(1))
  if (!any(keep)) {
    return(
      data.frame(
        item_loc = integer(),
        item_id = character()
      )
    )
  }
  bind_dfs(ft[keep])
}

#' Pull item difficulties from a single model
#' @param model The fitted model object. Output from [rasch()].
#' @return A data frame with the item ID, difficulty, and 95% CI.
#' @keywords internal
pull_item_diffs <- function(model) {
  difficulties <- model$model$xsi

  out <- data.frame(
    item_id = rownames(difficulties),
    difficulty = difficulties$xsi,
    se = difficulties$se.xsi,
    lower = ifelse(difficulties$se.xsi == 0,
      NA_real_,
      difficulties$xsi + qnorm(0.975) * difficulties$se.xsi
    ),
    upper = ifelse(difficulties$se.xsi == 0,
      NA_real_,
      difficulties$xsi + qnorm(0.025) * difficulties$se.xsi
    ),
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}

#' Get person abilities
#' @param model_ob The fitted model or list of fitted models
#' @param full_demo_data The full test, including the demographic data, e.g.,
#'   the output from \code{dbprocess::get_items()}. Note that if
#'   \code{model_ob} is a list, this argument should be a corresponding list
#'   with the same names. If \code{model_ob} is a single model, this argument
#'   should be the single corresponding data frame.
#' @param single_df Should a single data frame be returned? Defaults to
#'   \code{FALSE}, in which case a list of data frames is returned (assuming)
#'   \code{model_ob} is a list of models. Otherwise a single data frame is
#'   returned, which may be generally useful, but is less useful when passing
#'   the results to other functions in the package.
#' @return Person estimates with confidence standard errors, including
#'   conversions to RIT scores and the performance level
#' @export
get_person_estimates <- function(model_ob, full_demo_data, single_df = FALSE) {
  out <- Map(estimate_theta, model_ob, full_demo_data)
  out <- lapply(out, function(x) {
    x[, !is_item(x)]
  })
  out <- Map(convert_theta, out, names(out))
  if (single_df) {
    return(bind_dfs(out))
  }
  out
}

#' Estimate person abilities
#'
#' @param model The fitted model object. Output from [rasch()].
#' @param full_demo_data The full test, including the demographic data, e.g.,
#'   the output from \code{dbprocess::get_items()}. Note that if
#'   \code{model_ob} is a list, this argument should be a corresponding list
#'   with the same names. If \code{model_ob} is a single model, this argument
#'   should be the single corresponding data frame.
#' @return The \code{full_demo_data} data frame, with additional \code{theta}
#'   and \code{theta_se} columns.
#' @keywords internal
estimate_theta <- function(model, full_demo_data) {
  f <- file()
  sink(file = f)
  fs <- IRT.factor.scores(model$model, type = "MLE")
  sink()
  close(f)

  full_demo_data$theta <- as.numeric(fs[, 1, drop = TRUE])
  full_demo_data$theta_se <- as.numeric(fs[, 2, drop = TRUE])

  full_demo_data
}
