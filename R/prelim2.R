#' Function for selecting and prepping item data
#' @param test One specific test from \code{dbprocess::get_items()}
#' @param itemfile The items table from the database, e.g.,
#'   \code{orextdb::db_get("Items")}
#' @return A data frame for the specific test with only items and all missing
#'   values recoded to zero.
#' @export
#' @examples
#' \dontrun{
#' g3mth <- dbprocess::get_items(3, "Math")
#' items <- orextdb::db_get("Items")
#' prepped <- prep_items(g3mth, items)
#'
#' # check that there's no missing data
#' any(apply(prepped, 2, function(x) any(is.na(x))))
#' }
prep_items <- function(test, itemfile) {
  out <- test[ ,names(test) %in% itemfile$item_id_brt]
  as.data.frame(
    apply(out, 2, function(x) ifelse(is.na(x), 0, x))
  )
}

#' Function for creating an anchor file for analysis
#'
#' This function is used internally, but is exported for spot checking. It is
#' called by [rasch()] when \code{anchored = TRUE} to create the anchor file
#' during analysis.
#'
#' @inheritParams prep_items
#' @export
create_anchors <- function(test, itemfile) {
  item_sel <- (itemfile$item_id_brt %in% names(test)) &
                !is.na(itemfile$difficulty)
  anchors <- itemfile[item_sel, ]
  cbind(seq_len(ncol(prep_items(test))), anchors$difficulty)
}

#' Function to estimate a Rasch model
#'
#' Uses [TAM::tam.mml()] under the hood, becuase the theta estimates from this
#' package most closely matched those from Winsteps (specifically for extreme)
#' cases.
#' @inheritParams prep_items
#' @param items The items table from the database, e.g.,
#'   \code{orextdb::db_get("Items")}
#' @param anchored Should the model be estimated with the operational items
#' anchored at their estimated values? Logical, defaults to \code{TRUE}.
#' @param ... Additional arguments passed to [TAM::tam.mml()]
#' @export
rasch <- function(test, items, anchored = TRUE, ...) {
  item_data <- prep_items(test)
  if (anchored) {
    out <- tam.mml(item_data, xsi.fixed = create_anchors(test, items), ...)
  } else {
    out <- tam.mml(item_data, ...)
  }
  out
}

#' Pull item difficulties from the model
#' @param model The fitted model. Output from [rasch()].
#' @return A data frame with the item ID, difficulty, and 95% CI.
#' @export
pull_item_diffs <- function(model) {
  difficulties <- model$xsi

  out <- data.frame(
    item = rownames(difficulties),
    difficulty = difficulties$xsi,
    lower = ifelse(difficulties$se.xsi == 0,
                   NA_real_,
                   difficulties$xsi + qnorm(0.975) * difficulties$se.xsi),
    upper = ifelse(difficulties$se.xsi == 0,
                   NA_real_,
                   difficulties$xsi + qnorm(0.025) * difficulties$se.xsi),
    stringsAsFactors = FALSE
  )

  rownames(out) <- NULL
  out
}

#' Estimate person abilities
#'
#' @param model The fitted model. Output from [rasch()].
#' @param full_demo_data The full test, including the demographic data, e.g.,
#'   one specific test from \code{dbprocess::get_items()}.
#' @return The \code{full_demo_data} data frame, with additional \code{theta}
#'   and \code{theta_se} columns.
estimate_theta <- function(model, full_demo_data) {
  fs <- IRT.factor.scores(model, type = "MLE")

  full_demo_data$theta <- fs[ ,1]
  full_demo_data$theta_se <- fs[ ,2]

  full_demo_data
}


