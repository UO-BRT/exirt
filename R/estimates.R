#' High-level function to esimate/create student estimates
#'
#' This function does the modeling and pulls the student estimates
#' for every grade/content area (unless a specific grade/content area is
#' supplied). Student estimates are computed with all field-test
#' items removed.
#'
#' @inheritParams rasch
#' @inheritParams get_person_estimates
#' @export

estimate_abilities <- function(test, single_df = TRUE) {
  models <- rasch(test)
  get_person_estimates(models, test, single_df)
}

#' High-level function to estimate/create item difficulty estimates 
#' for field-test items
#'
#' This function does the modeling and pulls the item difficulty estimates
#' for every grade/content area (unless a specific grade/content area is
#' supplied). Operational items are anchored at their previously estimated
#' item difficulty values.
#'
#' @inheritParams rasch
#' @inheritParams get_item_diffs
#' @export

estimate_ft_difficulties <- function(test, single_df = TRUE) {
  models <- rasch(test, omit_field_test = FALSE)
  item_estimates <- get_item_diffs(models, single_df)
  ft_items <- get_ft_items(models)

  out <- merge(
    item_estimates,
    ft_items,
    by = c("test", "item_id"),
    all.y = TRUE
  )
  out <- out[order(out$test, out$item_loc), ]
  out <- out[, -grep("item_loc", names(out))]

  if (!single_df) {
    out <- split(out, out$test)
    out <- lapply(out, function(x) {
      rownames(x) <- NULL
      x
    })
  }
  out
}