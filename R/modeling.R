#' Function estimate Rasch models
#'
#' Uses [TAM::tam.mml()] under the hood, because the theta estimates from this
#' package most closely matched those from Winsteps (specifically for extreme)
#' cases. Can estimate models from either a list tests or a single test.
#' Takes care of the item prep (i.e., recoding \code{NA} values to 0) and,
#' optionally, item anchoring.
#'
#' @param test Either a list of test score data from which to estimate models
#'   or a single data frame with the item-level data. Note, this can be all
#'   tests. It does not have to be a list of only one content area, for
#'   example.
#' @param omit_field_test Should field test items (those that are not anchored)
#'   be omitted in the estimation? Defaults to \code{TRUE}. Note this should
#'   always be \code{TRUE} when person estimates are extracted from the model.
#' @param ... Additional arguments passed to [TAM::tam.mml()]
#' @export
#' @return A list of models
#' @examples
#' \dontrun{
#'   library(exirt)
#'   all_tests <- dbprocess::get_items(db = "2021")
#'   models <- rasch(all_tests)
#' }
rasch <- function(test, omit_field_test = TRUE, ...) {
  itms <- orextdb::db_get("Items", db = attr(test, "db"))
  if (is.data.frame(test)) {
    return(rasch_(test, itms, omit_field_test, anchored, ...))
  }
  lapply(test, rasch_, itms, omit_field_test, anchored, ...)
}

#' Internal function to estimate a Rasch model
#'
#' @param test One specific test from \code{dbprocess::get_items()}
#' @param items The items table from the database, e.g.,
#'   \code{orextdb::db_get("Items")}
#' @param ... Additional arguments passed to [TAM::tam.mml()]
#' @keywords internal
#' @noRd
rasch_ <- function(test, items, omit_field_test = TRUE, ...) {
  item_data <- prep_items(test)
  anchors <- create_anchors(test, items)

  # find field test items
  ft <- setdiff(seq_len(ncol(item_data)), anchors[, "item_location"])
  ft_ids <- names(item_data[, ft, drop = FALSE])
  ft_frame <- data.frame(item_loc = ft, item_id = ft_ids)

  if (isTRUE(omit_field_test)) {
    anchor_locs <- anchors[, "item_location", drop = TRUE]
    item_data <- item_data[, anchor_locs]
    anchors <- cbind(seq_len(ncol(item_data)), anchors[, 2, drop = TRUE])
  }

  out <- tam.mml(
    item_data,
    xsi.fixed = anchors,
    verbose = FALSE
  )

  list(model = out, field_test_items = ft_frame)
}
