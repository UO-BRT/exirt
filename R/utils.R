#' Function for selecting and prepping item data
#' @param test One specific test from \code{dbprocess::get_items()}
#' @return A data frame for the specific test with only items and all missing
#'   values recoded to zero.
#' @keywords internal
#' @noRd
#' @examples
#' \dontrun{
#' g3mth <- dbprocess::get_items(3, "Math")
#' items <- orextdb::db_get("Items")
#' prepped <- prep_items(g3mth, items)
#'
#' # check that there's no missing data
#' any(apply(prepped, 2, function(x) any(is.na(x))))
#' }
prep_items <- function(test) {
  test <- test[ ,is_item(test)]
  as.data.frame(
    apply(test, 2, function(x) ifelse(is.na(x), 0, x))
  )
}

#' Function for creating an anchor file for analysis
#'
#' This function is used internally, but is exported for spot checking. It is
#' called by [rasch()] when \code{anchored = TRUE} to create the anchor file
#' during analysis.
#'
#' @param test One specific test from \code{dbprocess::get_items()}
#' @param itemfile The items table from the database, e.g.,
#'   \code{orextdb::db_get("Items")}
#' @keywords internal
#' @noRd
create_anchors <- function(test, itemfile) {
  test <- prep_items(test)
  item_sel <- itemfile$item_id_brt %in% names(test)
  anchors <- itemfile[item_sel, ]
  locs <- seq_len(ncol(test))[names(test) %in% anchors$item_id_brt]
  m <- cbind(
    item_location = as.numeric(locs),
    item_difficulty = as.numeric(anchors$item_difficulty)
  )
  m[!is.na(m[ ,2]), ]
}

#' Bind a list of data frames together
#'
#' @param l A list of data frames
#' @keywords internal
#' @noRd
bind_dfs <- function(l) {
  nms <- names(l)

  out <- Map(function(x, nm) {
    x$test <- nm
    x
    },
    x = l,
    nm = nms
  )

  out <- do.call("rbind", out)
  rownames(out) <- NULL
  out <- out[, c(ncol(out), 1:(ncol(out) - 1))]

  out
}

is_item <- function(d) {
  grepl("^[EMS]\\d", names(d))
}

#' Implement standard rounding, rather than R's rounding
#' @param x A number (type == double)
#' @param n The number of digits to round to
#' @keywords internal
#' @noRd
round2 <- function(x, n = 0) {
  directionality <- sign(x)
  out <- abs(x) * 10^n + 0.5
  out <- trunc(out) / 10^n
  out * directionality
}

#' Paste a list together
#'
#' Adds commas after each element and a "and" between the last and second to
#' last elements.
#'
#' @param x A character vector
#' @examples {
#' paste_collapse(c("red", "green", "blue", "orange"))
#' #> [1] "red, green, blue, and orange"
#' }
#' @keywords internal
#' @noRd
paste_collapse <- function(x) {
  x[length(x)] <- paste("and", x[length(x)])  
  x[-length(x)] <- paste0(x[-length(x)], ",")
  paste(x, collapse = " ")
}

infer_test <- function(item_ids) {
  content <- unique(substr(item_ids, 1, 1))
  if(length(content) > 1) {
    stop("More than one content area is represented in the given items")
  }
  content <- switch(content,
    "^E" = "ELA",
    "M" = "Math",
    "S" = "Science"
  )
  grade <- unique(as.numeric(gsub("^.{1}(\\d\\d?).+$", "\\1", item_ids)))
  if (length(grade) > 1) {
    stop("More than one grade level is represented in the given items")
  }
  paste0(content, "_G", grade)
}