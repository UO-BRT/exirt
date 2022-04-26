#' Function for selecting and prepping item data
#' @param test One specific test from \code{dbprocess::get_items()}
#' @return A data frame for the specific test with only items and all missing
#'   values recoded to zero.
#' @keywords internal
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
  test <- test[, is_item(test)]
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

create_anchors <- function(test, itemfile) {
  test <- prep_items(test)
  item_sel <- itemfile$item_id_brt %in% names(test)
  anchors <- itemfile[item_sel, ]
  #anchors <- dplyr::distinct(anchors, item_id_brt, .keep_all = TRUE)

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
#' @keywords internal
paste_collapse <- function(x) {
  x[length(x)] <- paste("and", x[length(x)])
  x[-length(x)] <- paste0(x[-length(x)], ",")
  paste(x, collapse = " ")
}

#' Function to infer the test type based on the item ids.
#'
#' Will stop and give a message if there is more than 1 content area.
#' @param item_ids a vector of item ids from which to infer test type

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

#' Create a patterned data frame for all possible raw scores
#' @param item_names The column names (items) from which to generate the
#'   data frame. These become the column names of the patterned data frame
#' @keywords internal
create_pattern_frame <- function(item_names) {
  n <- length(item_names)

  full_zeros <- matrix(rep(0, n), nrow = 1)
  full_ones <- matrix(rep(1, n), nrow = 1)

  ones <- lapply(seq_len(n - 1), function(x) rep(1, x))
  zeros <- lapply(rev(seq_len(n - 1)), function(x) rep(0, x))

  m <- Map(function(a, b) matrix(c(a, b), nrow = 1), a = ones, b = zeros)
  m <- c(list(full_zeros), m, list(full_ones))

  d <- as.data.frame(Reduce(rbind, m))
  names(d) <- item_names
  d
}
