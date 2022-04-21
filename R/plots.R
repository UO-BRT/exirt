#' Estimate the probability of a correct response
#'
#' This function works because we know we have a Rasch model, otherwise the
#' function would be just a bit more complicated. Provides the probability of a
#' correct response for a given item difficulty, \code{b}, and a given ability
#' estimate, \code{theta}. Note that \code{theta} is typically passed as a
#' vector of abilities and so a vector of probabilities is returned.
#'
#' @param b The item difficulty estimate
#' @param theta The person ability estimate. This is the \code{theta} estimate
#'   from [get_person_estimates()] (not the RIT score).
#' @keywords internal
prob <- function(b, theta) {
  1 / (1 + exp(-(theta - b)))
}

#' Item information function
#'
#' Returns the individual information for a given item along the vector of
#' theta values supplied. These are summed to provide the test infromation
#' function.
#'
#' @param b The item difficulty estimate
#' @param theta The person ability estimate. This is the \code{theta} estimate
#'   from [get_person_estimates()] (not the RIT score).
#' @keywords internal
iif <- function(b, theta) {
  p <- vapply(b, prob, theta, FUN.VALUE = double(length(theta)))
  q <- 1 - p
  p * q
}

#' Test information function
#'
#' Sums the item information for each \code{theta} value supplied to provide
#' the overall information provided by the test for the given \code{theta}
#' values supplied. Internally, the \code{theta} values are converted to RIT
#' scores to make the output more directly interpretable.
#'
#' @param id The data frame returned from [get_item_diffs()].
#' @param name The name of the given test, e.g., \code{"ELA_G8"},
#'   \code{"Math_G11"}.
#' @param theta The person ability estimate. This is the \code{theta} estimate
#'   from [get_person_estimates()] (not the RIT score). Defaults to a sequence
#'   from -6 to 6 in increments of 0.01.
#' @keywords Internal
tif_ <- function(id, name, theta = seq(-6, 6, 0.01)) {
  rit <- convert_theta(
    data.frame(theta = theta, theta_se = 1),
    name,
    round = FALSE
  )
  tif <- rowSums(iif(id$difficulty, theta))

  cut <- pull_cuts(name)
  cut <- cut[, grepl("^c\\d", names(cut))]
  attributes(cut) <- NULL

  out <- data.frame(
    rit = rit$rit,
    tif = tif
  )
  attributes(out) <- c(
    attributes(out),
    list(
      name = name,
      cuts = data.frame(cuts = factor(unlist(cut)))
    )
  )
  out
}

#' Test information function
#'
#' Sums the item information for each \code{theta} value supplied to provide
#' the overall information provided by the test for the given \code{theta}
#' values supplied. Internally, the \code{theta} values are converted to RIT
#' scores to make the output more directly interpretable.
#'
#' @param item_diff_table The data frame returned from [get_item_diffs()]
#'   with \code{single_df = FALSE}.
#' @param theta The person ability estimate. This is the \code{theta} estimate
#'   from [get_person_estimates()] (not the RIT score). Defaults to a sequence
#'   from -6 to 6 in increments of 0.01.
#' @export
tif <- function(item_diff_table, theta = seq(-6, 6, 0.01)) {
  if (is.data.frame(item_diff_table)) {
    return(tif_(item_diff_table, infer_test(item_diff_table$item), theta))
  }
  Map(
    function(diffs, nms) {
      tif_(diffs, nms, theta)
      },
      diffs = item_diff_table,
      nms = names(item_diff_table)
  )
}

#' Plot the test information function
#'
#' @param item_diff_table The data frame returned from [get_item_diffs()].
#' @param theta The person ability estimate. This is the \code{theta} estimate
#'   from [get_person_estimates()] (not the RIT score). Defaults to a sequence
#'   from -6 to 6 in increments of 0.01.
#' @export
tif_plot <- function(item_diff_table, theta = seq(-6, 6, 0.01)) {
  tif_df <- tif(item_diff_table, theta = theta)
  if (is.data.frame(tif_df)) {
    return(tif_plot_(tif_df))
  }
  lapply(tif_df, tif_plot_)
}

#' Internal function that uses \code{tif()} output
#'
#' @param tif_df The test information function data frame. Output from [tif()]
#' @keywords internal
#'
tif_plot_ <- function(tif_df) {
  shade_frame <- tif_df[tif_df$tif >= 5, ]
  cut_frame <- attr(tif_df, "cuts")
  content <- gsub("^(.+)_G.+", "\\1", attr(tif_df, "name"))
  grade <- gsub(".+G(.+)$", "\\1", attr(tif_df, "name"))

  ggplot(tif_df, aes(.data$rit, .data$tif)) +
    geom_line() +
    geom_ribbon(aes(ymin = -Inf, ymax = tif), shade_frame) +
    geom_vline(
      aes(xintercept = as.numeric(as.character(cuts)), color = cuts),
      cut_frame
    ) +
    guides(color = FALSE) +
    labs(
      title = paste0(content, ": Grade ", grade),
      x = "RIT",
      y = "Information"
    )
}

#' Estimate the expected total score for any values of theta
#'
#' Internally, the theta scores are transformed to RIT scores so the resulting
#' output is more directly interpretable.
#'
#' @inherit tif
#' @export
tcc <- function(item_diff_table, theta = seq(-6, 6, 0.01)) {
  if (is.data.frame(item_diff_table)) {
    nm <- infer_test(item_diff_table$item)
    out <- tcc_(item_diff_table, nm, theta)
    out$test <- nm
  } else {
    out <- Map(
      function(diffs, nms) {
        tcc_(diffs, nms, theta)
      },
      diffs = item_diff_table,
      nms = names(item_diff_table)
    )
    out <- bind_dfs(out)
  }
  out$content <- gsub("^(.+)_G.+$", "\\1", out$test)
  out$grade <- gsub("^.+_G(.+)$", "\\1", out$test)
  out[c("content", "grade", "rit", "expected_total")]
}

#' @keywords internal
tcc_ <- function(item_diff_table, name, theta) {
  rit <- convert_theta(
    data.frame(theta = theta, theta_se = 1),
    name,
    round = FALSE
  )
  p <- vapply(item_diff_table$difficulty, prob, theta, FUN.VALUE = double(length(theta)))

  cut <- pull_cuts(name)
  cut <- cut[, grepl("^c\\d", names(cut))]
  attributes(cut) <- NULL

  out <- data.frame(
    rit = rit$rit,
    expected_total = rowSums(p)
  )
  attributes(out) <- c(
    attributes(out),
    list(
      name = name,
      cuts = data.frame(cuts = factor(unlist(cut)))
    )
  )
  out
}

#' Produce Test Characteristic Curve Plots
#'
#' @inheritParams tif_plot
#' @param content Subset the plot to a specific content area. Defaults to
#'   "ELA|Math", which will select both ELA and Math.
#' @param grades The grade levels to include in the plot. Defaults to
#'   \code{[3-8]}, in which case grades 3 through 8 will be selected. Any grade
#'   range can be supplied, with the range passed in brackets. A single grade
#'   can also be supplied, which does not require brackets, (e.g., \code{5}
#'   would select only grade 5)
#' @export
tcc_plot <- function(
    item_diff_table,
    theta = seq(-6, 6, 0.01),
    content = c("ELA|Math", "ELA", "Math", "Science", "ELA|Science",
                "Math|Science", "all"),
    grades = "[3-8]"
  ) {
  content <- match.arg(content)
  content <- ifelse(content == "all", ".+", content)

  tcc_df <- tcc(item_diff_table, theta)
  selection <- grepl(content, tcc_df$content) & grepl(grades, tcc_df$grade)
  tcc_df <- tcc_df[selection, ]

  multi_color <- length(
    unique(paste0(tcc_df$content, tcc_df$grade))
  ) > 1

  p <- ggplot(tcc_df, aes(.data$rit, .data$expected_total)) +
         labs(
           title = "Test Characteristic Curves",
           x = "RIT",
           y = "Expected Score"
         )
  if (multi_color) {
    p <- p + geom_line(aes(color = .data$grade))
  } else {
    p <- p + geom_line()
  }

  n_content <- length(unique(tcc_df$content))
  n_grades <- length(unique(tcc_df$grade))
  if (n_content == 1 & n_grades == 1) {
    subttl <- paste0(unique(tcc_df$content), ", Grade ", unique(tcc_df$grade))
  } else if (n_content == 1 & n_grades > 1) {
    subttl <- unique(tcc_df$content)
  }

  if (n_content > 1) {
    p +
      facet_wrap("content")
  } else {
    p +
      labs(subtitle = subttl)
  }
}
