#' get scale scores for each possible raw score

raw_to_scale <- function() {
  pd <- get_pattern_data()
  models <- rasch(pd)

  d <- lapply(pd, function(x) {
    x$id <- rownames(x)
    x
  })

  thetas <- Map(estimate_theta, models, d)
  raw_theta <- lapply(thetas, function(x) {
    x$raw <- rowSums(x[, seq_len(ncol(x) - 3)])
    x[c("raw", "theta", "theta_se")]
  })

  raw_theta
}

#' use the \code{cuts.R} file to get the cut values for proficiency thresholds
#' @param name The name of the given test, e.g., \code{"ELA_G8"},
#'   \code{"Math_G11"}.
pull_cuts <- function(name) {
  cg <- unique(strsplit(name, "_G")[[1]])
  cuts[cuts$content == cg[1] & cuts$grade == cg[2], ]
}

#' convert raw score to theta and proficiency estimates
#' @param name The name of the given test, e.g., \code{"ELA_G8"}
#' @param round Implement standard rounding, rather than R's rounding.
#' @param raw_theta_tbl table with theta values, in a column called \code{theta}. Produced by \code{raw_to_scale()}
#'
convert_theta <- function(raw_theta_tbl, name, round = TRUE) {
  cuts_tmp <- pull_cuts(name)

  ss <- (raw_theta_tbl$theta * 10) + cuts_tmp$add
  if (isTRUE(round)) {
    ss <- round2(ss)
  }
    pl <-
      cut(
        ss,
        breaks = c(-Inf, cuts_tmp$c1, cuts_tmp$c2, cuts_tmp$c3, Inf),
        labels = 1:4
    )

  d <- data.frame(
    theta = raw_theta_tbl$theta,
    theta_se = raw_theta_tbl$theta_se,
    rit = ss,
    rit_se = raw_theta_tbl$theta_se * 10,
    perf_level = pl
  )
  raw_theta_ss <- raw_theta_tbl[!grepl("^theta", names(raw_theta_tbl))]
  cbind.data.frame(raw_theta_ss, d)
}

#' obtain raw to scale scores for the ELA subgroups

raw_to_scale_subscores <- function() {

  pd <- dbprocess::get_pattern_data()
  pd <- pd[grep('ELA', names(pd))]

  models <- rasch(pd)

  d <- lapply(pd, function(x) {
    x$id <- rownames(x)
    x
  })

  all_items <- lapply(d, names)

  reading_items <- lapply(all_items, grep, pattern = 'RL|RF|RI')
  writing_items <- lapply(all_items, grep, pattern = 'WR')

  all_item_names <- names(all_items)

  names(reading_items) <- gsub(pattern = 'ELA', replacement = 'Rdg', x = all_item_names)
  names(writing_items) <- gsub(pattern = 'ELA', replacement = 'Wri', x = all_item_names)

  for (i in seq_along(names(all_items))){
    reading_items[[i]] <- all_items[[i]][reading_items[[i]]]
    writing_items[[i]] <- all_items[[i]][writing_items[[i]]]
  }


  reading_subscore_pd <- lapply(reading_items, create_pattern_frame)
  writing_subscore_pd <- lapply(writing_items, create_pattern_frame)



  reading_rasch <-
    rasch(
      test = reading_subscore_pd
    )

  writing_rasch <-
    rasch(
      test = writing_subscore_pd
    )


  reading_thetas <- Map(estimate_theta, reading_rasch, reading_subscore_pd)
  reading_raw_theta <- lapply(reading_thetas, function(x) {
    x$raw <- rowSums(x[, seq_len(ncol(x) - 2)])
    x[c("raw", "theta", "theta_se")]
  })

  writing_thetas <- Map(estimate_theta, writing_rasch, writing_subscore_pd)
  writing_raw_theta <- lapply(writing_thetas, function(x) {
    x$raw <- rowSums(x[, seq_len(ncol(x) - 2)])
    x[c("raw", "theta", "theta_se")]
  })

  list(reading = reading_raw_theta, writing = writing_raw_theta)
}


#' Raw to RIT conversions
#'
#' Creates a single dataframe that has the raw score, theta value and standard
#' error associated with that raw score, the conversion to a RIT score and
#' RIT standard error, and the performance level for the corresponding score.
#' All conversions are returned for all tests.
#'
#' @param round Should the RIT scores be rounded? Defaults to \code{TRUE}. Note
#'   the rounding is done as typical, not as [base::round()] does. See the
#'   source code for the \code{round2} function for more detail.
#' @param subscore Do you want ELA subscores reported as well?
#' @export
#'
raw_to_rit <- function(round = TRUE, subscore = FALSE) {
  if (subscore == FALSE){
    rs <- raw_to_scale()
    out <- Map(convert_theta, rs, names(rs))
    out <- bind_dfs(out)
  }
  if (subscore == TRUE){
    rs <- raw_to_scale_subscores()
    rs_reading <- rs$reading
    rs_writing <- rs$writing
    out_reading <- Map(convert_theta, rs_reading, names(rs_reading))
    out_writing <- Map(convert_theta, rs_writing, names(rs_writing))
    out_writing <- bind_dfs(out_writing)
    out_reading <- bind_dfs(out_reading)
    out <- rbind.data.frame(out_writing, out_reading)
  }

  return(out)
}

