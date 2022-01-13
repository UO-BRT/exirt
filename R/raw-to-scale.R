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

pull_cuts <- function(name) {
  cg <- unique(strsplit(name, "_G")[[1]])
  cuts[cuts$content == cg[1] & cuts$grade == cg[2], ]
}

convert_theta <- function(raw_theta_tbl, name, round = TRUE) {
  cuts <- pull_cuts(name)

  ss <- (raw_theta_tbl$theta * 10) + cuts$add
  if (round) {
    ss <- round2(ss)
  }
  pl <- cut(ss,
            breaks = c(-Inf, cuts$c1, cuts$c2, cuts$c3, Inf),
            labels = 1:4)

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
#' @export
#'
raw_to_rit <- function(round = TRUE) {
  rs <- raw_to_scale()
  out <- lapply(rs, convert_theta, names(rs))
  bind_dfs(out)
}
