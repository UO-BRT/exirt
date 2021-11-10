raw_to_scale <- function() {
  items <- db_get("Items")
  pd <- get_pattern_data(items = items)

  models <- lapply(pd, rasch, items)

  d <- lapply(pd, function(x) {
    x$id <- rownames(x)
    x
  })

  thetas <- Map(estimate_theta, models, d)
  raw_theta <- lapply(thetas, function(x) {
    x$raw <- rowSums(x[ ,seq_len(ncol(x) - 3)])
    x[c("raw", "theta", "theta_se")]
  })

  raw_theta <- lapply(raw_theta, function(x) x[unique(x$raw), ])
  raw_theta
}

pull_cuts <- function(name) {
  cg <- unique(strsplit(name, "_G")[[1]])
  cuts[cuts$content == cg[1] & cuts$grade == cg[2], ]
}

convert_theta <- function(raw_theta_tbl, name, round = TRUE) {
  cut <- pull_cuts(name)

  ss <- (raw_theta_tbl$theta * 10) + cut$add
  if (round) {
    ss <- round2(ss)
  }
  pl <- cut(ss,
            breaks = c(-Inf, cut$c1, cut$c2, cut$c3, Inf),
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

# rs <- raw_to_scale()
# mapping <- lapply(rs, convert_theta, names(rs))
