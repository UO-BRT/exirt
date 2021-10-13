items <- orextdb::db_get("Items")

item_diffs <- purrr::map_df(
  fs::dir_ls(here::here("ifiles20")),
  readr::read_csv,
  .id = "file"
) |>
  dplyr::select(item_id_brt = ItemID, difficulty = Difficulty)

items <- dplyr::left_join(items, item_diffs)

###############

all_items <- dbprocess::get_items()




####
prior <- readr::read_csv(here::here("pfiles21", "g5ELApfiles21.csv"))

tmp <- estimate_theta(fixed, all_items$ELA_G5)

dplyr::left_join(
  tmp[ ,c("ssid", "theta")],
  prior[ ,c("ssid", "Theta")]
) |>
  #dplyr::arrange(desc(theta))
  ggplot2::ggplot(ggplot2::aes(theta, Theta)) +
  ggplot2::geom_point() +
  ggplot2::geom_line(color = "magenta") +
  ggplot2::geom_abline(slope = 1, intercept = 0, color = "cornflowerblue") +
  ggplot2::labs(x = "New Estimates", y = "Winsteps Estimates",
                title = "Comparison of Person Estimates") +
  ggplot2::theme_minimal()
