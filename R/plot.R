# plot.R

plot_time <- function(df, x, y, group)
{
  ggplot2::ggplot(df) +
    ggplot2::aes(
      x = {{x}},
      y = {{y}},
      color = {{group}}
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data$well)
    ) +
    ggplot2::geom_point()
}

plot_rates <- function(x, normalize = TRUE)
{
  if (is.na(bf(x))) {
    a <- c("OCR", "ECAR")
  } else {
    a <- c("OCR", "PER")
  }



  rates(x) %>%
    dplyr::filter(rate %in% a) %>%
    plot_time(x = measurement, y = value, group = group) +
    ggplot2::facet_wrap(~ rate, ncol = 1, scales = "free_y")
}
