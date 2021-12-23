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

### TODO: YOU ARE HERE ###

plot_rates <- function(x)
{
  rates(x) %>%
    tidyr::pivot_longer(cols = c(OCR, ECAR), names_to = "measurement", values_to = )
}
