# plot.R

plot_labeller <- function(units = NA_character_)
{
  if (!is.na(units)) {
    x <- stringr::str_c("/", units)
  } else {
    x <- ""
  }
  ggplot2::as_labeller(
    c(
      OCR = stringr::str_c("OCR~(pmol~O[2]/min", x, ")"),
      ECAR = stringr::str_c("ECAR~(mpH/min", x, ")"),
      PER = stringr::str_c("PER~(pmol~H^'+'/min", x, ")"),
      O2 = "O[2]~(mmHg)",
      pH = "pH"
    ),
    default = ggplot2::label_parsed
  )
}

plot_rates <- function(x, normalize = TRUE, by = c("well", "group"))
{
  if (is.na(bf(x))) {
    a <- c("OCR", "ECAR")
  } else {
    a <- c("OCR", "PER")
  }
  df <- dplyr::filter(rates(x), .data$rate %in% a)

  if (normalize) {
    df <-
      dplyr::left_join(df, cells(x), by = "well") %>%
      dplyr::mutate(value = .data$value.x / .data$value.y )
    unit <- units(x)
  } else {
    unit <- NA_character_
  }

  ggplot2::ggplot(df) +
    ggplot2::facet_wrap(
      ~ rate,
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      labeller = plot_labeller(unit)
    ) +
    ggplot2::aes(
      x = .data$measurement,
      y = .data$value,
      color = .data$group
    ) +
    {if (by == "well") {
      list(
        ggplot2::geom_line(
          ggplot2::aes(group = .data$well),
          show.legend = FALSE
        ),
        ggplot2::geom_point(
          ggplot2::aes(fill = .data$group),
          pch = 21,
          color = "white"
        )
      )
    }} +
    {if (by == "group") {
      list(
        ggplot2::stat_summary(
          ggplot2::aes(group = .data$group),
          geom = "line",
          fun = "mean",
          show.legend = FALSE
        ),
        ggplot2::stat_summary(
          ggplot2::aes(group = .data$group),
          geom = "linerange",
          fun.data = "mean_se",
          show.legend = FALSE
        ),
        ggplot2::stat_summary(
          ggplot2::aes(fill = .data$group),
          geom = "point",
          fun = "mean",
          pch = 21,
          size = 2,
          color = "white"
        )
      )
    }} +
    ggplot2::labs(
      x = "Measurement",
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    ggplot2::theme_gray() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background = ggplot2::element_blank()
    )
}

plot_levels <- function(x, by = c("well", "group"))
{
  ggplot2::ggplot(levels(x)) +
    ggplot2::facet_wrap(
      ~ sensor,
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      labeller = plot_labeller()
    ) +
    ggplot2::aes(
      x = .data$time,
      y = .data$value,
      color = .data$group
    ) +
    {if (by == "well") {
      list(
        ggplot2::geom_line(
          ggplot2::aes(group = .data$well),
          show.legend = FALSE
        )
      )
    }} +
    {if (by == "group") {
      list(
        ggplot2::stat_summary(
          ggplot2::aes(group = .data$group),
          geom = "line",
          fun = "mean",
          show.legend = FALSE
        ),
        ggplot2::stat_summary(
          ggplot2::aes(
            group = .data$group,
            fill = .data$group
          ),
          geom = "smooth",
          fun.data = "mean_se",
          se = TRUE,
          alpha = 0.25,
          show.legend = FALSE
        )
      )
    }} +
    ggplot2::labs(
      x = "Time",
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    ggplot2::theme_gray() +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background = ggplot2::element_blank()
    )
}
