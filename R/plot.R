# plot.R

# documentation -----------------------------------------------------------

#' Plot
#'
#' Generic plotting function.
#'
#' @param x An **R** object. The `seahorse` package describes methods for
#'     [seahorse::Seahorse] and [seahorse::Herd-class] objects.
#' @param ... Parameters passed on to other methods.
#' @returns A plot.
setGeneric("plot", function(x, y, ...) standardGeneric("plot"))


# Seahorse ----------------------------------------------------------------

#' @export
#' @describeIn plot This method plots level or rate data from `Seahorse`
#'     objects. The plots can include or exclude blank and outlier wells by
#'     passing these arguments to `levels()` and `rates()`. Similarly,
#'     non-normalized or normalized rate data may be reviewed.
#' @param y \describe{
#'     `cells` = Plot normalization values. \cr
#'     `levels` = Plot levels. \cr
#'     `rates` = Plot rates. \cr
#'     `summary` = Plot summary data. \cr
#'     `mst` = Plot mito stress results. \cr
#'     `gst` = Plot glyco stress results. \cr
#'     `atp` = Plot ATP rate assay results.}
#' @param group Plot summary data for each experimental group or data from
#'     individual wells separately.
#' @param blanks Should blank wells be included in the plot?
#' @param outliers Should outliers be included in the plot?
#' @param normalize Divide values by normalization factor
#' @param type Plot ATP rate data as a "scatter" plot or "bar" plot.
#' @param ... Additional arguments passed along to `levels()` or `rates()`,
#'     including `blanks` and `outliers`.
#'
#' @aliases plot,Seahorse,character-method Seahorse_plot
#' @seealso [rates()], [levels()]
#' @examples
#' plot(sheldon, "levels")
#' plot(sheldon, "rates", group = FALSE)
#' plot(sheldon, "rates", group = TRUE, blanks = TRUE)
#' plot(sheldon, "rates", group = TRUE, blanks = FALSE)
#'
setMethod(
  "plot",
  signature = c(x = "Seahorse", y = "character"),
  function(
    x,
    y = c("cells", "levels", "rates", "summary", "mst", "gst", "atp"),
    group = TRUE,
    blanks = TRUE,
    outliers = TRUE,
    normalize = TRUE,
    type = c("scatter", "bar")
  ) {
    z <- rlang::arg_match(y)
    type <- rlang::arg_match(type)
    switch(
      z,
      cells = plot_cells(x),
      levels = plot_levels(
        x,
        group = group,
        blanks = blanks,
        outliers = outliers
      ),
      rates = plot_rates(
        x,
        group = group,
        blanks = blanks,
        outliers = outliers,
        normalize = normalize
      ),
      summary = plot_summary(x),
      mst = plot_mst(x),
      gst = plot_gst(x),
      atp = plot_atp(x, type = type)
    )
  }
)


# helpers -----------------------------------------------------------------

plot_cells <- function(x) {
  dplyr::left_join(x@wells, x@cells, by = "well") |>
    dplyr::filter(.data$type != "blank") |>
    ggplot2::ggplot() +
    ggplot2::aes(
      x = .data$group,
      y = .data$value
    ) +
    ggplot2::geom_point(
      size = 1.5,
      pch = 1
    ) +
    ggplot2::geom_errorbar(
      stat = "summary",
      fun.data = ggplot2::mean_se,
      ggplot2::aes(color = .data$group),
      width = 0,
      linewidth = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      stat = "summary",
      fun = "mean",
      ggplot2::aes(fill = .data$group),
      pch = 21,
      color = "white",
      size = 3,
      stroke = 1,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      title = "Normalization Values",
      x = NULL,
      y = units(x),
      color = NULL,
      fill = NULL
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 5))
}

plot_levels <- function(x, group = TRUE, blanks = TRUE, outliers = TRUE) {
  df <- levels(x, blanks = blanks, outliers = outliers)
  out <-
    dplyr::mutate(
      x@outliers,
      sensor = ifelse(.data$rate == "OCR", "O2", "pH"),
      outlier = TRUE
    ) |>
    dplyr::select(-"rate")
  df <-
    dplyr::left_join(df, out, by = c("sensor", "well")) |>
    dplyr::mutate(outlier = ifelse(is.na(.data$outlier), FALSE, TRUE))

  p <-
    df |>
    ggplot2::ggplot() +
    ggplot2::facet_wrap(
      facet = ggplot2::vars(.data$sensor),
      ncol = 1,
      scales = "free_y",
      strip.position = "left",
      labeller = plot_labeller()
    ) +
    ggplot2::aes(
      x = .data$time / 60,
      y = .data$value
    ) +
    ggplot2::labs(
      x = "Time (min)",
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background = ggplot2::element_blank()
    )

  if (group) {
    p +
      ggplot2::stat_summary(
        ggplot2::aes(color = .data$group),
        geom = "line",
        fun = "mean",
        show.legend = TRUE
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(
          group = .data$group,
          color = .data$group,
          fill = .data$group
        ),
        geom = "smooth",
        fun.data = ggplot2::mean_se,
        se = TRUE,
        alpha = 0.25,
        show.legend = FALSE
      )
  } else {
    p +
      ggplot2::geom_line(
        ggplot2::aes(
          group = .data$well,
          color = .data$group,
          linetype = .data$outlier
        ),
        show.legend = TRUE
      ) +
      ggplot2::guides(linetype = "none")
  }
}


plot_rates <- function(
    x,
    group = TRUE,
    blanks = TRUE,
    outliers = TRUE,
    normalize = TRUE
) {
  if (normalize) {
    unit <- x@units
    blanks <- FALSE
  } else {
    unit <- NA_character_
  }

  df <- rates(x, blanks = blanks, outliers = outliers, normalize = normalize)

  out <- dplyr::mutate(x@outliers, outlier = TRUE)
  df <-
    dplyr::left_join(df, out, by = c("rate", "well")) |>
    dplyr::mutate(outlier = ifelse(is.na(.data$outlier), FALSE, TRUE))

  p <-
    ggplot2::ggplot(df) +
    ggplot2::facet_wrap(
      facet = ggplot2::vars(.data$rate),
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
    ggplot2::labs(
      x = "Measurement",
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
    ggplot2::theme(
      strip.placement = "outside",
      strip.background = ggplot2::element_blank()
    )

  if (group) {
    p +
      ggplot2::stat_summary(
        ggplot2::aes(group = .data$group),
        geom = "line",
        fun = "mean",
        show.legend = FALSE
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(group = .data$group),
        geom = "linerange",
        fun.data = ggplot2::mean_se,
        show.legend = FALSE
      ) +
      ggplot2::stat_summary(
        ggplot2::aes(fill = .data$group),
        geom = "point",
        fun = "mean",
        pch = 21,
        size = 2,
        color = "white"
      )
  } else {
    grp <- intersect(names(df), c("well", "experiment"))
    if (grp == "well") {
      p <-
        p +
        ggplot2::geom_line(
          ggplot2::aes(
            group = interaction(.data[[grp]]),
            linetype = .data$outlier
          ),
          show.legend = FALSE
        )
    } else if (grp == "experiment") {
      p <-
        p +
        ggplot2::geom_line(
          ggplot2::aes(
            group = interaction(.data[[grp]], .data$group),
            linetype = .data$outlier
          ),
          show.legend = FALSE
        )
    }
    p +
      ggplot2::geom_point(
        ggplot2::aes(
          fill = .data$group,
          shape = .data$outlier
          ),
        pch = 21,
        color = "white"
      ) +
      ggplot2::guides(shape = "none")
  }
}


plot_summary <- function(x) {
  df <- x@summary
  unit <- x@units

  ggplot2::ggplot(df) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$rate),
      ncol = 1,
      scales = "free_y",
      strip.position = "top",
      labeller = plot_labeller(unit)
    ) +
    ggplot2::aes(
      x = .data$stage,
      y = .data$value
    ) +
    ggplot2::geom_bar(
      stat = "summary",
      fun = "mean",
      ggplot2::aes(fill = .data$group),
      position = ggplot2::position_dodge(width = 0.7),
      width = 0.65,
      show.legend = TRUE
    ) +
    ggplot2::geom_errorbar(
      stat = "summary",
      fun.data = ggplot2::mean_se,
      ggplot2::aes(group = .data$group),
      position = ggplot2::position_dodge(width = 0.7),
      width = 0.2,
      linewidth = 0.4,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(group = .data$group),
      position = ggplot2::position_dodge(width = 0.7),
      size = 0.75,
      pch = 1
    ) +
    ggplot2::labs(
      title = "Summary Data",
      x = NULL,
      y = NULL,
      color = NULL,
      fill = NULL
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank()
    )
}

plot_mst <- function(x) {
  df <- mst(x)

  ggplot2::ggplot(df) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$rate),
      nrow = 1,
      scales = "free_y",
      strip.position = "top",
      labeller = ggplot2::as_labeller(toupper)
    ) +
    ggplot2::aes(
      x = .data$group,
      y = .data$value
    ) +
    ggplot2::geom_point(
      size = 1.5,
      pch = 1
    ) +
    ggplot2::geom_errorbar(
      stat = "summary",
      fun.data = ggplot2::mean_se,
      ggplot2::aes(color = .data$group),
      width = 0,
      linewidth = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      stat = "summary",
      fun = "mean",
      ggplot2::aes(fill = .data$group),
      pch = 21,
      color = "white",
      size = 3,
      stroke = 1,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      title = "Mito Stress Assay",
      x = NULL,
      y = "Fraction",
      color = NULL,
      fill = NULL
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
    ggplot2::theme(
      strip.background = ggplot2::element_blank()
    )
}

plot_gst <- function(x) {
  df <- gst(x)

  ggplot2::ggplot(df) +
    ggplot2::aes(
      x = .data$group,
      y = .data$value
    ) +
    ggplot2::geom_point(
      size = 1.5,
      pch = 1
    ) +
    ggplot2::geom_errorbar(
      stat = "summary",
      fun.data = ggplot2::mean_se,
      ggplot2::aes(color = .data$group),
      width = 0,
      linewidth = 1,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      stat = "summary",
      fun = "mean",
      ggplot2::aes(fill = .data$group),
      pch = 21,
      color = "white",
      size = 3,
      stroke = 1,
      show.legend = FALSE
    ) +
    ggplot2::labs(
      title = "Glyco Stress Assay",
      x = NULL,
      y = "Fraction",
      color = NULL,
      fill = NULL
    ) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 7))
}

plot_atp <- function(x, type) {
  df <- atp(x)
  unit <- units(x)

  switch(
    type,
    scatter = {
      atp(x, "scatter") |>
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .data$mito$y,
          y = .data$glyco$y,
          xmin = .data$mito$ymin,
          xmax = .data$mito$ymax,
          ymin = .data$glyco$ymin,
          ymax = .data$glyco$ymax,
          fill = .data$group,
          color = .data$group
        ) +
        ggplot2::geom_errorbar(
          width = 0,
          linewidth = 1,
          show.legend = FALSE
        ) +
        ggplot2::geom_errorbarh(
          height = 0,
          linewidth = 1,
          show.legend = FALSE
        ) +
        ggplot2::geom_point(
          pch = 21,
          color = "white",
          stroke = 1,
          size = 3,
          show.legend = FALSE
        ) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
        ggplot2::labs(
          title = "ATP Production Rates",
          x = "Mito ATP",
          y = "Glyco ATP",
          color = NULL,
          fill = NULL
        ) +
        ggplot2::coord_fixed(expand = TRUE)
    },

    bar = {
      ls <- atp(x, "bar")

      fills <- c("#1f78b4", "#e31a1c")
      shapes <- 16:17

      ggplot2::ggplot(ls$means) +
        ggplot2::aes(
          x = .data$group,
          y = .data$y
        ) +
        ggplot2::geom_col(
          ggplot2::aes(fill = .data$rate),
          width = 0.65
        ) +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$ymin,
            ymax = .data$ymax
          ),
          width = 0.2,
          linewidth = 0.4
        ) +
        ggplot2::geom_point(
          data = ls$pts,
          ggplot2::aes(
            y = .data$value,
            shape = .data$rate.x
          ),
          color = "black",
          size = 1.5,
          show.legend = TRUE
        ) +
        ggplot2::labs(
          title = "ATP Production Rates",
          x = NULL,
          y = stringr::str_c("pmol/min/", unit),
          fill = NULL,
          shape = NULL
        ) +
        ggplot2::guides(
          fill = ggplot2::guide_legend(
            override.aes = list(fill = fills, shape = rev(shapes))
          )
        ) +
        ggplot2::guides(shape = "none") +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 4)) +
        ggplot2::scale_fill_manual(values = fills) +
        ggplot2::scale_shape_manual(values = shapes) +
        ggplot2::theme(
          legend.position = "bottom",
          legend.key.size = ggplot2::unit(1, "lines")
        )
    }
  )
}

plot_labeller <- function(units = NA_character_) {
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
