# calcs.R

level_O2 <- function(raw, config, blanks) {
  df <-
    raw |>
    dplyr::filter(.data$sensor == "O2") |>
    dplyr::select("measurement":"sensor", "fluor")

  blanks <-
    blanks[blanks$rate == "OCR", "well"] |>
    dplyr::pull()

  if (!all(is.na(blanks))) {
    ft <-
      df |>
      dplyr::filter(.data$well %in% blanks) |>
      dplyr::group_by(.data$time) |>
      dplyr::summarise(ft = mean(.data$fluor))
  } else {
    ft <-
      tibble::tibble(
        time = unique(df$time),
        ft = config$O2_tar
      )
  }
  df |>
    dplyr::left_join(ft, by = "time") |>
    dplyr::mutate(
      O2 = config$O20 + config$f0 * (.data$ft - .data$fluor) /
        .data$fluor / .data$ft / config$Ksv
    ) |>
    dplyr::select("measurement", "well", "time", "O2")
}


rate_O2 <- function(levels, config) {
  levels |>
    dplyr::group_by(.data$well) |>
    tidyr::nest() |>
    dplyr::mutate(OCR = purrr::map(.data$data, calc_ocr, config = config)) |>
    tidyr::unnest(c("OCR")) |>
    dplyr::select(!"data")
}


calc_ocr <- function(levels, config) {
  with(
    config,
    {
      indices <-
        levels |>
        dplyr::group_by(.data$measurement) |>
        dplyr::summarize(max = which.max(.data$time)) |>
        dplyr::mutate(idx = cumsum(.data$max)) |>
        dplyr::pull(.data$idx) |>
        {\(x) x[-length(x)]}()

      gaps <- purrr::map_dfr(indices, \(x) fill_O2(levels, idx = x))

      levels |>
        dplyr::mutate(type = "measure") |>
        dplyr::bind_rows(gaps) |>
        dplyr::arrange(.data$measurement, .data$time) |>
        dplyr::mutate(
          sg_O2 = pracma::savgol(.data$O2, 7, 2, 0),
          sg_O2 = replace(.data$sg_O2, 1:3, .data$sg_O2[[4]]),
          dO2_dt = pracma::savgol(.data$O2, 7, 2, 1) /
            pracma::savgol(.data$time, 7, 2, 1),
          dO2_dt = replace(.data$dO2_dt, 1:3, 0),
          d2O2_dt2 = pracma::savgol(.data$O2, 7, 2, 2) /
            pracma::savgol(.data$time, 7, 2, 1) ^ 2,
          d2O2_dt2 = replace(.data$d2O2_dt2, 1:3, 0),
          exp_tag = exp((Kaw + Kw) * .data$time),
          exp = .data$exp_tag *
            (Kaw * O20 + Kw * (.data$sg_O2 + .data$dO2_dt / Kp)),
          integral = calc_integral(.data$time, .data$exp),
          O2w = (.data$O2[[1]] + .data$integral) / .data$exp_tag,
          ocr_raw = Kac * O20 - (Kac + Kc) * .data$sg_O2 + Kc * .data$O2w -
            (Kac + Kp + Kc) / Kp * .data$dO2_dt - .data$d2O2_dt2 / Kp,
          ocr = 1E9 * 60 * Vc * 1E-6 * (.data$ocr_raw * 0.214 / O20)
        ) |>
        dplyr::filter(.data$type == "measure") |>
        dplyr::group_by(.data$measurement) |>
        dplyr::summarise(OCR = mean(.data$ocr[4:(length(.data$ocr) - 3)]))
    }
  )
}


fill_O2 <- function(df, idx) {
  a <- dplyr::slice(df, idx)
  b <- dplyr::slice(df, idx + 1)
  t <- seq(a$time, b$time, length = 25)
  t <- t[-c(1, length(t))]
  o <- b$O2 - (b$O2 - a$O2) * exp((t[[1]] - t) / 30)
  tibble::tibble(
    type = "mix",
    time = t,
    O2 = o,
    measurement = (a$measurement + b$measurement) / 2
  )
}


int <- function(x, y) {
  polynom::poly.calc(x, y) |>
    polynom::integral(limits = c(min(x), x[[2]]))
}


calc_integral <- function(t, e) {
  slider::slide2_dbl(
    .x = t,
    .y = e,
    int,
    .before = 1L,
    .after = 1L,
    .complete = TRUE
  ) |>
    tidyr::replace_na(0) |>
    cumsum()
}


level_pH <- function(raw, config) {
  pH_m <- 2.27061645420046
  pH_b <- 5.12938272236599

  raw |>
    dplyr::filter(.data$sensor == "pH") |>
    dplyr::select("measurement":"sensor", "fluor") |>
    dplyr::mutate(pH = .data$fluor / config$pH_em * pH_m + pH_b) |>
    dplyr::select("measurement", "well", "time", "pH")
}


rate_pH <- function(levels, blanks) {
  blanks <-
    blanks[blanks$rate == "ECAR", "well"] |>
    dplyr::pull()

  ecar <-
    levels |>
    dplyr::group_by(.data$well, .data$measurement) |>
    dplyr::slice(-(1:3)) |>
    dplyr::summarise(ECAR = -1000 * 60 * stats::coef(stats::lm(pH ~ time))[[2]])

  if (!all(is.na(blanks))) {
    ecar <-
      ecar |>
      dplyr::filter(.data$well %in% blanks) |>
      dplyr::group_by(.data$measurement) |>
      dplyr::summarise(cf = mean(.data$ECAR)) |>
      dplyr::right_join(ecar, by = "measurement") |>
      dplyr::mutate(ECAR = .data$ECAR - .data$cf) |>
      dplyr::select("well", "measurement", "ECAR")
  }
  dplyr::arrange(ecar, .data$well, .data$measurement)
}
