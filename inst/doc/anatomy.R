## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# system.file("extdata", "memory.asc", package = "eyeris") |>
#   eyeris::load_asc(block = "auto") |>
#   eyeris::deblink(extend = 50) |>
#   eyeris::detransient(n = 16) |>
#   eyeris::interpolate() |>
#   eyeris::lpfilt(wp = 4, ws = 8, rp = 1, rs = 35, plot_freqz = TRUE) |>
#   # eyeris::detrend() |>  # optional (please read docs before enabling)
#   eyeris::zscore()

## -----------------------------------------------------------------------------
citation("eyeris")

