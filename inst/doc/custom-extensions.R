## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# winsorize_pupil <- function(x, prev_op, lower = 0.01, upper = 0.99) {
#   vec <- x[[prev_op]]
#   q <- quantile(vec, probs = c(lower, upper), na.rm = TRUE)
#   vec[vec < q[1]] <- q[1]
#   vec[vec > q[2]] <- q[2]
#   vec
# }

## ----eval=FALSE---------------------------------------------------------------
# #' Winsorize pupil values
# #'
# #' Applies winsorization to extreme pupil values within each block.
# #'
# #' @param eyeris An `eyeris` object created by [load_asc()].
# #' @param lower Lower quantile threshold. Default is 0.01.
# #' @param upper Upper quantile threshold. Default is 0.99.
# #'
# #' @return Updated `eyeris` object with new winsorized pupil column.
# winsorize <- function(eyeris, lower = 0.01, upper = 0.99) {
#   pipeline_handler(
#     eyeris,
#     winsorize_pupil,
#     "winsorize",
#     lower = lower,
#     upper = upper
#   )
# }

## ----eval=FALSE---------------------------------------------------------------
# system.file("extdata", "memory.asc", package = "eyeris") |>
#   eyeris::load_asc(block = "auto") |>
#   eyeris::deblink(extend = 50) |>
#   winsorize()

## -----------------------------------------------------------------------------
citation("eyeris")

