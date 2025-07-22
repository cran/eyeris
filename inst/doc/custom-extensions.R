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
# #' @param call_info A list of call information and parameters. If not provided,
# #'   it will be generated from the function call.
# #'
# #' @return Updated `eyeris` object with new winsorized pupil column.
# winsorize <- function(eyeris, lower = 0.01, upper = 0.99, call_info = NULL) {
#   # create call_info if not provided
#   call_info <- if (is.null(call_info)) {
#     list(
#       call_stack = match.call(),
#       parameters = list(lower = lower, upper = upper)
#     )
#   } else {
#     call_info
#   }
# 
#   # handle binocular objects
#   if (is_binocular_object(eyeris)) {
#     # process left and right eyes independently
#     left_result <- eyeris$left |>
#       pipeline_handler(
#         winsorize_pupil,
#         "winsorize",
#         lower = lower,
#         upper = upper,
#         call_info = call_info
#       )
# 
#     right_result <- eyeris$right |>
#       pipeline_handler(
#         winsorize_pupil,
#         "winsorize",
#         lower = lower,
#         upper = upper,
#         call_info = call_info
#       )
# 
#     # return combined structure
#     list_out <- list(
#       left = left_result,
#       right = right_result,
#       original_file = eyeris$original_file,
#       raw_binocular_object = eyeris$raw_binocular_object
#     )
# 
#     class(list_out) <- "eyeris"
# 
#     return(list_out)
#   } else {
#     # regular eyeris object, process normally
#     eyeris |>
#       pipeline_handler(
#         winsorize_pupil,
#         "winsorize",
#         lower = lower,
#         upper = upper,
#         call_info = call_info
#       )
#   }
# }

## ----eval=FALSE---------------------------------------------------------------
# system.file("extdata", "memory.asc", package = "eyeris") |>
#   eyeris::load_asc(block = "auto") |>
#   eyeris::deblink(extend = 50) |>
#   winsorize()

## ----eval=FALSE---------------------------------------------------------------
# custom_function <- function(eyeris, param1, param2, call_info = NULL) {
#   # custom call_info with additional metadata
#   call_info <- if (is.null(call_info)) {
#     list(
#       call_stack = match.call(),
#       parameters = list(param1 = param1, param2 = param2),
#       metadata = list(
#         timestamp = Sys.time(),
#         version = "1.0.0",
#         description = "Custom processing step"
#       )
#     )
#   } else {
#     call_info
#   }
# 
#   pipeline_handler(
#     eyeris,
#     custom_pupil_function,
#     "custom",
#     param1 = param1,
#     param2 = param2,
#     call_info = call_info
#   )
# }

## -----------------------------------------------------------------------------
citation("eyeris")

