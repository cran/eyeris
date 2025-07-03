## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.dpi = 300
)

## ----setup, eval=FALSE--------------------------------------------------------
# # Install latest stable release from CRAN
# # install.packages("eyeris")
# 
# # pak
# # install.packages("pak")
# # pak::pak("shawntz/eyeris")
# 
# # Or install the development version from GitHub
# # install.packages("devtools")
# # devtools::install_github("shawntz/eyeris")

## ----pkgs, echo=FALSE, message=FALSE, warning=FALSE---------------------------
# install.packages("devtools")
# devtools::install_github("shawntz/eyeris")

## -----------------------------------------------------------------------------
library(eyeris)

## ----load-data----------------------------------------------------------------
demo_data <- eyelink_asc_demo_dataset()

## ----out.width='100%'---------------------------------------------------------
# Run an automated pipeline with no real-time inspection of parameters
output <- eyeris::glassbox(demo_data)

# Preview first and second steps of the pipeline
plot(
  output,
  steps = c(1, 2),
  preview_window = c(0, max(output$timeseries$block_1$time_secs)),
  seed = 0
)

## ----eval=FALSE, out.width='100%'---------------------------------------------
# output <- eyeris::glassbox(demo_data, interactive_preview = TRUE, seed = 0)

## -----------------------------------------------------------------------------
output <- eyeris::glassbox(
  demo_data,
  interactive_preview = FALSE, # TRUE to visualize each step in real-time
  deblink = list(extend = 40),
  lpfilt = list(plot_freqz = FALSE)
)

## ----echo=FALSE, out.width='80%', fig.align='center'--------------------------
"psychopy-eyetracker-sr-research-plug-in.png" |>
  knitr::include_graphics()

## -----------------------------------------------------------------------------
citation("eyeris")

