## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  fig.dpi = 300
)

## ----setup, eval=FALSE--------------------------------------------------------
# # Install from CRAN (coming soon)
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
demo_data <- system.file("extdata", "memory.asc", package = "eyeris")

## ----out.width='100%'---------------------------------------------------------
# Run an automated pipeline with no real-time inspection of parameters
output <- eyeris::glassbox(demo_data)

# Preview first and last steps of the pipeline
plot(
  output,
  steps = c(1, 5),
  preview_window = c(0, nrow(output$timeseries$block_1)),
  seed = 0
)

## ----eval=FALSE, out.width='100%'---------------------------------------------
# output <- eyeris::glassbox(demo_data, confirm = TRUE, seed = 0)

## -----------------------------------------------------------------------------
output <- eyeris::glassbox(
  demo_data,
  confirm = FALSE, # TRUE if you want to visualize each step in real-time
  deblink = list(extend = 40),
  lpfilt = list(plot_freqz = FALSE)
)

## ----echo=FALSE, out.width='80%', fig.align='center'--------------------------
"psychopy-eyetracker-sr-research-plug-in.png" |>
  knitr::include_graphics()

## -----------------------------------------------------------------------------
citation("eyeris")

