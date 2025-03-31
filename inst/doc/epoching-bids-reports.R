## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, fig.show='hide'---------------------------------------------------
# Load eyeris
library(eyeris)

# Load the example memory task file and run default glassbox preproc workflow
eye <- system.file("extdata", "memory.asc", package = "eyeris") |>
  glassbox()

## -----------------------------------------------------------------------------
eye_1a <- eye |>
  epoch(events = "PROBE*", limits = c(-1, 1))

## -----------------------------------------------------------------------------
eye_1a$epoch_probe

## -----------------------------------------------------------------------------
eye_1b <- eye |>
  epoch(
    events = "PROBE_START_{trial}",
    limits = c(0, 1),
    label = "probeAfter"
  )

eye_1b |>
  purrr::pluck("epoch_probeAfter") |>
  head()

## ----echo=FALSE---------------------------------------------------------------
eye_1b |>
  purrr::pluck("epoch_probeAfter") |>
  purrr::pluck("block_1") |>
  dplyr::select(template:trial) |>
  head(5)

## ----eval=FALSE---------------------------------------------------------------
# eye_1c <- eye |>
#   epoch(
#     events = "PROBE_START_{trial}",
#     limits = c(0, 1),
#     label = "probeEpochs",
#     calc_baseline = TRUE,
#     apply_baseline = TRUE,
#     baseline_type = "sub",
#     baseline_events = "DELAY_STOP_*",
#     baseline_period = c(-1, 0)
#   )

## ----eval=FALSE---------------------------------------------------------------
# start_events <- data.frame(
#   time = c(11334491, 11338691),
#   msg = c("TRIALID 22", "TRIALID 23")
# )
# 
# end_events <- data.frame(
#   time = c(11337158, 11341292),
#   msg = c("RESPONSE_22", "RESPONSE_23")
# )
# 
# eye_1d <- eye |>
#   epoch(
#     events = list(start_events, end_events, 1), # 1 = block number
#     label = "manualTrials"
#   )

## ----eval=FALSE---------------------------------------------------------------
# bidsify(
#   eyeris = eye_1c,
#   bids_dir = "~/Documents/eyeris",
#   participant_id = "001",
#   session_num = "01",
#   task_name = "assocmem",
#   run_num = "01",
#   save_raw = TRUE, # Also save raw timeseries
#   html_report = TRUE # Generate a preprocessing summary
# )

## -----------------------------------------------------------------------------
citation("eyeris")

