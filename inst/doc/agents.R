## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# glassbox(
#   file,
#   deblink = list(extend = 40),       # -> eyeris::deblink(extend = 40)
#   lpfilt  = list(plot_freqz = FALSE) # -> eyeris::lpfilt(plot_freqz = FALSE)
# )

## ----eval=FALSE---------------------------------------------------------------
# system.file("extdata", "memory.asc", package = "eyeris") |>
#   eyeris::load_asc(block = "auto") |>
#   eyeris::deblink(extend = 50) |>
#   eyeris::detransient(n = 16) |>
#   eyeris::interpolate() |>
#   eyeris::lpfilt(wp = 4, ws = 8, rp = 1, rs = 35) |>
#   eyeris::zscore()

## ----eval=FALSE---------------------------------------------------------------
# library(eyeris)
# 
# # 1. One opinionated call runs the full, expert-default pipeline:
# output <- glassbox(eyelink_asc_demo_dataset())
# 
# # 2. Override any step by name with a named list -- no positional guessing:
# output <- glassbox(
#   "sub-001_task-memory.asc",
#   deblink = list(extend = 40),        # args for eyeris::deblink()
#   lpfilt  = list(plot_freqz = FALSE)  # args for eyeris::lpfilt()
# )
# 
# # 3. Extract time-locked epochs around each event of interest:
# output <- epoch(
#   output,
#   events = "PROBE_START_{trial}",
#   limits = c(-1, 2),                  # seconds around each event
#   label  = "probe"
# )
# 
# # 4. Write BIDS-like derivatives + an interactive QC report (predictable paths):
# bidsify(
#   output,
#   bids_dir       = "~/study",
#   participant_id = "001",
#   session_num    = "01",
#   task_name      = "memory"
# )
# 
# # 5. Auto-generate a methods paragraph from the exact parameters that ran:
# boilerplate(output)

## -----------------------------------------------------------------------------
citation("eyeris")

