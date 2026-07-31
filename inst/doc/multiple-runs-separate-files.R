## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# library(eyeris)
# 
# dl <- path.expand("~/Downloads")
# asc_files <- file.path(dl, c(
#   "sub-AB01_t1_2024-01-15_10h00.00.000.asc",
#   "sub-AB01_t2_2024-01-15_10h30.00.000.asc",
#   "sub-AB01_t3_2024-01-15_11h00.00.000.asc"
# ))
# stopifnot(all(file.exists(asc_files)))
# 
# output_dir <- path.expand("~/Documents/eyeris")
# 
# for (i in seq_along(asc_files)) {
#   glassbox(asc_files[i], load_asc = list(block = i), verbose = TRUE) |>
#     epoch(
#       events = "TST_trial-{trial}_{item}_{associate}",
#       limits = c(0, 0.1),
#       label  = "trialEpochs"
#     ) |>
#     bidsify(
#       bids_dir       = output_dir,
#       participant_id = "AB01",
#       session_num    = "01",
#       task_name      = "assocmem",
#       save_raw       = TRUE,
#       html_report    = TRUE,
#       report_seed    = 0
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# library(eyeris)
# 
# dl <- path.expand("~/Downloads")
# asc_files <- file.path(dl, c(
#   "sub-AB01_t1_2024-01-15_10h00.00.000.asc",
#   "sub-AB01_t2_2024-01-15_10h30.00.000.asc",
#   "sub-AB01_t3_2024-01-15_11h00.00.000.asc"
# ))
# stopifnot(all(file.exists(asc_files)))
# 
# output_dir <- path.expand("~/Documents/eyeris")
# 
# for (i in seq_along(asc_files)) {
#   glassbox(asc_files[i], verbose = TRUE) |>
#     epoch(
#       events = "TST_trial-{trial}_{item}_{associate}",
#       limits = c(0, 0.1),
#       label  = "trialEpochs"
#     ) |>
#     bidsify(
#       bids_dir       = output_dir,
#       run_num        = i,
#       participant_id = "AB01",
#       session_num    = "01",
#       task_name      = "assocmem",
#       save_raw       = TRUE,
#       html_report    = TRUE,
#       report_seed    = 0
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# asc_files <- file.path(dl, c(
#   "sub-AB01_t1_2024-01-15_10h00.00.000.asc", # run 1
#   "sub-AB01_t3_2024-01-15_11h00.00.000.asc"  # run 3 (run 2 not collected)
# ))
# run_nums <- c(1, 3) # the TRUE run numbers, in the same order as `asc_files`
# 
# for (i in seq_along(asc_files)) {
#   glassbox(asc_files[i], load_asc = list(block = run_nums[i]), verbose = TRUE) |>
#     epoch(
#       events = "TST_trial-{trial}_{item}_{associate}",
#       limits = c(0, 0.1),
#       label  = "trialEpochs"
#     ) |>
#     bidsify(
#       bids_dir       = output_dir,
#       participant_id = "AB01",
#       session_num    = "01",
#       task_name      = "assocmem",
#       save_raw       = TRUE,
#       html_report    = TRUE,
#       report_seed    = 0
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# for (f in asc_files) {
#   run_n <- as.integer(sub(".*_t(\\d+)_.*", "\\1", basename(f)))
# 
#   glassbox(f, load_asc = list(block = run_n), verbose = TRUE) |>
#     epoch(
#       events = "TST_trial-{trial}_{item}_{associate}",
#       limits = c(0, 0.1),
#       label  = "trialEpochs"
#     ) |>
#     bidsify(
#       bids_dir       = output_dir,
#       participant_id = "AB01",
#       session_num    = "01",
#       task_name      = "assocmem",
#       save_raw       = TRUE,
#       html_report    = TRUE,
#       report_seed    = 0
#     )
# }

## ----eval=FALSE---------------------------------------------------------------
# glassbox(asc_files[2], load_asc = list(block = 2), verbose = TRUE) |>
#   epoch(
#     events = "TST_trial-{trial}_{item}_{associate}",
#     limits = c(0, 0.1),
#     label  = "trialEpochs"
#   ) |>
#   bidsify(
#     bids_dir       = output_dir,
#     participant_id = "AB01",
#     session_num    = "01",
#     task_name      = "assocmem",
#     save_raw       = TRUE,
#     html_report    = TRUE,
#     report_seed    = 0
#   )

## ----confirm-single-run, fig.show='hide'--------------------------------------
library(eyeris)

# the bundled demo file stands in for one of your per-run .asc files
demo_file <- eyelink_asc_demo_dataset()

eye <- glassbox(demo_file, load_asc = list(block = 1), verbose = FALSE)

length(eye$timeseries) # 1 -> exactly one run in this file
names(eye$timeseries)  # "block_1" -> will be written as run-01

## ----eval=FALSE---------------------------------------------------------------
# glassbox(one_file_with_all_runs, load_asc = list(block = "auto")) |>
#   epoch(
#     events = "TST_trial-{trial}_{item}_{associate}",
#     limits = c(0, 0.1),
#     label  = "trialEpochs"
#   ) |>
#   bidsify(
#     bids_dir       = output_dir,
#     participant_id = "AB01",
#     session_num    = "01",
#     task_name      = "assocmem"
#   )

## -----------------------------------------------------------------------------
citation("eyeris")

