## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.dpi = 300
)

## ----eval=FALSE---------------------------------------------------------------
# eye <- eye |>
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
# bidsify(
#   eyeris = eye_1c,
#   bids_dir = tempdir(), # Replace with preferred path, like "~/Documents/eyeris"
#   participant_id = "001",
#   session_num = "01",
#   task_name = "assocmem",
#   run_num = "01",
#   save_raw = TRUE, # Also save raw timeseries
#   html_report = TRUE, # Generate an interactive preproc summary report document
#   report_seed = 0 # Make randomly selected plot epochs reproducible across runs
# )

## ----echo=FALSE, out.width='75%'----------------------------------------------
img_url <- paste0("https://github.com/shawntz/eyeris/raw/dev/inst/",
                  "figures/report_example_annotated-1.png")
knitr::include_graphics(img_url)

## ----echo=FALSE, out.width='100%'---------------------------------------------
gif_url <- paste0("https://github.com/shawntz/eyeris/raw/dev/inst/",
                  "figures/interactive-reports-demo.gif")
knitr::include_graphics(gif_url)

## ----eval=FALSE---------------------------------------------------------------
# plot_gaze_heatmap(
#   eyeris = eyeris_data$timeseries$block_1,
#   screen_width = eyeris_data$info$screen.x,
#   screen_height = eyeris_data$info$screen.y,
#   n_bins = 50,
#   col_palette = "viridis"
# )

## -----------------------------------------------------------------------------
citation("eyeris")

