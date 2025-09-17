## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, include=FALSE-----------------------------------------------------
# library(eyeris)

## ----basic-export-------------------------------------------------------------
# result <- eyeris_db_to_chunked_files(
#   bids_dir = "/path/to/your/bids/directory",
#   db_path = "my-project"  # your database name
# )
# 
# # view what was exported
# print(result)

## ----file-size-control--------------------------------------------------------
# # Create smaller files for easy distribution
# result <- eyeris_db_to_chunked_files(
#   bids_dir = "/path/to/bids",
#   db_path = "large-project",
#   max_file_size_mb = 100,    # 100MB files instead of 500MB
#   chunk_size = 500000        # Process 500k rows at a time
# )

## ----selective-export---------------------------------------------------------
# # Export only pupil timeseries and events
# result <- eyeris_db_to_chunked_files(
#   bids_dir = "/path/to/bids",
#   db_path = "large-project",
#   data_types = c("timeseries", "events"),
#   subjects = c("sub-001", "sub-002", "sub-003")  # Specific subjects only
# )

## ----parquet-export-----------------------------------------------------------
# result <- eyeris_db_to_chunked_files(
#   bids_dir = "/path/to/bids",
#   db_path = "large-project",
#   file_format = "parquet",
#   max_file_size_mb = 200
# )

## ----read-files---------------------------------------------------------------
# # Read a single CSV file
# data <- read.csv("path/to/timeseries_chunked.csv")
# 
# # Read a single Parquet file (requires arrow package)
# if (requireNamespace("arrow", quietly = TRUE)) {
#   data <- arrow::read_parquet("path/to/timeseries_chunked.parquet")
# }

## ----combine-files------------------------------------------------------------
# # Find all parts of a split dataset
# files <- list.files(
#   "path/to/eyerisdb_export/my-project/",
#   pattern = "timeseries_chunked_.*\\.csv$",
#   full.names = TRUE
# )
# 
# # Read and combine all parts
# combined_data <- do.call(rbind, lapply(files, read.csv))
# 
# # Or use the built-in helper function
# combined_data <- read_eyeris_parquet(
#   parquet_dir = "path/to/eyerisdb_export/my-project/",
#   data_type = "timeseries"
# )

## ----custom-processing--------------------------------------------------------
# # Connect to database directly
# con <- eyeris_db_connect("/path/to/bids", "large-project")
# 
# # Define custom analysis function for pupil data
# analyze_chunk <- function(chunk) {
#   # Calculate summary statistics for this chunk
#   stats <- data.frame(
#     n_rows = nrow(chunk),
#     subjects = length(unique(chunk$subject_id)),
#     mean_eye_x = mean(chunk$eye_x, na.rm = TRUE),
#     mean_eye_y = mean(chunk$eye_y, na.rm = TRUE),
#     mean_pupil_raw = mean(chunk$pupil_raw, na.rm = TRUE),
#     mean_pupil_processed = mean(chunk$pupil_raw_deblink_detransient_interpolate_lpfilt_z, na.rm = TRUE),
#     missing_pupil_pct = sum(is.na(chunk$pupil_raw)) / nrow(chunk) * 100,
#     hz_modes = paste(unique(chunk$hz), collapse = ",")
#   )
# 
#   # Save chunk summary (append to growing file)
#   write.csv(stats, "chunk_summaries.csv", append = file.exists("chunk_summaries.csv"))
# 
#   return(TRUE)  # Indicate success
# }
# 
# # Hypothetical example: process large timeseries dataset in chunks
# result <- process_chunked_query(
#   con = con,
#   query = "
#     SELECT subject_id, session_id, time_secs, eye_x, eye_y,
#            pupil_raw, pupil_raw_deblink_detransient_interpolate_lpfilt_z, hz
#     FROM timeseries_01_enc_clamp_run01
#     WHERE pupil_raw > 0 AND eye_x IS NOT NULL
#     ORDER BY time_secs
#   ",
#   chunk_size = 100000,
#   process_chunk = analyze_chunk
# )
# 
# eyeris_db_disconnect(con)

## ----very-large---------------------------------------------------------------
# # Optimize for very large datasets
# result <- eyeris_db_to_chunked_files(
#   bids_dir = "/path/to/bids",
#   db_path = "massive-project",
#   chunk_size = 2000000,        # 2M rows per chunk for efficiency
#   max_file_size_mb = 1000,     # 1GB files (larger but fewer files)
#   file_format = "parquet",     # Better compression
#   data_types = "timeseries"    # Focus on primary data type for analysis
# )

## ----memory-fix---------------------------------------------------------------
# # Reduce chunk size
# result <- eyeris_db_to_chunked_files(
#   bids_dir = "/path/to/bids",
#   db_path = "project",
#   chunk_size = 250000,  # Smaller chunks
#   verbose = TRUE        # Monitor progress
# )

