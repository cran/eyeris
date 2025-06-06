% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline-loadasc.R
\name{load_asc}
\alias{load_asc}
\title{Load and parse SR Research EyeLink \code{.asc} files}
\usage{
load_asc(file, block = "auto")
}
\arguments{
\item{file}{An SR Research EyeLink \code{.asc} file generated by the official
EyeLink \code{edf2asc} command.}

\item{block}{Optional block number specification. The following are valid
options:
\itemize{
\item "auto" (default): Automatically handles multiple recording segments
embedded within the same \code{.asc} file. We recommend using this default as
this is likely the safer choice then assuming a single-block recording
(unless you know what you're doing).
\item \code{NULL}: Omits block column. Suitable for single-block recordings.
\item Numeric value: Manually sets block number based on the value provided
here.
}}
}
\value{
An object of S3 class \code{eyeris} with the following attributes:
\enumerate{
\item \code{file}: Path to the original \code{.asc} file.
\item \code{timeseries}: Dataframe of all raw timeseries data from the tracker.
\item \code{events}: Dataframe of all event messages and their timestamps.
\item \code{blinks}: Dataframe of all blink events.
\item \code{info}: Dataframe of various metadata parsed from the file header.
\item \code{latest}: \code{eyeris} variable for tracking pipeline run history.
}
}
\description{
This function builds upon the \code{\link[eyelinker:read.asc]{eyelinker::read.asc()}} function to parse the
messages and metadata within the EyeLink \code{.asc} file. After loading and
additional processing, this function returns an S3 \code{eyeris} class for use in
all subsequent \code{eyeris} pipeline steps and functions.
}
\details{
This function is automatically called by \code{glassbox()} by default. If needed,
customize the parameters for \code{load_asc} by providing a parameter list.

Users should prefer using \code{glassbox()} rather than invoking this function
directly unless they have a specific reason to customize the pipeline
manually.
}
\note{
This function is part of the \code{glassbox()} preprocessing pipeline and is not
intended for direct use in most cases. Provide parameters via
\code{load_asc = list(...)}.

Advanced users may call it directly if needed.
}
\examples{
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox(load_asc = list(block = 1))

# Other useful parameter configurations
## (1) Basic usage (no block column specified)
demo_data |>
  eyeris::load_asc()

## (2) Manual specification of block number
demo_data |>
  eyeris::load_asc(block = 3)

## (3) Auto-detect multiple recording segments embedded within the same file
##  (i.e., the default behavior)
demo_data |>
  eyeris::load_asc(block = "auto")

## (4) Omit block column
demo_data |>
  eyeris::load_asc(block = NULL)

}
\seealso{
\code{\link[eyelinker:read.asc]{eyelinker::read.asc()}} which this function wraps.

\code{\link[=glassbox]{glassbox()}} for the recommended way to run this step as
part of the full eyeris glassbox preprocessing pipeline.
}
