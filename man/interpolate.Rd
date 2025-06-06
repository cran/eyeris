% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline-interpolate.R
\name{interpolate}
\alias{interpolate}
\title{Interpolate missing pupil samples}
\usage{
interpolate(eyeris, verbose = TRUE)
}
\arguments{
\item{eyeris}{An object of class \code{eyeris} dervived from \code{\link[=load_asc]{load_asc()}}.}

\item{verbose}{A flag to indicate whether to print detailed logging messages.
Defaults to \code{TRUE}. Set to \code{FALSE} to suppress messages about the current
processing step and run silently.}
}
\value{
An \code{eyeris} object with a new column in \code{timeseries}:
\verb{pupil_raw_\{...\}_interpolate}.
}
\description{
Linear interpolation of time series data. The intended use of this method
is for filling in missing pupil samples (NAs) in the time series. This method
uses "na.approx()" function from the zoo package, which implements linear
interpolation using the "approx()" function from the stats package.
Currently, NAs at the beginning and the end of the data are replaced with
values on either end, respectively, using the "rule = 2" argument in the
\code{approx()} function.
}
\details{
This function is automatically called by \code{glassbox()} by default. Use
\code{glassbox(interpolate = FALSE)} to disable this step as needed.

Users should prefer using \code{glassbox()} rather than invoking this function
directly unless they have a specific reason to customize the pipeline
manually.
}
\note{
This function is part of the \code{glassbox()} preprocessing pipeline and is not
intended for direct use in most cases. Use \code{glassbox(interpolate = TRUE)}.

Advanced users may call it directly if needed.
}
\examples{
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  # set to FALSE to skip (not recommended)
  eyeris::glassbox(interpolate = TRUE) |>
  plot(seed = 0)

}
\seealso{
\code{\link[=glassbox]{glassbox()}} for the recommended way to run this step as
part of the full eyeris glassbox preprocessing pipeline.
}
