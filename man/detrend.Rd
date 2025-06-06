% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline-detrend.R
\name{detrend}
\alias{detrend}
\title{Detrend the pupil time series}
\usage{
detrend(eyeris)
}
\arguments{
\item{eyeris}{An object of class \code{eyeris} dervived from \code{\link[=load_asc]{load_asc()}}.}
}
\value{
An \code{eyeris} object with two new columns in \code{timeseries}:
\code{detrend_fitted_betas}, and \verb{pupil_raw_\{...\}_detrend}.
}
\description{
Linearly detrend_pupil data by fitting a linear model of \code{pupil_data ~ time},
and return the fitted betas and the residuals (\code{pupil_data - fitted_values}).
}
\details{
This function is automatically called by \code{glassbox()} if \code{detrend = TRUE}.

Users should prefer using \code{glassbox()} rather than invoking this function
directly unless they have a specific reason to customize the pipeline
manually.
}
\note{
This function is part of the \code{glassbox()} preprocessing pipeline and is not
intended for direct use in most cases. Use \code{glassbox(detrend = TRUE)}.

Advanced users may call it directly if needed.
}
\examples{
demo_data <- eyelink_asc_demo_dataset()

demo_data |>
  eyeris::glassbox(detrend = TRUE) |>  # set to FALSE to skip step (default)
  plot(seed = 0)

}
\seealso{
\code{\link[=glassbox]{glassbox()}} for the recommended way to run this step as
part of the full eyeris glassbox preprocessing pipeline.
}
