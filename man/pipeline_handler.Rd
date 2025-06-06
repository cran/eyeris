% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pipeline-handler.R
\name{pipeline_handler}
\alias{pipeline_handler}
\title{Build a generic operation (extension) for the \code{eyeris} pipeline}
\usage{
pipeline_handler(eyeris, operation, new_suffix, ...)
}
\arguments{
\item{eyeris}{An object of class \code{eyeris} containing timeseries data
in a list of dataframes (one per block), various metadata collected
by the tracker, and \code{eyeris} specific pointers for tracking the
preprocessing history for that specific instance of the \code{eyeris} object.}

\item{operation}{The name of the function to apply to the timeseries data.
This custom function should accept a dataframe \code{x}, a string \code{prev_op}
(i.e., the name of the previous pupil column -- which you DO NOT need to
supply as a literal string as this is inferred from the \code{latest} pointer
within the \code{eyeris} object), and any custom parameters you would like.}

\item{new_suffix}{A chracter string indicating the suffix you would like
to be appended to the name of the previous operation's column, which will
be used for the new column name in the updated preprocessed dataframe(s).}

\item{...}{Additional (optional) arguments passed to the \code{operation} method.}
}
\value{
An updated \code{eyeris} object with the new column added to the
\code{timeseries} dataframe and the \code{latest} pointer updated to the name of the
most recently added column plus all previous columns (ie, the history "trace"
of preprocessing steps from start-to-present).
}
\description{
\code{pipeline_handler} enables flexible integration of custom data
processing functions into the \code{eyeris} pipeline. Under the hood,
each preprocessing function in \code{eyeris} is a wrapper around a
core operation that gets tracked, versioned, and stored using this
\code{pipeline_handler} method. As such, custom pipeline steps must conform
to the \code{eyeris} protocol for maximum compatibility with the downstream
functions we provide.
}
\details{
Following the \code{eyeris} protocol also ensures:
\itemize{
\item all operations follow a predictable structure, and
\item that new pupil data columns based on previous operations in the
chain are able to be dynamically constructed within the core
timeseries data frame.
}
}
\examples{
# first, define your custom data preprocessing function
winsorize_pupil <- function(x, prev_op, lower = 0.01, upper = 0.99) {
  vec <- x[[prev_op]]
  q <- quantile(vec, probs = c(lower, upper), na.rm = TRUE)
  vec[vec < q[1]] <- q[1]
  vec[vec > q[2]] <- q[2]
  vec
}

# second, construct your `pipeline_handler` method wrapper
winsorize <- function(eyeris, lower = 0.01, upper = 0.99) {
  pipeline_handler(
    eyeris,
    winsorize_pupil,
    "winsorize",
    lower = lower,
    upper = upper
  )
}

# and voilà, you can now connect your custom extension
# directly into your custom `eyeris` pipeline definition!
custom_eye <- system.file("extdata", "memory.asc", package = "eyeris") |>
  eyeris::load_asc(block = "auto") |>
  eyeris::deblink(extend = 50) |>
  winsorize()

plot(custom_eye, seed = 1)

}
\seealso{
For more details, please check out the following vignettes:
\itemize{
\item Anatomy of an eyeris Object
}

\code{vignette("anatomy", package = "eyeris")}
\itemize{
\item Building Your Own Custom Pipeline Extensions
}

\code{vignette("custom-extensions", package = "eyeris")}
}
