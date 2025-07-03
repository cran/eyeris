
<!-- README.md is generated from README.Rmd-->

# `eyeris`: Flexible, Extensible, & Reproducible Pupillometry Preprocessing <a href="https://shawnschwartz.com/eyeris/" title="eyeris website"><img src="man/figures/logo.png" align="right" width="100" alt="eyeris website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/eyeris)](https://CRAN.R-project.org/package=eyeris)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![build](https://github.com/shawntz/eyeris/actions/workflows/build.yml/badge.svg)](https://github.com/shawntz/eyeris/actions/workflows/build.yml)
[![linter](https://github.com/shawntz/eyeris/actions/workflows/linter.yml/badge.svg)](https://github.com/shawntz/eyeris/actions/workflows/linter.yml)
[![pkgdown](https://github.com/shawntz/eyeris/actions/workflows/pkgdown.yml/badge.svg)](https://github.com/shawntz/eyeris/actions/workflows/pkgdown.yml)
[![bioRxiv
Preprint](https://img.shields.io/badge/bioRxiv_Preprint-DOI-brightgreen)](https://doi.org/10.1101/2025.06.01.657312)
<!-- badges: end -->

<!-- The goal of eyeris is to ... -->

## Motivation

Despite decades of pupillometry research, many established packages and
workflows unfortunately lack design principles based on (F)indability
(A)ccessbility (I)nteroperability (R)eusability (FAIR) principles.
`eyeris`, on the other hand follows a thoughtful design philosophy that
results in an intuitive, modular, performant, and extensible
pupillometry data preprocessing framework. Much of these design
principles were heavily inspired by `Nipype`.

`eyeris` also provides a highly opinionated pipeline for tonic and
phasic pupillometry preprocessing (inspired by `fMRIPrep`). These
opinions are the product of many hours of discussions from core members
and signal processing experts from the Stanford Memory Lab (Shawn
Schwartz, Mingjian He, Haopei Yang, Alice Xue, and Anthony Wagner).

`eyeris` also introduces a `BIDS`-like structure for organizing
derivative (preprocessed) pupillometry data, as well as an intuitive
workflow for inspecting preprocessed pupillometry epochs within
beautiful, interactive HTML report files (see demonstration below ⬇️)!
The package also includes gaze heatmaps that show the distribution of
eye coordinates across the entire screen area, helping you assess data
quality and participant attention patterns. These heatmaps are
automatically generated in the BIDS reports and can also be created
manually.

<img src="https://github.com/shawntz/eyeris/raw/dev/inst/figures/interactive-reports-demo.gif" width="100%" />

## Installation

### stable release from CRAN

You can install the stable release of [`eyeris` from
CRAN](https://cran.r-project.org/package=eyeris) with:

``` r
install.packages("eyeris")
```

or

``` r
# install.packages("pak")
pak::pak("eyeris")
```

### development version from GitHub

You can install the development version of [`eyeris` from
GitHub](https://github.com/shawntz/eyeris) with:

``` r
# install.packages("devtools")
devtools::install_github("shawntz/eyeris", ref = "dev")
```

## Example

### the `glassbox()` “prescription” function

This is a basic example of how to use `eyeris` out of the box with our
very *opinionated* set of steps and parameters that one should start out
with when preprocessing pupillometry data. Critically, this is a
“glassbox” – as opposed to a “blackbox” – since each step and parameter
implemented herein is fully open and accessible to you. We designed each
pipeline step / function to be like legos – they are intentionally and
carefully designed in a way that allows you to flexibly construct and
compare different pipelines.

We hope you enjoy! -shawn

``` r
set.seed(32)

library(eyeris)
#> 
#> eyeris v2.0.0 - Lumpy Space Princess ꒰•ᴗ•｡꒱۶
#> Welcome! Type ?`eyeris` to get started.

demo_data <- eyelink_asc_demo_dataset()

eyeris_preproc <- glassbox(
  demo_data,
  lpfilt = list(plot_freqz = FALSE)
)
#> ✔ [  OK  ] - Running eyeris::load_asc()
#> ℹ [ INFO ] - Processing block: block_1
#> ✔ [  OK  ] - Running eyeris::deblink() for block_1
#> ✔ [  OK  ] - Running eyeris::detransient() for block_1
#> ✔ [  OK  ] - Running eyeris::interpolate() for block_1
#> ✔ [  OK  ] - Running eyeris::lpfilt() for block_1
#> ! [ SKIP ] - Skipping eyeris::downsample() for block_1
#> ! [ SKIP ] - Skipping eyeris::bin() for block_1
#> ! [ SKIP ] - Skipping eyeris::detrend() for block_1
#> ✔ [  OK  ] - Running eyeris::zscore() for block_1
#> ✔ [  OK  ] - Running eyeris::summarize_confounds()
```

### step-wise correction of pupillary signal

``` r
plot(eyeris_preproc, add_progressive_summary = TRUE)
```

<div style="display: flex; justify-content: center; gap: 20px;">

<img src="https://github.com/shawntz/eyeris/raw/dev/inst/figures/ts_coalesced.gif" width="49%" alt="glassbox timeseries animation"><img src="https://github.com/shawntz/eyeris/raw/dev/inst/figures/hists_coalesced.gif" width="49%" alt="glassbox histograms animation">

</div>

### final pre-post correction of pupillary signal (raw ➡ preprocessed)

``` r
start_time <- min(eyeris_preproc$timeseries$block_1$time_secs)
end_time <- max(eyeris_preproc$timeseries$block_1$time_secs)

plot(eyeris_preproc,
  # steps = c(1, 5), # uncomment to specify a subset of preprocessing steps to plot; by default, all steps will plot in the order in which they were executed by eyeris
  preview_window = c(start_time, end_time),
  add_progressive_summary = TRUE
)
#> ! [ INFO ] - Plotting block 1 from possible blocks: 1
#> ℹ [ INFO ] - Plotting with sampling rate: 1000 Hz
```

<img src="man/figures/README-timeseries-plot-1.png" width="100%" /><img src="man/figures/README-timeseries-plot-2.png" width="100%" /><img src="man/figures/README-timeseries-plot-3.png" width="100%" /><img src="man/figures/README-timeseries-plot-4.png" width="100%" /><img src="man/figures/README-timeseries-plot-5.png" width="100%" /><img src="man/figures/README-timeseries-plot-6.png" width="100%" />

    #> ℹ [ INFO ] - Creating progressive summary plot for block_1

<img src="man/figures/README-timeseries-plot-7.png" width="100%" />

    #> ✔ [  OK  ] - Progressive summary plot created successfully!

    plot_gaze_heatmap(
      eyeris = eyeris_preproc,
      block = 1
    )

<img src="man/figures/README-timeseries-plot-8.png" width="100%" />

## Logging `eyeris` commands with `eyelogger()`

The `eyelogger()` utility lets you run any `eyeris` command (or block of
R code) while automatically capturing all console output and errors to
timestamped log files. This is especially useful for reproducibility,
debugging, or running batch jobs.

**How it works:** - All standard output (`stdout`) and standard error
(`stderr`) are saved to log files in a directory you specify (or a
temporary directory by default). - Each run produces two log files: -
`<timestamp>.out`: all console output - `<timestamp>.err`: all warnings
and errors

### Usage

You can wrap any `eyeris` command or block of code in
`eyelogger({ ... })`:

``` r
library(eyeris)

# log a simple code block with messages, warnings, and prints
eyelogger({
  message("eyeris `glassbox()` completed successfully.")
  warning("eyeris `glassbox()` completed with warnings.")
  print("some eyeris-related information.")
})

# log a real eyeris pipeline run, saving logs to a custom directory
log_dir <- file.path(tempdir(), "eyeris_logs")
eyelogger({
  glassbox(eyelink_asc_demo_dataset(), interactive_preview = FALSE)
}, log_dir = log_dir)
```

### Parameters

- `eyeris_cmd`: The code to run (wrap in `{}` for multiple lines).
- `log_dir`: Directory to save logs (default: a temporary directory).
- `timestamp_format`: Format for log file names (default:
  `"%Y%m%d_%H%M%S"`).

### What you get

After running, you’ll find log files in your specified directory, e.g.:

    20240614_153012.out   # console output
    20240614_153012.err   # warnings and errors

This makes it easy to keep a record of your preprocessing runs and debug
any issues that arise.

------------------------------------------------------------------------

## `eyeris` dependency graph :see_no_evil:

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

------------------------------------------------------------------------

# Contributing to `eyeris`

Thank you for considering contributing to the open-source `eyeris` R
package; there are many ways one could contribute to `eyeris`.

We believe the best preprocessing practices emerge from collective
expertise and rigorous discussion. Please see the [contribution
guidelines](https://shawnschwartz.com/eyeris/CONTRIBUTING.html) for more
information on how to get started..

## Code of Conduct

Please note that the eyeris project is released with a [Contributor Code
of Conduct](https://shawnschwartz.com/eyeris/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.

## Suggestions, questions, issues?

Please use the issues tab (<https://github.com/shawntz/eyeris/issues>)
to make note of any bugs, comments, suggestions, feedback, etc… all are
welcomed and appreciated, thanks!

## 📚 Citing `eyeris`

<div class="alert alert-light" style="padding-bottom: 0;">

If you use the `eyeris` package in your research, please consider citing
our preprint!

Run the following in R to get the citation:

</div>

``` r
citation("eyeris")
#> To cite package 'eyeris' in publications use:
#> 
#>   Schwartz ST, Yang H, Xue AM, He M (2025). "eyeris: A flexible,
#>   extensible, and reproducible pupillometry preprocessing framework in
#>   R." _bioRxiv_, 1-37. doi:10.1101/2025.06.01.657312
#>   <https://doi.org/10.1101/2025.06.01.657312>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {eyeris: A flexible, extensible, and reproducible pupillometry preprocessing framework in R},
#>     author = {Shawn T Schwartz and Haopei Yang and Alice M Xue and Mingjian He},
#>     journal = {bioRxiv},
#>     year = {2025},
#>     pages = {1--37},
#>     doi = {10.1101/2025.06.01.657312},
#>   }
```
