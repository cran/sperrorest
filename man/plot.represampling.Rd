% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sperrorest_resampling.R
\name{plot.represampling}
\alias{plot.represampling}
\alias{plot.resampling}
\title{Plot spatial resampling objects}
\usage{
\method{plot}{represampling}(x, data, coords = c("x", "y"), pch = "+", wiggle_sd = 0, ...)

\method{plot}{resampling}(x, ...)
}
\arguments{
\item{x}{a \link{represampling} resp. \link{resampling} object.}

\item{data}{a \code{data.frame} of samples containing at least the x and y
coordinates of samples as specified by \code{coords}.}

\item{coords}{vector of length 2 defining the variables in \code{data} that
contain the x and y coordinates of sample locations.}

\item{pch}{point symbol (to be passed to \link{points}).}

\item{wiggle_sd}{'wiggle' the point locations in x and y direction to avoid
overplotting of samples drawn multiple times by bootstrap methods; this is
a standard deviation (in the units of the x/y coordinates) of a normal
distribution and defaults to 0 (no wiggling).}

\item{...}{additional arguments to \link{plot}.}
}
\description{
\code{plot.represampling} displays the partitions or samples
corresponding arising from the resampling of a data set.
}
\note{
This function is not intended for samples obtained by resampling with
replacement (e.g., bootstrap) because training and test points will be
overplotted in that case. The size of the plotting region will also limit
the number of maps that can be displayed at once, i.e., the number of rows
(repetitions) and fields (columns).
}
\examples{
data(ecuador)
# non-spatial cross-validation:
resamp <- partition_cv(ecuador, nfold = 5, repetition = 1:2)
# plot(resamp, ecuador)
# spatial cross-validation using k-means clustering:
resamp <- partition_kmeans(ecuador, nfold = 5, repetition = 1:2)
# plot(resamp, ecuador)
}
