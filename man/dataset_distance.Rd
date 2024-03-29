% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sperrorest_misc.R
\name{dataset_distance}
\alias{dataset_distance}
\title{Calculate mean nearest-neighbour distance between point datasets}
\usage{
dataset_distance(
  d1,
  d2,
  x_name = "x",
  y_name = "y",
  fun = mean,
  method = "euclidean",
  ...
)
}
\arguments{
\item{d1}{a \code{data.frame} with (at least) columns with names given by \code{x_name}
and \code{y_name}; these contain the x and y coordinates, respectively.}

\item{d2}{see \code{d1}  - second set of points}

\item{x_name}{name of column in \code{d1} and \code{d2} containing the x coordinates of
points.}

\item{y_name}{same for y coordinates}

\item{fun}{function to be applied to the vector of nearest-neighbor
distances of \code{d1} from \code{d2}.}

\item{method}{type of distance metric to be used; only \code{'euclidean'} is
currently supported.}

\item{...}{additional arguments to \code{fun}.}
}
\value{
depends on \code{fun}; typically (e.g., \code{mean}) a numeric vector
of length 1
}
\description{
\code{dataset_distance} calculates Euclidean nearest-neighbour
distances between two point datasets and summarizes these distances using
some function, by default the mean.
}
\details{
Nearest-neighbour distances are calculated for each point in \code{d1},
resulting in a vector of length \code{nrow(d1)}, and \code{fun} is applied to this
vector.
}
\examples{
df <- data.frame(x = rnorm(100), y = rnorm(100))
dataset_distance(df, df) # == 0
}
\seealso{
\link{add.distance}
}
