% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess.R
\name{preprocess_optional}
\alias{preprocess_optional}
\title{Optional Preprocessing}
\usage{
preprocess_optional(
  dataset,
  target,
  reduce_variables = NULL,
  variance_threshold = 1,
  reduce_target = NULL,
  reduce_target_n = 0
)
}
\arguments{
\item{dataset}{A dataframe.}
}
\value{
A cleaned dataset.
}
\description{
can currently remove variables with low variance and small classes of the target
}
\details{
TODO: Encoding, Scaling, Resample (extra step?)
TODO: each step as own function, combine in "preprocess_optional"
}
