% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getters.R
\name{get_sample_info}
\alias{get_sample_info}
\title{Get the sample information}
\usage{
get_sample_info(exp)
}
\arguments{
\item{exp}{The \link{Experiment} object.}
}
\value{
The sample information tibble.
}
\description{
Get the sample information from an \link{Experiment} object.
}
\details{
A copy, instead of the original tibble, will be returned.
This prevents the original tibble from being altered.
Alteration of data in an \link{Experiment} object is strictly managed
by the class, so that the expression matrix, sample information,
and variable information are always consistent.
}
