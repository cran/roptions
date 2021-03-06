% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/call_vega.R
\name{call.vega}
\alias{call.vega}
\title{Call Vega}
\usage{
call.vega(s, k, t, sd, r, d = 0)
}
\arguments{
\item{s}{Spot Price of Underlying Asset}

\item{k}{Exercise Price of Contract}

\item{t}{Time to Expiration}

\item{sd}{Volatality}

\item{r}{Risk free rate of return}

\item{d}{Divident Yield (use cont.rate()), Default: 0}
}
\value{
Output gives the Vega of a Option Contract.
}
\description{
Calculate the Vega (Option Greek) of Option Contract
}
\details{
Vega represents the rate of change between an option's value and the underlying asset's implied volatility.
}
\examples{
call.vega(100, 105, 0.25, 0.35, 0.0488)
}
