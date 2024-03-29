% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{lcurry}
\alias{lcurry}
\title{Pre-Set Function Parameters}
\usage{
lcurry(FUN, ...)
}
\arguments{
\item{FUN}{the function to pre-set parameters for}

\item{...}{parameters to pre-set}
}
\value{
\code{FUN} wrapped with pre-set parameters
}
\description{
Create a new function from an existing function, but with parameters pre-set.
This is a function intended for testing to simplify complex expressions
involving the \verb{_exec} functions.  It merely stores the function expression to
execute in the lexical environment it was created in.  All symbols will be
resolved at evaluation time, not at creation time.
}
\details{
This is inspired by a function originally from Byron Ellis, adapted by Jamie
F Olson, and discovered by me via Peter Danenberg's \code{{functional}} (see
packages \code{?functional::Curry} and \code{functional::CurryL}).  The implementation
here is different, in particular it makes it easy to see what the intended
call is by displaying the function contents (see examples).
}
\examples{
sum_nona <- lcurry(sum, na.rm=TRUE)
sum_nona(c(1, NA, 2))
sum_nona
}
