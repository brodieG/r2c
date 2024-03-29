% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/group.R
\name{process_groups}
\alias{process_groups}
\title{Compute Group Meta Data}
\usage{
process_groups(groups, sorted = FALSE)
}
\arguments{
\item{groups}{an integer, numeric, or factor vector.  Alternatively, a list
of equal-length such vectors, the interaction of which defines individual
groups to organize the vectors in \code{data} into (multiple vectors not
implemented yet).  Numeric vectors are coerced to integer, thus copied.
Vectors of integer type, but with different classes/attributes (other than
factors) will be treated as integer vectors.  The vectors must be the
same length as those in \code{data}.  NA values are considered one group. If a
list, the result of the calculation will be returned as a "data.frame",
otherwise as a named vector.  Currently only one group vector is allowed,
even when using list mode.  Support for multiple group vectors and other
types of vectors will be added in the future.  Zero length groups are not
computed on at all (e.g. missing factor levels, zero-length group vector).}

\item{sorted}{TRUE or FALSE (default), whether the vectors in \code{groups} are
already sorted.  If set to TRUE, no sorting will be done on the groups, nor
later on the \code{data} by \code{\link{group_exec}}. If the data is truly sorted this
produces the same results while avoiding the cost of sorting.  If the data
is not sorted by groups, \code{g} will produce groups corresponding to
equal-value runs it contains, which might be useful in some circumstances.}
}
\value{
an "r2c.groups" object, which is a list containing group sizes,
labels, and group count, along with other meta data such as the group
ordering vector.
}
\description{
\code{\link{group_exec}} sorts data by groups prior to iterating through them.  When
running \code{group_exec} multiple times on the same data, it is better to
pre-sort the data and tell \code{group_exec} as much so it does  not sort the data
again.  We can do the latter with \code{process_groups}, which additionally
computes group information we can re-use across calls.
}
\note{
The structure and content of the return value may change in the future.
}
\examples{
## Use same group data for different but same length data.
## (alternatively, could use two functions on same data).
n <- 10
dat <- data.frame(x=runif(n), y=runif(n), g=sample(1:3, n, replace=TRUE))

## Pre-sort by group and compute grouping data
dat <- dat[order(dat[['g']]),]
g.r2c <- process_groups(dat[['g']], sorted=TRUE) # note sorted=TRUE

## Re-use pre-computed group data
f <- r2cq(sum(x))
with(dat, group_exec(f, x, groups=g.r2c))
with(dat, group_exec(f, y, groups=g.r2c))

## Claim unsorted data is sorted to implement RLE
g <- c(1, 2, 2, 1, 1, 1, 2, 2)
group_exec(f, rep(1, length(g)), process_groups(g, sorted=TRUE))
rle(g)$values
rle(g)$lengths
}
\seealso{
\code{\link{group_exec}}
}
