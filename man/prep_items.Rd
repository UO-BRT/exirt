% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{prep_items}
\alias{prep_items}
\title{Function for selecting and prepping item data}
\usage{
prep_items(test)
}
\arguments{
\item{test}{One specific test from \code{dbprocess::get_items()}}
}
\value{
A data frame for the specific test with only items and all missing
values recoded to zero.
}
\description{
Function for selecting and prepping item data
}
\examples{
\dontrun{
g3mth <- dbprocess::get_items(3, "Math")
items <- orextdb::db_get("Items")
prepped <- prep_items(g3mth, items)

# check that there's no missing data
any(apply(prepped, 2, function(x) any(is.na(x))))
}
}
\keyword{internal}
