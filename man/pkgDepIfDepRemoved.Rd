% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkgDep.R
\name{pkgDepIfDepRemoved}
\alias{pkgDepIfDepRemoved}
\title{Package dependencies when one or more packages removed}
\usage{
pkgDepIfDepRemoved(pkg = character(), depsRemoved = character())
}
\arguments{
\item{pkg}{A package name to be testing the dependencies}

\item{depsRemoved}{A vector of package names who are to be "removed"
from the \code{pkg} immediate dependencies}
}
\value{
A character vector of the packages that would removed from recursive dependencies
of \code{pkg}
if \code{depsRemoved} were removed from first level dependencies
}
\description{
This is primarily for package developers.
It allows the testing of what the recursive dependencies would be if a package was removed
from the immediate dependencies.
}
\examples{
\dontrun{
pkgDepIfDepRemoved("Require", "remotes")
}
}
