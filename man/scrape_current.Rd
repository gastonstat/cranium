\name{scrape_current}
\alias{scrape_current}
\title{Scrape Attributes of Current R Package Version}
\usage{
  scrape_current(package_doc)
}
\arguments{
  \item{package_doc}{package HTML document with Description
  of an R package's current version}
}
\value{
  list with package attributes. If an attribute is empty,
  the returned value is \code{NA}
}
\description{
  Extracts the attributes from the html content of a
  current R package version
}
\details{
  This function is called by
  \code{\link{scrape_current_pkg}} and
  \code{\link{scrape_current_pkg_html}}
}
\keyword{internal}

