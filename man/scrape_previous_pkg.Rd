\name{scrape_previous_pkg}
\alias{scrape_previous_pkg}
\title{Data Frame with the previous versions of an R package}
\usage{
  scrape_previous_pkg(pkg_name)
}
\arguments{
  \item{pkg_name}{name of an R package in CRAN}
}
\value{
  data frame with five columns: \code{"Name"},
  \code{"File"}, \code{"Date"}, \code{"Time"}, and
  \code{"Size"}
}
\description{
  Extracts HTML contents of an R package's previous
  versions and returns a data frame with cleaned
  information
}
\details{
  This function returns a data frame with the scraped
  contents of the original HTML table.
}
\examples{
\dontrun{
  # extract cleaned HTML table in a data frame
  prev_table = scrape_previous_pkg("plspm")
}
}
\seealso{
  \code{\link{scrape_current_pkg}}
}

