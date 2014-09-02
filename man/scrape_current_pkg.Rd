\name{scrape_current_pkg}
\alias{scrape_current_pkg}
\title{Scrape Attributes of Current R Package Version}
\usage{
  scrape_current_pkg(pkg_name)
}
\arguments{
  \item{pkg_name}{name of an R package in CRAN (current
  version)}
}
\value{
  list with package attributes. If an attribute is empty,
  the returned value is \code{NA}
}
\description{
  Given a package name, extracts the attributes from the
  current version description
}
\examples{
\dontrun{
  # extract list of attributes
  plspm_attrs = scrape_current_pkg("plspm")
}
}

