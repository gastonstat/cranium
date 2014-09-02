\name{pkg_parser}
\alias{pkg_parser}
\title{R Package Parser}
\usage{
  pkg_parser(pkg_name)
}
\arguments{
  \item{pkg_name}{name of an R package in CRAN}
}
\value{
  HTML parsed content
}
\description{
  Convenient wrapper to parse the html content of an R
  package page in CRAN
}
\examples{
\dontrun{
  # example with R package 'plspm'
  plspm_url = "http://cran.r-project.org/web/packages/plspm/index.html"
  plspm_doc = pkg_parser(plspm_url)
}
}
\keyword{internal}

