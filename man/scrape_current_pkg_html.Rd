\name{scrape_current_pkg_html}
\alias{scrape_current_pkg_html}
\title{Scrape Attributes of the HTML content containing the
Current R Package Version}
\usage{
  scrape_current_pkg_html(pkg_html)
}
\arguments{
  \item{pkg_html}{package HTML document with Description of
  an R package's current version}
}
\value{
  list with package attributes. If an attribute is empty,
  the returned value is \code{NA}
}
\description{
  Given a package HTML content description, extracts the
  attributes of its current version
}
\examples{
\dontrun{
  # download a copy to your current working directory
  plspm_current = download_current_pkg("plspm", "plspm_current.html")

  # extract list of attributes
  plspm_attrs = scrape_current_pkg_html(plspm_current)
}
}

