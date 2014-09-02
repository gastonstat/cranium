\name{download_current_pkg}
\alias{download_current_pkg}
\title{Download HTML copy of an R Package current version}
\usage{
  download_current_pkg(package = "", file = "")
}
\arguments{
  \item{package}{name of a package.}

  \item{file}{character string with the name where the
  downloaded file is saved.}
}
\description{
  The HTML content of the "Available CRAN Packages By Name"
  is downloaded to the specified file.
}
\examples{
\dontrun{
  # download a copy of 'plspm' to your working directory
  download_current_pkg("plspm", file = "plspm_current.html")
}
}
\seealso{
  \code{\link{download.file}},
  \code{\link{download_current_cran}}
}

