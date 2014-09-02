\name{download_previous_pkg}
\alias{download_previous_pkg}
\title{Download HTML copy of an R package previous versions}
\usage{
  download_previous_pkg(package = "", file = "")
}
\arguments{
  \item{package}{name of a package.}

  \item{file}{character string with the name where the
  downloaded file is saved.}
}
\description{
  The HTML content of R package Archive is downloaded to
  the specified file.
}
\examples{
\dontrun{
  # download a copy of 'plspm' to your working directory
  download_previous_pkg("plspm", file = "plspm_previous.html")
}
}
\seealso{
  \code{\link{download.file}},
  \code{\link{download_previous_cran}}
}

