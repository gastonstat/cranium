\name{download_current_cran}
\alias{download_current_cran}
\title{Download HTML copy of Current R Packages in CRAN}
\usage{
  download_current_cran(file = "")
}
\arguments{
  \item{file}{character string with the name where the
  downloaded file is saved.}
}
\description{
  The HTML content of the "Available CRAN Packages By Name"
  is downloaded to the specified file.
}
\examples{
\dontrun{
  # download a copy to your working directory
  download_current_cran("current_cran.html")
}
}
\seealso{
  \code{\link{download.file}},
  \code{\link{download_previous_cran}}
}

