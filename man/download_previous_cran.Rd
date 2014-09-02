\name{download_previous_cran}
\alias{download_previous_cran}
\title{Download HTML copy of Previous R Packages in CRAN}
\usage{
  download_previous_cran(file = "")
}
\arguments{
  \item{file}{character string with the name where the
  downloaded file is saved.}
}
\description{
  The HTML content an R package description is downloaded
  to the specified file.
}
\examples{
\dontrun{
  # download a copy to your working directory
  download_previous_cran("previous_cran.html")
}
}
\seealso{
  \code{\link{download.file}},
  \code{\link{download_current_cran}}
}

