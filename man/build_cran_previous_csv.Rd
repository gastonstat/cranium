\name{build_cran_previous_csv}
\alias{build_cran_previous_csv}
\title{Build data table 'cran_previous.csv'}
\usage{
  build_cran_previous_csv(pkg_names, file = "")
}
\arguments{
  \item{pkg_names}{character vector with names of packages}

  \item{file}{name of the destination file to write the csv
  table}
}
\value{
  csv file with scraped data. The fields are \code{Name},
  \code{File}, \code{Date}, \code{Time} and \code{Size}
}
\description{
  Workhorse function for crawling Previous Versions of R
  packages in CRAN
}
\examples{
\dontrun{
  # some packages in CRAN
  pkgs = c("knitr", "plyr", "plspm")

  # build table and save results in working directory
  build_previous_csv(pkgs, file = "previous.csv")
}
}

