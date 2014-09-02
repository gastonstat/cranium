\name{build_current_csv}
\alias{build_current_csv}
\title{Build data table 'cran_packages.csv'}
\usage{
  build_current_csv(pkg_names, file = "")
}
\arguments{
  \item{pkg_names}{character vector with names of packages}

  \item{file}{name of the destination file to write table}
}
\value{
  csv file with scraped data
}
\description{
  Workhorse function for crawling Current Versions of R
  packages in CRAN
}
\examples{
\dontrun{
  # some packages in CRAN
  pkgs = c("knitr", "plyr", "plspm")

  # build table and save results in working directory
  build_cran_current_csv(pkgs, file = "current.csv")
}
}

