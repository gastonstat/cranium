\name{build_json_tree}
\alias{build_json_tree}
\title{Build JSON Tree}
\usage{
  build_json_tree(pkg_desc, file = "")
}
\arguments{
  \item{pkg_desc}{list of attributes from a package current
  description (obtained from output in
  \code{\link{scrape_current_pkg}})}

  \item{file}{name of file to write the JSON tree}
}
\description{
  Builds a Tree structure under JSON format representing
  the dependencies and reverse dependencies of an R package
}

