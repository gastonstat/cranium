\name{split_pkg_attr}
\alias{split_pkg_attr}
\title{Split Package Attribute}
\usage{
  split_pkg_attr(pkg_attr)
}
\arguments{
  \item{pkg_attr}{character string with the contents of an
  R package attribute (eg Name, Author)}
}
\value{
  character vector with split content
}
\description{
  Splits a single attribute by removing commas
}
\details{
  This function is used to create the JSON file for package
  dependencies and reverse dependencies
}
\examples{
\dontrun{
  plspm_depends = c("amap, diagram, tester, turner")
  split_pkg_attr(plspm_depends)
}
}
\keyword{internal}

