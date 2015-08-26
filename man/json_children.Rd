\name{json_children}
\alias{json_children}
\title{JSON Children}
\usage{
  json_children(pkg_attribute)
}
\arguments{
  \item{pkg_attribute}{character string with the contents
  of an R package attribute (eg Name, Author)}
}
\value{
  character vector with split content
}
\description{
  Converts the string of a package attribute into a JSON
  children array
}
\details{
  This function is used to create the JSON file for package
  dependencies and reverse dependencies
}
\examples{
\dontrun{
  plspm_depends = c("amap, diagram, tester, turner")
  json_children(plspm_depends)
}
}
\keyword{internal}

