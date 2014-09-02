\name{previous_pkg_df}
\alias{previous_pkg_df}
\title{Data Frame of R an package previous versions}
\usage{
  previous_pkg_df(html_table)
}
\arguments{
  \item{html_table}{an HTML table}
}
\value{
  data frame with five columns: \code{"Name"},
  \code{"File"}, \code{"Date"}, \code{"Time"}, and
  \code{"Size"}
}
\description{
  Extracts HTML contents of an R package previous versions
  and returns a data frame with cleaned information
}
\details{
  This function returns a data frame with the scraped
  contents of the original HTML table.
}
\examples{
\dontrun{
  # Previous versions of package "plspm"
  plspm_previous = previous_pkg_html_table("plspm")
  plspm_previous_df = previous_pkg_df(plspm_previous)

  plspm_previous_df
}
}

