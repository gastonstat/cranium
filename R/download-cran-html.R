#' @title Download HTML copy of Current R Packages in CRAN
#' 
#' @description The HTML content of the "Available CRAN Packages By Name" 
#' is downloaded to the specified file.
#' 
#' @param file character string with the name where the downloaded 
#' file is saved.
#' @seealso \code{\link{download.file}}, 
#' \code{\link{download_previous_cran}}
#' @examples
#' \dontrun{
#'   # download a copy to your working directory
#'   download_current_cran("current_cran.html")
#' }
download_current_cran <- function(file = "") 
{
  if (file == "")
    stop("\n'download_current_cran()' requires a destiny file")
  
  # assemble URL of 'Available CRAN packages by name'
  cran_url = "http://cran.r-project.org/"
  by_name = "web/packages/available_packages_by_name.html"    
  cran_packages_url = paste(cran_url, by_name, sep = "")
  
  # download html copy to specified file
  download.file(cran_packages_url, destfile = file)
}


#' @title Download HTML copy of an R Package current version
#' 
#' @description The HTML content of the "Available CRAN Packages By Name" 
#' is downloaded to the specified file.
#' 
#' @param package name of a package.
#' @param file character string with the name where the downloaded 
#' file is saved.
#' @seealso \code{\link{download.file}}, 
#' \code{\link{download_current_cran}}
#' @examples
#' \dontrun{
#'   # download a copy of 'plspm' to your working directory
#'   download_current_pkg("plspm", file = "plspm_current.html")
#' }
download_current_pkg <- function(package = "", file = "") 
{
  if (package == "")
    stop("\n'download_current_pkg()' requires a package name")
  if (file == "")
    stop("\n'download_current_pkg()' requires a destiny file")
  
  # assemble URL of package HTML description to be downloaded
  pkg_url = sprintf("http://cran.r-project.org/web/packages/%s/index.html",
                    package)
    
  # download html copy to specified file
  download.file(pkg_url, destfile = file)
}


#' @title Download HTML copy of Previous R Packages in CRAN
#' 
#' @description The HTML content an R package description  
#' is downloaded to the specified file.
#' 
#' @param file character string with the name where the downloaded 
#' file is saved.
#' @seealso \code{\link{download.file}}, 
#' \code{\link{download_current_cran}}
#' @examples
#' \dontrun{
#'   # download a copy to your working directory
#'   download_previous_cran("previous_cran.html")
#' }
download_previous_cran <- function(file = "") 
{
  if (file == "")
    stop("\n'download_previous_cran()' requires a destiny file")
  
  # URL of 'Old versions of contributed R packages'
  origin  = "http://cran.r-project.org/src/contrib/Archive/"
  # download html copy to specified file
  download.file(origin, destfile = file)
}


#' @title Download HTML copy of an R package previous versions 
#' 
#' @description The HTML content of R package Archive 
#' is downloaded to the specified file.
#' 
#' @param package name of a package.
#' @param file character string with the name where the downloaded 
#' file is saved.
#' @seealso \code{\link{download.file}}, 
#' \code{\link{download_previous_cran}}
#' @examples
#' \dontrun{
#'   # download a copy of 'plspm' to your working directory
#'   download_previous_pkg("plspm", file = "plspm_previous.html")
#' }
download_previous_pkg <- function(package = "", file = "") 
{
  if (package == "")
    stop("\n'download_current_pkg()' requires a package name")
  if (file == "")
    stop("\n'download_current_pkg()' requires a destiny file")
  
  # assemble URL of package HTML archive to be downloaded
  pkg_url = sprintf("http://cran.r-project.org/src/contrib/Archive/%s",
                    package)
  
  # download html copy to specified file
  download.file(pkg_url, destfile = file)
}
