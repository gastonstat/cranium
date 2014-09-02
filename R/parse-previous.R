#' @title URL of a package previous versions
#' 
#' @description Returns the URL of a package previous versions in CRAN
#' @param package name of an R package in CRAN
#' @return complete URL of previous package versions
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   # URL of previous versions of 'plspm'
#'   previous_pkg_url("plspm")
#' }
previous_pkg_url <- function(package) {
  sprintf("http://cran.r-project.org/src/contrib/Archive/%s",
          package)
}


#' @title Parse HTML Table of an R Package Previous Versions
#' 
#' @param pkg_name name of an R package in CRAN
#' @return HTML table of an R package previous versions
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   # HTML table of previous versions of 'plspm'
#'   plspm_previous = previous_pkg_html_table("plspm")
#' } 
previous_pkg_html_table <- function(pkg_name) 
{
  pkg_url = previous_pkg_url(pkg_name)
  # output HTML table
  readHTMLTable(pkg_url, which = 1, header = FALSE,
                stringsAsFactors = FALSE)
}


#' @title Data Frame of R an package previous versions
#' 
#' @description Extracts HTML contents of an R package previous versions 
#' and returns a data frame with cleaned information
#' 
#' @details This function returns a data frame with the scraped contents 
#' of the original HTML table.
#' 
#' @param html_table an HTML table
#' @return data frame with five columns: \code{"Name"}, \code{"File"}, 
#' \code{"Date"}, \code{"Time"}, and \code{"Size"}
#' @export
#' @examples
#' \dontrun{
#'   # Previous versions of package "plspm"
#'   plspm_previous = previous_pkg_html_table("plspm")
#'   plspm_previous_df = previous_pkg_df(plspm_previous)
#'   
#'   plspm_previous_df
#' } 
previous_pkg_df <- function(html_table) 
{
  # remove first and fifth column
  html_table = html_table[,-c(1,5)]
  # remove first, second, and last useless rows
  end_row = nrow(html_table)
  html_table = html_table[-c(1,2,3,end_row),]
  # extract package name
  name = unlist(strsplit(x = html_table[1,1], split = "_"))[1L]
  # split 'Last modified' column into 'Date' and 'Time'
  last_modified = strsplit(html_table[,2], split = ' ')
  Date = unlist(lapply(last_modified, function(x) x[1L]))
  Time = unlist(lapply(last_modified, function(x) x[2L]))
  
  # return cleaned archive table
  data.frame(
    Name = rep(name, nrow(html_table)),
    File = html_table[,1L],
    Date = Date,
    Time = Time, 
    Size = html_table[,3L],
    stringsAsFactors = FALSE)
}


#' @title Data Frame with the previous versions of an R package 
#' 
#' @description Extracts HTML contents of an R package's previous versions 
#' and returns a data frame with cleaned information
#' 
#' @details This function returns a data frame with the scraped contents 
#' of the original HTML table.
#' 
#' @param pkg_name name of an R package in CRAN
#' @return data frame with five columns: \code{"Name"}, \code{"File"}, 
#' \code{"Date"}, \code{"Time"}, and \code{"Size"}
#' @seealso \code{\link{scrape_current_pkg}}
#' @export
#' @examples
#' \dontrun{
#'   # extract cleaned HTML table in a data frame
#'   prev_table = scrape_previous_pkg("plspm")
#' } 
scrape_previous_pkg <- function(pkg_name)
{
  # extrcat HTML table from Contrib Archive content  
  archive_table = previous_pkg_html_table(pkg_name) 
  # return cleaned data frame
  previous_pkg_df(archive_table)
}


#' @title Build data table 'cran_previous.csv'
#' 
#' @description Workhorse function for crawling Previous Versions of 
#' R packages in CRAN
#' 
#' @param pkg_names character vector with names of packages
#' @param file name of the destination file to write the csv table
#' @return csv file with scraped data. The fields are \code{Name}, 
#' \code{File}, \code{Date}, \code{Time} and \code{Size}
#' @export
#' @examples
#' \dontrun{
#'   # some packages in CRAN
#'   pkgs = c("knitr", "plyr", "plspm")
#'   
#'   # build table and save results in working directory
#'   build_previous_csv(pkgs, file = "previous.csv")
#' } 
build_cran_previous_csv <- function(pkg_names, file = "")
{
  if (!is.character(pkg_names))
    stop("\n'pkg_names' must be a character vector")
  if (file == "")
    stop("\na 'file' name is required")
  
  # initialize file to store packages
  arch_colnames = c("Name,File,Date,Time,Size")
  cat(arch_colnames, sep = "\n", file = file)
  
  # populate file
  for (i in seq_along(pkg_names))
  {
    # extract data frame of package previous versions
    pkg_clean_table = scrape_previous_pkg(pkg_names[i])
    # append it to file
    write.table(pkg_clean_table, 
                file = file, sep = ',', quote = FALSE,
                row.names = FALSE, col.names = FALSE, append = TRUE)
  }  
}
