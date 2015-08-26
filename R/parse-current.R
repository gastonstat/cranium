#' @title URL of a package current description 
#' 
#' @description Returns the URL of a package current description in CRAN
#' @param package name of an R package in CRAN
#' @return complete URL of current package description
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   # URL of current description of 'plspm'
#'   current_pkg_url("plspm")
#' }
current_pkg_url <- function(package) {
  sprintf("http://cran.r-project.org/web/packages/%s/index.html",
          package)
}


#' @title R Package Parser
#' 
#' @description Convenient wrapper to parse the html content of 
#' an R package page in CRAN
#' 
#' @param pkg_name name of an R package in CRAN
#' @return HTML parsed content
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   # example with R package 'plspm'
#'   plspm_url = "http://cran.r-project.org/web/packages/plspm/index.html"
#'   plspm_doc = pkg_parser(plspm_url)
#' } 
pkg_parser <- function(pkg_name) 
{
  # form package URL (current description in CRAN)
  pkg_url = current_pkg_url(pkg_name)
  # output
  htmlParse(pkg_url)
}


#' @title Get Package Name 
#' @description Exrtacts the package name from parsed html content
#' @param pkg_doc an object of class \code{"HTMLInternalDocument"}
#' @keywords internal
#' @export
get_pkg_name <- function(pkg_doc) {
  pkg_title = xpathSApply(pkg_doc, "//h2", xmlValue)
  # get just the name (first word)
  str_extract(string = pkg_title, pattern = "\\w+")
}


#' @title Get Package Description 
#' @description Extracts the package description from parsed html content
#' @param pkg_doc an object of class \code{"HTMLInternalDocument"}
#' @keywords internal
#' @export
get_pkg_description <- function(pkg_doc) {
  pkg_desc = xpathSApply(pkg_doc, "//p", xmlValue)
  # remove new lines '\n'
  gsub(pattern = "\n", replacement = ' ', x = pkg_desc)
}


#' @title Get Table of Attributes
#' @description Extracts the html table of attributes describing a package
#' @param pkg_doc an object of class \code{"HTMLInternalDocument"}
#' @keywords internal
#' @export
get_table_attrs <- function(pkg_doc) {
  pkg_tables = readHTMLTable(pkg_doc, 
                             header = FALSE, 
                             stringsAsFactors = FALSE)
  # return stacked html tables
  attrs_df = do.call("rbind", pkg_tables)
  # some string processing of attribute names
  attr_names = gsub(pattern = ":", replacement = "", attrs_df[,1L])
  attr_names = gsub(pattern = "\\s", replacement = "", attr_names)
  attrs_df[,1L] = attr_names
  # output
  attrs_df
}


#' @title Get Package Attribute
#' @description Extracts a given package attribute
#' @param pkg_attrs data frame with package attributes
#' @param attribute the name of an attribute
#' @keywords internal
#' @aliases get_pkg_attribute
#' get_pkg_version get_pkg_depends get_pkg_suggests
#' get_pkg_imports get_pkg_date get_pkg_author get_pkg_maintainer 
#' get_pkg_license get_pkg_compilation get_pkg_url get_pkg_citation
#' get_pkg_materials get_pkg_views get_pkg_rev_depends
#' get_pkg_rev_imports get_pkg_rev_suggests
#' @export get_pkg_attribute
#' get_pkg_version get_pkg_depends get_pkg_suggests
#' get_pkg_imports get_pkg_date get_pkg_author get_pkg_maintainer 
#' get_pkg_license get_pkg_compilation get_pkg_url get_pkg_citation
#' get_pkg_materials get_pkg_views get_pkg_rev_depends
#' get_pkg_rev_imports get_pkg_rev_suggests
get_pkg_attribute <- function(pkg_attrs, attribute) {
  attr_col = grep(pattern = attribute, pkg_attrs[,1L], ignore.case = TRUE)
  if (length(attr_col) > 0) {
    attrs = pkg_attrs[attr_col,2L]
    if (length(attrs) > 1) {
      attrs = paste(attrs, collapse = ', ')
    }
    return(attrs)
  } else {
    return(NA)
  }
}

get_pkg_version <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "version")
}

get_pkg_depends <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "depends")
}

get_pkg_suggests <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "suggests")
}

get_pkg_imports <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "imports")
}

get_pkg_date <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "published")
}

get_pkg_author <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "author")
}

get_pkg_maintainer <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "maintainer")
}

get_pkg_license <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "license")
}

get_pkg_compilation <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "needscompilation")
}

get_pkg_url <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "url")
}

get_pkg_citation <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "citation")
}

get_pkg_materials <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "materials")
}

get_pkg_views <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "inviews")
}

get_pkg_rev_depends <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "reversedepends")
}

get_pkg_rev_imports <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "reverseimports")
}

get_pkg_rev_suggests <- function(pkg_attrs) {
  get_pkg_attribute(pkg_attrs, "reversesuggests")
}


#' @title Get Table of Reverse Dependencies
#' @description Extracts the html table of reverse dependencies
#' @param pkg_doc an object of class \code{"HTMLInternalDocument"}
#' @keywords internal
#' @export
get_table_reverse <- function(pkg_doc) 
{
  # extract content from h4 elements
  h4_elements = xpathSApply(pkg_doc, "//h4", xmlValue)
  # check if there is "Reverse dependencies:"
  check_reverse = grep(pattern = "Reverse", x = h4_elements)
  if (length(check_reverse) > 0) {
    rev_table = readHTMLTable(pkg_doc, which = 3, 
                              header = FALSE, 
                              stringsAsFactors = FALSE)
    rev_table[,1] = substr(rev_table[,1], start = 9, stop = 17)
    return(rev_table)
  } else {
    return(NULL)
  }
}



#' @title Scrape Attributes of Current R Package Version
#' @description Extracts the attributes from the html content of a 
#' current R package version
#' @details This function is called by \code{\link{scrape_current_pkg}}
#' and \code{\link{scrape_current_pkg_html}}
#' @param package_doc package HTML document with  
#' Description of an R package's current version
#' @return list with package attributes. If an attribute is empty, the 
#' returned value is \code{NA}
#' @keywords internal
#' @export
scrape_current <- function(package_doc)
{
  # table of attributes
  package_attributes = get_table_attrs(package_doc)
    
  # return list with package attributes
  list(
    name = get_pkg_name(package_doc),
    description = get_pkg_description(package_doc),
    version = get_pkg_version(package_attributes),
    depends = get_pkg_depends(package_attributes),
    suggests = get_pkg_suggests(package_attributes),
    imports = get_pkg_imports(package_attributes),
    date = get_pkg_date(package_attributes),
    author = get_pkg_author(package_attributes),
    maintainer = get_pkg_maintainer(package_attributes),
    license = get_pkg_license(package_attributes),
    compilation = get_pkg_compilation(package_attributes),
    url = get_pkg_url(package_attributes),
    citation = get_pkg_citation(package_attributes),
    views = get_pkg_views(package_attributes),
    materials = get_pkg_materials(package_attributes),
    revdepends = get_pkg_rev_depends(package_attributes),
    revsuggests = get_pkg_rev_suggests(package_attributes),
    revimports = get_pkg_rev_imports(package_attributes)
  )
}


#' @title Scrape Attributes of Current R Package Version
#' @description Given a package name, extracts the attributes from the 
#' current version description
#' @param pkg_name name of an R package in CRAN (current version) 
#' @return list with package attributes. If an attribute is empty, the 
#' returned value is \code{NA}
#' @export
#' @examples
#' \dontrun{
#'   # extract list of attributes
#'   plspm_attrs = scrape_current_pkg("plspm")
#' }
scrape_current_pkg <- function(pkg_name)
{
  # parse package HTML document
  package_doc = pkg_parser(pkg_name)
  # scrape list of attributes
  scrape_current(package_doc)
}


#' @title Scrape Attributes of the HTML content containing the 
#' Current R Package Version
#' @description Given a package HTML content description, extracts the 
#' attributes of its current version
#' @param pkg_html package HTML document with  
#' Description of an R package's current version
#' @return list with package attributes. If an attribute is empty, the 
#' returned value is \code{NA}
#' @export
#' @examples
#' \dontrun{
#'   # download a copy to your current working directory
#'   plspm_current = download_current_pkg("plspm", "plspm_current.html")
#'   
#'   # extract list of attributes
#'   plspm_attrs = scrape_current_pkg_html(plspm_current)
#' }
scrape_current_pkg_html <- function(pkg_html)
{
  # parse package HTML document
  package_doc = htmlParse(pkg_html)
  # scrape list of attributes
  scrape_current(package_doc)
}



#' @title Build data table 'cran_packages.csv'
#' 
#' @description Workhorse function for crawling Current Versions of 
#' R packages in CRAN
#' 
#' @param pkg_names character vector with names of packages
#' @param file name of the destination file to write table
#' @return csv file with scraped data
#' @export
#' @examples
#' \dontrun{
#'   # some packages in CRAN
#'   pkgs = c("knitr", "plyr", "plspm")
#'   
#'   # build table and save results in working directory
#'   build_cran_current_csv(pkgs, file = "current.csv")
#' } 
build_current_csv <- function(pkg_names, file = "")
{
  if (file == "")
    stop("\n'build_current_csv()' requires a destiny file")

  ## Write output to file (start with column names)
  # scrape first package in the list of package names
  aux_url = sprintf("http://cran.r-project.org/web/packages/%s/index.html",
                    pkg_names)
  aux = scrape_current_pkg(aux_url)
  aux_df = data.frame(aux, stringsAsFactors = FALSE)
  table_colnames = paste(names(aux_df), collapse = ",")

  # write column names in file
  cat(table_colnames, "\n", file = file)
  # populate first row in file
  write.table(aux_df, file = file, sep = ',', 
              col.names = FALSE, row.names = FALSE, append = TRUE)
  
  if (length(pkg_names) > 1) {
    # crawl CRAN and fill-in 'cran_current.csv'
    for (i in 2L:length(pkg_names))
    {
      pkg_url = sprintf("http://cran.r-project.org/web/packages/%s/index.html",
                        pkg_names[i])
      aux = scrape_current_pkg(pkg_url)
      aux_df = data.frame(aux, stringsAsFactors = FALSE)
      # populate following rows in file
      write.table(aux_df, file = file, sep = ',', 
                  col.names = FALSE, row.names = FALSE, append = TRUE)
    } 
  }
}

