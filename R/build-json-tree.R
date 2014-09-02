#' @title Split Package Attribute 
#' @description Splits a single attribute by removing commas
#' @details This function is used to create the JSON file for 
#' package dependencies and reverse dependencies
#' @param pkg_attr character string with the contents of an R package 
#' attribute (eg Name, Author)
#' @return character vector with split content
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   plspm_depends = c("R (> 3.0.1), amap, diagram, tester, turner")
#'   split_pkg_attr(plspm_depends)
#' }
split_pkg_attr <- function(pkg_attr)
{
  if (!is.na(pkg_attr)) {
    pkg_attr = unlist(strsplit(pkg_attr, split = ", "))
    pkg_attr = gsub("[[:punct:]]", "", pkg_attr)
  }
  # output
  pkg_attr
}


#' @title Add Quotes
#' @description Adds double quotation marks around a character string
#' @details This function is used to create the JSON file for 
#' package dependencies and reverse dependencies
#' @param str character string
#' @return quoted character string
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   s = 'quotations'
#'   add_quotes(s)
#' }
add_quotes <- function(str)
{
  if (!is.character(str))
    str = as.character(str)
  # add left-right quotes to each word 
  paste('\"', str, '\"', sep = '')
}


#' @title JSON Children
#' @description Converts the string of a package attribute into a 
#' JSON children array
#' @details This function is used to create the JSON file for 
#' package dependencies and reverse dependencies
#' @param pkg_attribute character string with the contents of an R package 
#' attribute (eg Name, Author)
#' @return character vector with split content
#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#'   plspm_depends = c("R (> 3.0.1), amap, diagram, tester, turner")
#'   json_children(plspm_depends)
#' }
json_children <- function(pkg_attribute)
{
  if (!is.na(pkg_attribute)) {
    split_attr = split_pkg_attr(pkg_attribute)
    # add left-right quotes to each word 
    split_attr = add_quotes(split_attr)
    # convert into json array notation
    children = paste('{ "name":', split_attr, '}', 
                     sep = ' ', collapse = ',\n\t\t\t\t')
    child_array = paste('[\n\t\t\t\t', children, '\n\t\t\t]', sep = '')
  } else {
    child_array = 'null'
  }
  # output
  paste('"children":', child_array)
}


#' @title Build JSON Tree
#' @description Builds a Tree structure under JSON format representing 
#' the dependencies and reverse dependencies of an R package
#' @param pkg_desc list of attributes from a package current description 
#' (obtained from output in \code{\link{scrape_current_pkg}})
#' @param file name of file to write the JSON tree
#' @export
#' \dontrun{
#'   plspm_deps = list(
#'     depends = c("R (≥ 3.0.1), amap, diagram, tester, turner"),
#'     imports = NA,
#'     suggests = c("plsdepot, FactoMineR, ggplot2, reshape, testthat, knitr"),
#'     revdepends = c("pathmox"),
#'     revimports = NA,
#'     revsuggests = c("matrixpls, plsdepot")
#'   )
#' 
#'   build_json_tree(plspm_deps, file = "toy_json_tree.json")
#' }
build_json_tree <- function(pkg_desc, file = "")
{
  # child node names
  child_names = c("depends", "imports", "suggests", 
                  "revdepends", "revimports", "revsuggests")
  
  # first lines
  cat('{ \n\t"name": ', add_quotes(pkg_desc$name), ',\n\t"children": [\n', 
      sep = '', file = file)
  # loop over child nodes
  for (i in seq_along(child_names)) 
  {
    cat('\t\t{\n\t\t\t"name":', add_quotes(child_names[i]), ',\n', sep = ' ',
        file = file, append = TRUE)
    cat('\t\t\t', json_children(pkg_desc[[child_names[i]]]),
        file = file, append = TRUE)
    if (i != length(child_names)) {
      cat('\n\t\t},\n', file = file, append = TRUE)
    } else {
      cat('\n\t\t}\n', file = file, append = TRUE)
    }
  }
  # last closing lines
  cat('\t]\n}', file = file, append = TRUE)
}
