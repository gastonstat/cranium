# CRANIUM

The R package ```"cranium"``` is part of the **Craniography** project for exploring R packages in [CRAN](http://cran.r-project.org/) (Comprehensive R Archive Network).

It is developed and maintained by [Gaston Sanchez](http://gastonsanchez.com), and kindly shared to the R community.


## Installation

An integral part of the **craniography** project is the R package 
```"cranium"``` freely available on github. To install the package, 
open your R console and run the following commands:

```ruby
# install "devtools" (if you don't have it yet)
install.packages("devtools") 

# load devtools
library(devtools)

# install "cranium"
install_github('cranium', username = 'gastonstat')

# load cranium
librayr(cranium)
```



### Description

```"cranium"``` allows you to do three major tasks:

- Parse a given package in CRAN available (current versions)
- Parse a given package in CRAN Archive (previous versions)
- Build JSON structure of a package dependencies to plot with ```D3.js```


### Scraping the attributes of a package current version description

To parse the description of an R package's current version, pass the name of the package you are interested in to the ```scrape_current_pkg()``` function. The output is a list with all the attributes of the package's description:

```ruby
# R package 'plspm'
package_name = "plspm"

# List of attributes from the current description of 'plspm'
current_plspm = scrape_current_pkg(package_name)
```


### Scraping previous versions of a given package 

To parse previous versions (i.e. archived versions) of an R package, pass the name of the package you are interested in to the  
```scrape_previous_pkg()``` function. The output is a data frame with the information of previous versions:

```ruby
# R package 'plspm'
package_name = "plspm"

# Data Frame with the previous versions of 'plspm'
prev_table = scrape_previous_pkg(package_name)
```


### Download a local HTML copy of a package description

Most of the times it's a good idea to save a local copy of the HTML CRAN descriptions of the explored package. You can download the HTML file with the current description of an R package like this:

```ruby
# R package 'plspm'
package_name = "plspm"

# destiny file
html_current = "plspm_current.html"

# download local html copy (in your working directory)
download_current_pkg(package_name, html_current)
```


### Download a local HTML copy of a package previous version 

Most of the times it's a good idea to save a local copy of the HTML CRAN descriptions of the explored package. You can download the HTML file with the archived (previous) versions of an R package like this:

```ruby
# R package 'plspm'
package_name = "plspm"

# destiny file
html_previous = "plspm_previous.html"

# download local html copy (in your working directory)
download_previous_pkg(package_name, html_previous)
```


### Build JSON Tree

To help you visualize the dependencies of an R package using the 
[D3.js](http://d3js.org/) JavaScript library, ```"cranium"``` prodives the function ```build_json_tree()``` to get a json file with the right data structure required by tree layouts 
(eg [Radial Reingold–Tilford Tree](http://bl.ocks.org/mbostock/4063550), 
[Dendrogram](http://bl.ocks.org/mbostock/4063570))

```ruby
# List of attributes from the current description of 'plspm'
current_plspm = scrape_current_pkg(package_name)

# select dependencies of 'plspm'
plspm_deps = current_plspm[c("depends", "imports", "suggests", 
  "revdepends", "revimports", "revsuggests")]

build_json_tree(plspm_deps, file = "toy_json_tree.json")
```



Author Contact
---------------
[Gaston Sanchez](http://www.gastonsanchez.com)
(`gaston.stat at gmail.com`)
