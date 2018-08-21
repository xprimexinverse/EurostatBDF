The EurostatBDF package
==========

This R package can be used to retrieve datasets from the Eurostat [Bulk Download Facility](http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing). 

The code behind this package also serves as a backend to an EViews add-in created by the author.

The package can retrieve the data in two formats:

1. TSV
2. SDMX (Note: not yet available, but coming soon)

To cite the EurostatBDF package in publications, use:

>  Graeme Walsh (2018). _EurostatBDF_ R package version **0.1.0**.

Installation - Method 1
-----------

To install the package, use the [devtools package](http://cran.r-project.org/web/packages/devtools/index.html) as follows:

```r
install.packages("devtools")
library(devtools)
install_github(repo="xprimexinverse/EurostatBDF")
```

Installation - Method 2
-----------

If you have trouble installing the package using devtools, here is a second method.

1. Go to [GitHub](https://github.com/xprimexinverse/EurostatBDF)
2. Download the zip file (click on Clone or Download)
3. Unzip the file
4. In the unzipped file, open the R Project (.rproj) file
5. In RStudio, click Build (top right corner) then Build & Reload (this step may require installing Rtools)

Installation - Method 3
-----------

If you're really stuck, you can load the functions in the package as follows:

```r
source("https://raw.githubusercontent.com/xprimexinverse/EurostatBDF/master/R/EurostatBDF.R")
```

Quick Start
-----------

Begin by loading the package and reading some of the man pages:

```r
library(EurostatBDF)
?EurostatBDF
?getEurostatBDF
```

Examples
-----------

To quickly get a feel for how the package works, run the following examples.

The first example shows how to retrieve Annual National Accounts data from the Bulk Download Facility. The dataset is returned as a list object with 3 elements: "data", "code_lists", and "metadata".

```r
# Example 1
nama_10_gdp <- getEurostat("nama_10_gdp")
class(nama_10_gdp)
names(nama_10_gdp)
```

The second example demonstrates how to retrieve Quarterly National Accounts data from the Bulk Download Facility. The dataset is returned as a list object with three elements: "data", "code_lists", and "metadata".

```r
# Example 2
namq_10_gdp <- getEurostat("namq_10_gdp")
class(namq_10_gdp)
names(namq_10_gdp)
```

The third example again demonstrates how to retrieve Annual National Accounts data from the Bulk Download Facility. This time the raw data is included in the list object as a fourth element.

```r
# Example 3
nama_10_gdp <- getEurostat("nama_10_gdp", append_raw = TRUE)
class(nama_10_gdp)
names(nama_10_gdp)
```

The fourth example demonstrates how to print the dataset metadata. 

```r
# Example 4
nama_10_gdp$metadata
```

The next example demonstrates how to explore the dimensions of the dataset.

```r
# Example 5
names(nama_10_gdp$code_lists)
nama_10_gdp$code_lists[["geo"]]
nama_10_gdp$code_lists[["na_item"]]
```

The next example demonstrates how to extract a variable from the dataset.

```r
# Example 6
unit    <- "CLV10_MEUR"
na_item <- "B1G"
geo     <- "IE"

(selection <- paste(unit, na_item, geo, sep=","))

nama_10_gdp$data[which(nama_10_gdp$data[,1]==selection),]
```

The final example shows how to read the help pages for a few of the other useful functions in the package.

```r
# Example 7
?getEurostatRaw
?getToc
?getTdic
```

Feedback, Bugs, Suggestions
-----------

Please contact me at <graeme.walsh@centralbank.ie> or <graeme.walsh@hotmail.co.uk>


News (2018 - August)
-----------
This is the first release of the package. Future plans include adding the option to download the data in SDMX format and adding helper functions for extracting variables from the dataset. The former is less of a priority at the moment because working with SDMX files (e.g. downloading and parsing) is slower when compared to using TSV files.

Disclaimer
-----------

I have no affiliation with Eurostat. This package is not official software of Eurostat nor is the package endorsed by Eurostat.
