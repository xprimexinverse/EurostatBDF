# Header ------------------------------------------------------------------
#
# Title: R code for interacting with the Eurostat Bulk Download Facility
#
# Author: Graeme Walsh <graeme.walsh@hotmail.co.uk>
# Date: 17/08/2018


# Package Info ------------------------------------------------------------

#' A package for downloading datasets from the Eurostat Bulk Download Facility.
#'
#' The workhorse function of the package is \code{getEurostat()}. This function returns a
#' list containing a Eurostat dataset, the code lists for the dataset, and the dataset metadata.
#' It is optional for the function to also return the raw data (as it was downloaded)
#' and the data in CSV form (containing codes and labels). The code behind this package also
#' serves as a backend to an EViews add-in created by the author.
#'
#'
#' @section EurostatBDF functions:
#' \code{getEurostat()}
#'
#' @docType package
#' @name EurostatBDF
NULL


# Important variables -----------------------------------------------------

# New environment
EurostatBDF.env <- new.env()

# Eurostat Bulk Download Facility URL
EurostatBDF.env$base_url <- "http://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing"

# Query strings
EurostatBDF.env$data_query <- "?sort=1&file=data%2F"
EurostatBDF.env$dic_query  <- "?sort=1&file=dic%2Fen%2F"
EurostatBDF.env$home_query <- "?sort=1&file="

# Table dictionary
EurostatBDF.env$tdic_name <- "table_dic"

# Table of contents
EurostatBDF.env$toc_name <- "table_of_contents_en"

# File extensions
EurostatBDF.env$filext <- c(".tsv.gz", ".sdmx.zip", ".dic", ".sdmx.xml", ".txt", ".gz", ".zip")

# Flags in database
EurostatBDF.env$flags <- "[[:space:]]+[bcdefnprsuz]"


# Dictionary functions ----------------------------------------------------

#' @title Get a dictionary file from the Eurostat Bulk Download Facility
#' @description Reads a Eurostat dictionary file into R as a two-column dataframe
#' @param dic_name The name of the dictionary file of interest
#' @export
#' @importFrom utils read.table
#' @author Graeme Walsh
#' @details The dictionary file is read from the /dic/en sub-folder
getDic <- function(dic_name) {
  dic <- read.table(file = paste0(EurostatBDF.env$base_url, EurostatBDF.env$dic_query, dic_name, EurostatBDF.env$filext[3]), sep = "\t", quote = "", stringsAsFactors = FALSE, col.names = c("Code", "Label"))
  return(dic)
}

#' @title Get the table dictionary file from the Eurostat Bulk Download Facility
#' @description Reads the Eurostat table dictionary file into R as a two-column dataframe
#' @export
#' @importFrom utils read.table
#' @author Graeme Walsh
#' @details The table dictionary file is read from the /dic/en sub-folder
getTdic <- function() {
  tdic <- getDic(EurostatBDF.env$tdic_name)
  return(tdic)
}

#' @title Get the code lists for a dataset
#' @description Creates a list containing all of the dataset dimensions including values and labels
#' @param tidy_data A tidy Eurostat dataset
#' @export
#' @author Graeme Walsh
#' @details The dataset dimensions are matched to the values in the corresponding dictionary files
getCodeLists <- function(tidy_data){

  # Get dataset dimensions (e.g. geo, unit)
  dims <- strsplit(sub("\\\\", ",", names(tidy_data)[1]), split = ",")[[1]]
  dims <- dims[-length(dims)]

  # Get dataset dimension values (e.g. IE, PC_GDP)
  dim_vals <- apply(matrix(unlist(strsplit(tidy_data[,1], split = ",")), ncol = length(dims), byrow = TRUE), 2 , unique)

  # Get the dictionary files
  dics <- lapply(dims, getDic)

  # Match the dataset dimension values to their values and labels in the dictionary files
  code_lists <- list()
  for (i in c(1:length(dims))) {
    code_lists[[i]] <- dics[[i]][match(dim_vals[[i]], dics[[i]][,1]),]
    colnames(code_lists[[i]]) <- c("Code","Label")
    rownames(code_lists[[i]]) <- NULL
  }
  names(code_lists) <- dims

  return(code_lists)
}


# Table of contents functions ---------------------------------------------

#' @title Get the table of contents file from the Eurostat Bulk Download Facility
#' @description Reads the Eurostat table of contents file into R as a eight-column dataframe
#' @export
#' @importFrom utils read.table
#' @author Graeme Walsh
#' @details The table of contents file is read from the home directory
getToc <- function() {
  toc <- read.table(file = paste0(EurostatBDF.env$base_url, EurostatBDF.env$home_query, EurostatBDF.env$toc_name, EurostatBDF.env$filext[5]), sep = "\t", quote = "\"", header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  return(toc)
}

#' @title Get the metadata associated with a dataset
#' @description Reads the metadata for a dataset from the table of contents file
#' @param dataset_name The dataset code of interest
#' @export
#' @author Graeme Walsh
#' @details The metdata contains information such as title, code, last update, etc.
getMeta <- function(dataset_name){

  # Get the table of contents file
  toc <- getToc()

  # Find the metadata by matching the dataset code name its value in the toc
  metadata <- toc[which(toc[,"code"] == dataset_name),][1,]
  rownames(metadata) <- NULL

  return(metadata)
}

#' @title Get the time period and frequency of a dataset
#' @description Gets the time period and frequency information from the table of contents file
#' @param dataset_name The dataset code of interest
#' @export
#' @author Graeme Walsh
#' @details Returns the frequency, start date, and end date of the dataset
getPeriod <- function(dataset_name){

  # Get the metadata from table of contents file
  metadata <- getMeta(dataset_name)

  # Get the start and end period of the dataset
  time_period <- metadata[,c("data start", "data end")]

  # Get the frequency of the dataset
  if(grepl("Q", time_period[1])) {
    freq <- "Q"
  } else if(grepl("M", time_period[1])){
    freq <- "M"
  } else if(grepl("H", time_period[1])){
    freq <- "H"
  } else if(grepl("W", time_period[1])){
    freq <- "W"
  } else if(grepl("D", time_period[1])){
    freq <- "D"
  } else if(!grepl("\\D", time_period[1])){
    freq <- "A"
  } else{
    freq <- "Unknown"
  }

  period <- cbind(freq, time_period)

  return(period)
}


# Read data functions -----------------------------------------------------

#' @title Get a dataset in raw format from the Eurostat Bulk Download Facility
#' @description Download data from Eurostat
#' @param dataset_name The dataset code of interest
#' @param format The format of the dataset
#' @export
#' @importFrom utils download.file read.table
#' @author Graeme Walsh
#' @details The dataset is downloaded from the Bulk Download Facility
getEurostatRaw <- function(dataset_name, format="tsv"){
  if(format == "tsv"){

    # Create .gz temp file
    temp <- tempfile(fileext = EurostatBDF.env$filext[6])

    # Download the file
    download.file(url = paste0(EurostatBDF.env$base_url, EurostatBDF.env$data_query, dataset_name, EurostatBDF.env$filext[1]), destfile = temp)

    # Read the .tsv file as dataframe
    raw_data <- read.table(file = gzfile(temp), sep = "\t", header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE, check.names = FALSE)

  }else if(format == "sdmx"){

    stop("SDMX file format not yet supported by the EurostatBDF package. Try TSV format, instead.")

    # Create .zip temp file
    # temp <- tempfile(fileext = EurostatBDF.env$filext[7])

    # Download the file
    # download.file(url = paste0(EurostatBDF.env$base_url, EurostatBDF.env$data_query, dataset_name, EurostatBDF.env$filext[2]), destfile = temp)

    # Code needs to be written...
  }
  return(raw_data)
}

#' @title Get a complete dataset from the Eurostat Bulk Download Facility
#' @description Download data from Eurostat (including code lists and metadata)
#' @param dataset_name The dataset code of interest
#' @param append_raw TRUE or FALSE
#' @param append_csv TRUE or FALSE
#' @export
#' @author Graeme Walsh
#' @details The dataset is returned as a list containing data, code lists, and metadata. The raw data can also be returned.
getEurostat <- function(dataset_name, append_raw=FALSE, append_csv=FALSE){

  # Get the raw data
  raw_data <- getEurostatRaw(dataset_name)

  # Tidy the raw data
  tidy_data <- tidyRawData(raw_data)

  # Get the code lists for the dataset
  code_lists <- getCodeLists(tidy_data)

  # Get the metadata for the dataset
  meta_data <- getMeta(dataset_name)

  # Package together all of the information into a list
  eurostat_dataset <- list(data = tidy_data, code_lists = code_lists, metadata = meta_data)

  # Append raw data
  if(append_raw == TRUE){
    eurostat_dataset <- append(eurostat_dataset, list(raw_data), after = length(eurostat_dataset))
    names(eurostat_dataset)[length(eurostat_dataset)] <- "raw_data"
  }

  # Prepare and append CSV data
  if(append_csv == TRUE){
    csv_data <- prepareCSV(tidy_data)
    eurostat_dataset <- append(eurostat_dataset, list(csv_data), after = length(eurostat_dataset))
    names(eurostat_dataset)[length(eurostat_dataset)] <- "csv_data"
  }

  return(eurostat_dataset)
}


# Data manipulation functions ---------------------------------------------

#' @title Tidy a raw format Eurostat dataset (remove flags, etc.)
#' @description Tidy a raw format Eurostat dataset (remove flags, etc.)
#' @param raw_data The raw dataset of interest
#' @export
#' @author Graeme Walsh
#' @details Flags are removed, missing values are replaced (: to NA), and columns are converted from descending to ascending
tidyRawData <- function(raw_data){

  # Remove flags, replace missing values with NA, and convert from strings to numerics
  raw_data[2:ncol(raw_data)] <- lapply(raw_data[2:ncol(raw_data)], function(x) {
    x <- gsub(EurostatBDF.env$flags, "", x)
    x[x == ":"] <- NA
    x <- as.numeric(as.character(x))
  })

  # Rearrange columns from descending to ascending order
  tidy_data <- raw_data[, c(1, ncol(raw_data):2)]

  return(tidy_data)
}


# Functions for EViews ----------------------------------------------------

#' @title Prepare a dataset to be saved in CSV format (add codes and labels to dataframe)
#' @description Prepares a dataset to be saved in CSV format (containing codes and labels)
#' @param tidy_data A tidy Eurostat dataset
#' @export
#' @author Graeme Walsh
#' @details Codes and labels are added to the dataframe
prepareCSV <- function(tidy_data){

  # Get dataset dimensions (e.g. geo, unit)
  dims <- strsplit(sub("\\\\", ",", names(tidy_data)[1]), split = ",")[[1]]
  dims <- dims[-length(dims)]

  # Get dataset dimension values (e.g. IE, PC_GDP)
  dim_vals <- apply(matrix(unlist(strsplit(tidy_data[,1], split = ",")), ncol = length(dims), byrow = TRUE), 2 , unique)

  # Get the dictionary files
  dics <- lapply(dims, getDic)

  # Split the identifier into separate columns
  var_id    <- strsplit(tidy_data[,1], split = ",")
  var_codes <- do.call("rbind", var_id)

  # Match the dimension values to their labels in the dictionary files
  labs <- list()
  for (i in c(1:(length(dims)))) {
    labs[[i]] <- dics[[i]][match(var_codes[,i], dics[[i]][,1]),]
  }
  labs_cols <- do.call("cbind", labs)
  colnames(labs_cols) <- paste0(rep(dims[1:length(dims)], each=2), c("_code", "_description"))

  # Add labels to the dataframe
  csv_data <- cbind(tidy_data[,1], labs_cols, tidy_data[,-1])
  rownames(csv_data) <- NULL
  colnames(csv_data)[1] <- "identifier"

  return(csv_data)
}

#' @title Get column header properties (for use in EViews)
#' @description Gets the names and number of column headers (for use in EViews)
#' @param csv_data A tidy Eurostat dataset prepared to be exported as CSV file for EViews
#' @export
#' @author Graeme Walsh
#' @details Returns the names and number of column headers for importing a CSV file into EViews
getColhead <- function(csv_data){

  # Get column header names
  colheadnames <- names(which(sapply(csv_data, is.character)))
  colheadnames <- c("Name", colheadnames)

  # Get the number of column headers
  colheadnum <- length(colheadnames)

  # Prepare the column header names string for EViews
  colheadnamesQ <- paste0("\"", colheadnames, "\"")
  colheadnamesQC <- paste0(colheadnamesQ, ",")
  colheadnamesQC[length(colheadnamesQC)] <- colheadnamesQ[length(colheadnamesQ)]
  colheadnamesFinal <- paste(colheadnamesQC, collapse = " ")

  # Return the names and number as a list
  colhead <- list(names=colheadnamesFinal, num=colheadnum)

  return(colhead)
}

