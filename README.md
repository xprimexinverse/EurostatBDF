# EurostatBDF
An R package for downloading datasets from the Eurostat Bulk Download Facility

# Description
The workhorse function of the package is getEurostat(). This function returns a
list containing a Eurostat dataset, the code lists for the dataset, and the dataset metadata.
It is optional for the function to also return the raw data (as it was downloaded)
and the data in CSV form (containing codes and labels). 

The code behind this package also serves as a backend to an EViews add-in created by the author.

The package has no dependencies other than base R.
