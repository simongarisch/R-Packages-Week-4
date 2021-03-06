% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Collect Month / Year columns from FARS csv data}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector of years that you wish to collect data for}
}
\value{
Returns a list containing data frame objects with columns month and year (or NULL / nothing if there was an error).
}
\description{
This function takes a vector of years, reads the FARS data associated with these years and returns a list of data frame
results containing the month and year column for each year's data frame.
The FARS csv files you intend to read must be in your working directory otherwise the function will give you a warning: invalid year.
The code for this function is part of the Building R Packages course sourced from coursera.
Requires the dplyr and readr package to be loaded.
Example data is only available for 2013:2015.
}
\examples{
\dontrun{
  setwd("data")
  years <- 2013:2015
  fdat <- fars_read_years(years)
  print(class(fdat))
  print(head(fdat[1]))
}

}
