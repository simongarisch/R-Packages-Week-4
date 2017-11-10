
#*****************************************************************************************************************************
# THESE ARE NOTES DESCRIBING THE SETUP... feel free to skip...
# created a new package in R with File -> New Project -> New Directory -> R Package
# deleted the default hello.R file and dropped in fars_functions.R
# there is a book from Hadley Wickham on creating R packages here: http://r-pkgs.had.co.nz/
# there are a few things to install before we get started as laid out in the intro section: http://r-pkgs.had.co.nz/intro.html
# download Rtools particular to your setup: https://cran.r-project.org/bin/windows/Rtools/
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages("rstudioapi")
# rstudioapi::isAvailable("0.99.149")
# devtools::install_github("hadley/devtools")

# we'll be looking to complete the following...
# * for documentation we can use roxygen2: http://r-pkgs.had.co.nz/man.html
# * for testing we can use testthat: http://r-pkgs.had.co.nz/tests.html

# there are settings in RStudio that we'll want to setup... Go to Build -> Configure Build Tools -> ...
# *** Check 'Generate documentation with Roxygen'. Check all of the roxygen options. Now we can create docs with Build -> Document
# *** Add '--as-cran' (no quotes) to the R CMD check additional options to enable CRAN checks over your package

# Run 'devtools::use_testthat()' (no quotes) which will create a tests/testthat folder for automated testing
# These tests will run automatically when R CMD check runs (in addition to the CRAN checks we've setup above)
# Also see https://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf for testing info

# using data in the package: 'devtools::use_data' will create a data folder that holds our data
# I've loaded the three files using R's Environment -> 'Inport Dasdtaset'. Note that they are now dataframes in memory called
# accident_2013.csv, accident_2014.csv, accident_2015.csv of the class data.frame
# you'll need to be careful with default file sizes that get loaded by default
# Place these data frames in our package data folder using
# devtools::use_data(accident_2013.csv) # https://www.rdocumentation.org/packages/devtools/versions/1.13.3/topics/use_data
# devtools::use_data(accident_2014.csv)
# devtools::use_data(accident_2015.csv)
# I'm also placing the raw .csv files into this folder
# LazyData: TRUE by default in the description file, so these datasets will get auto loaded with the package
# I've set LazyData to False so that the data has to be explicitly loaded

# roxygen2 manual: http://roxygen.org/roxygen2-manual.pdf and https://bookdown.org/rdpeng/RProgDA/documentation.html#common-roxygen2-tags
# there is also the option in RStudio's Code Tools to click 'Insert Roxygen Skeleton', but this is fairly minimal

# I have managed library requirements such as readr and dplyr, for example, from the description file.

# you can test the documentation below with
#?fars_read
#example(fars_read)            ####
#?make_filename
#example(make_filename)        ####
#?fars_read_years
#example(fars_read_years)      ####
#?fars_summarize_years
#example(fars_summarize_years) ####
#?fars_map_state
#example(fars_map_state)       ####

# https://www.r-bloggers.com/rstudio-and-github/
#*****************************************************************************************************************************


#' Read csv files into a data.frame / tbl_df
#'
#' This basic function takes a csv file path and reads the file using readr::read_csv and dplyr::tbl_df.
#' Note that dplyr::tbl_df (in the code) is deprecated and you should use tibble::as_tibble() instead.
#' The code for this function is part of the Building R Packages course sourced from coursera.
#' Example data is only available for 2013-2015.
#' Requires readr and dplyr to run.
#'
#' @param filename Path to a csv file.
#'
#' @return Returns an object of the class data.frame, dbl_df, tbl.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   df <- fars_read(file.path("data", "accident_2013.csv.bz2"))
#'   print(class(df))
#'   head(df)
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr dbl_df
#'
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#*****************************************************************************************************************************


#' Get Fatality-Analysis-Reporting-System-(FARS) csv filename
#'
#' This function returns the filename for FARS csv data associated with a particular year.
#' See \href{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}{the FARS website} for more info.
#' The code for this function is part of the Building R Packages course sourced from coursera.
#' Example data is only available for 2013-2015.
#'
#' @param year The year associated with the file name you want to return.
#'
#' @return Returns a character string representing the name of a csv file.
#' @export
#'
#' @examples
#'   print(make_filename("2015"))
#'

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#*****************************************************************************************************************************
#' Collect Month / Year columns from FARS csv data
#'
#' This function takes a vector of years, reads the FARS data associated with these years and returns a list of data frame
#' results containing the month and year column for each year's data frame.
#' The FARS csv files you intend to read must be in your working directory otherwise the function will give you a warning: invalid year.
#' The code for this function is part of the Building R Packages course sourced from coursera.
#' Requires the dplyr and readr package to be loaded.
#' Example data is only available for 2013:2015.
#'
#' @param years A vector of years that you wish to collect data for
#'
#' @return Returns a list containing data frame objects with columns month and year (or NULL / nothing if there was an error).
#' @export
#'
#' @examples
#' \dontrun{
#'   setwd("data")
#'   years <- 2013:2015
#'   fdat <- fars_read_years(years)
#'   print(class(fdat))
#'   print(head(fdat[1]))
#' }
#'
#' @import dplyr, readr
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#*****************************************************************************************************************************
#' Summarize FARS data by month and year
#'
#' Designed to load multiple years of FARS csv data and show the total number of fatal road incidents by month (rows) and years (columns).
#' The FARS csv files you intend to read must be in your working directory otherwise the function will give you a warning: invalid year.
#' The code for this function is part of the Building R Packages course sourced from coursera.
#' Requires the dplyr and tidyr packages to be loaded.
#' Example data is only available for 2013:2015.
#'
#' @param years A vector of the years you'd like to collect data for
#'
#' @return A summary data.frame showing the total number of fatal road incidents by month / year.
#' @export
#'
#' @examples
#' \dontrun{
#'   setwd("data")
#'   years <- 2013:2015
#'   df <- fars_summarize_years(years)
#'   print(class(df))
#'   print(head(df))
#' }
#'
#' @import dplyr, readr, tidyr
#'
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#*****************************************************************************************************************************

#' Map FARS fatal road incients by state
#'
#' This function will collect FARS csv data for a given year and map the data for a given state number (state.num).
#' The code for this function is part of the Building R Packages course sourced from coursera.
#' Requires the dplyr, readr, maps, stats and graphics packages to be loaded.
#' Example data is only available for 2013:2015.
#'
#' @param state.num The unique state number
#' @param year The year for which we'll be collecting data
#'
#' @return No value is returned. Will map the FARS data.
#' @export
#'
#' @examples
#' \dontrun{
#'   setwd("data")
#'   year <- 2015
#'   state.num <- 15
#'   fars_map_state(state.num, year)
#' }
#'
#' @import dplyr, readr, tidyr, maps, graphics
#'
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
