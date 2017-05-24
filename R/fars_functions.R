### BUILDING R PACKAGES
## ASSIGNMENT 2: DOCUMENTING CODE
# GABOR SOLYMOSI

#' @title Read "Fatality Analysis Reporting System" datasets 
#'
#' @description The "fars_read" function reads a csv format FARS dataset. You can customize
#' the file to read (using the \code{filename} argument). If you do not save
#' it to a variable, the function will print out information about the dataset
#' to the console.
#' 
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' 
#' @param filename A file path where the dataset is available
#' 
#' @return This function returns a dataset as a "data.frame" (and also incorporates
#'    the dplyr package's "tbl_df" and "tbl" classes). Additionally, it prints out
#'    information about the dataset to the console if a variable name to save is 
#'    not defined.
#'
#' @note There is an "if" condition in the beginning, which checks if the given 
#'    file path exists, results in an error if returns FALSE. 
#' 
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv.bz2")
#' fars.df <- fars_read("accident_2014.csv.bz2")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' @title Create a default filename for a given year 
#'
#' @description The "make_filename" function creates a character vector, which 
#' describes the default filename for FARS files. You can customize the year in 
#' the filename (using the \code{year} argument). It saves the year to an 
#' integer variable inside the function, then concatenates the given year
#' number with the "accident" sting and includes the file format, too.
#' 
#' @param year A number or vector of numbers describing the years. It can also 
#'    be given as character values.
#' 
#' @return This function returns a character vector, which is in the default 
#'    format of the FARS filenames.
#'    
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename("2013")
#' make_filename(c(2013, 2014))
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata", sprintf("accident_%d.csv.bz2", year), package="Fars.Assignment")
}

#' @title Read "Fatality Analysis Reporting System" data specifying the year number 
#'
#' @description The "fars_read_years" function reads yearly data of FARS datasets 
#' into a list where each element of the list contains specific data for the given year. 
#' You can customize the years of which you want to get more information (using
#' the \code{years} argument). The function reads the specified FARS datasets and 
#' mutates a new variable ("year") for the certain year, and selects the "MONTH"
#' attribute from the dataset, then gives a list of data tables of these two attributes
#' as a result.
#'  
#' @param years A number or vector of numbers describing the years to analyze 
#' 
#' @inheritParams make_filename
#' @inheritParams fars_read
#' 
#' @import dplyr
#' 
#' @return This function returns a list containing the data tables with two 
#'    variables ("MONTH", "year").
#'    
#' @note There is an error handling with a tryCatch() function. In case of any
#'    errors, it results in a warning.
#' 
#' @examples
#' \dontrun{
#' fars_read_years(2015)
#' fars_read_years(c(2013, 2014))
#' fars.year <- fars_read_years(2013)
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select_(~ MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' @title Summarize the number of accidents by month and year in "Fatality Analysis 
#' Reporting System" data specifying the year numbers 
#'
#' @description The "fars_summarize_years" function reads yearly data of FARS datasets 
#' and summarizes the number of accidents by month for each year specified. You can 
#' customize the years of which you want to get more information (using the 
#' \code{years} argument). The function uses the "fars_read_years" function to read 
#' year-specific FARS data, then groups the observations by year and month, and 
#' summarizes the number of accidents into a dataframe.
#'  
#' @param years A number or vector of numbers describing the years to analyze 
#' 
#' @inheritParams fars_read_years
#' 
#' @import dplyr
#' @importFrom tidyr spread
#' 
#' @return This function returns a dataframe describing the amount of accidents
#'    for the given years.
#'    
#' @examples
#' \dontrun{
#' fars_summarize_years(2015)
#' fars_summarize_years(c(2013, 2014))
#' fars.summary <- fars_summarize_years(2013)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by_(~ year,~ MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread_(~ year, n)
}


#' @title Plot accidents to a map of a specific state for a given year.
#'
#' @description The "fars_map_state" function reads yearly data of FARS datasets,
#' filters the data to the given state number, and plots the accidents based on 
#' their geographic locations (latitude, longitude). You can customize the state 
#' number and the years of which you want to plot (using the \code{state.num} and 
#' \code{year} arguments). The function uses the "make_filename" and the 
#' "fars_read" functions to read the datasets for the year specified, then filters
#' the data for the given state number, and plots the accidents to a map of 
#' the specific state based on the locations of accidents.
#'  
#' @param state.num A number describing the state to plot 
#' @param year A number describing the year to plot 
#' 
#' @inheritParams make_filename
#' @inheritParams fars_read
#' 
#' @import maps
#' @importFrom dplyr filter
#' @importFrom graphics points
#' 
#' @return This function returns a map plot of a given state visualizing
#'    the accidents based on their location
#' 
#' @note There is an "if" statement verifying that the given \code{state.num}
#'    is a valid state number. Stops if it's invalid. Additionally, 
#'    another "if" statement checks if the state-filtered dataset 
#'    contains any accidents. If it's empty, gives an error message and
#'    stops with an invisible NULL result.
#' 
#' @examples
#' \dontrun{
#' fars_map_state(1, 2013)
#' fars_map_state(8, 2015)
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data,~ STATE == state.num)
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
