##'NAME
#'name fars_read
#'
##'TITLE
#'title read function
#'#'@note
#'fars_functions : Package for reading, summarising and create visual representation
#'for data from the Fatality Analysis Reporting System (FARS).
#'@note
#'fars_read function reads the given file and convert resulting data frame
#'to a table using tbl_df function from dplyr
#'@details
#': Function Checks the existence of given file. In case file is not found, a
#'           message is generated. While reading the file,suppresses the progress
#'           messages to have a clean output
#'@examples \dontrun{- fars_read("accident_2015.csv.bz2")}
#'@import readr
#'@importFrom dplyr tbl_df
#'@param - filename : a character string with path of the file
#'@return - Tibble object with csv file data
#'@export
fars_read <-
  function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}
##'NAME
#'name make_filename
#'
##'TITLE
#'title function creates a custom file name based on year argument value
#'@note make_filename function creates a custom file name based on year argument value
#'@examples \dontrun{make_filename(2015)}
#'@param - year- Numeric e.g. 2015
#'@return - character string having custom file name e.g.accident_2015.csv.bz2
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
##'name
#'name fars_read_years
#'
##'TITLE
#'title function to iteratively read the files
#'@note -fars_read_years function to iteratively read the files based on numeric year
#'vector
#'@examples \dontrun{fars_read_years(c(2015,1980,2010))}
#'@importFrom dplyr select
#'@importFrom dplyr mutate
#'@importFrom magrittr %>%
#'@param - a numeric vector contains years e.g. c(2015,2001,1980)
#'@return - a list of tibbles each containing year and month columns. If files does not exist, returns NULL
#'@export
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
##'name
#'name fars_summarize_years
#'
##'TITLE
#'title combines files
#'@note -fars_summarize_years combines multiple year files by rows and then group on year and month
#' Further it calculates summary of the resulting data frame and spreads across year

#'@param- a numeric vector contains years e.g. c(2015,2001,1980)

#'@return A wide format tibble. One column for year and a MONTH column. Each
#' value is the count of observations in a year-month.

#'@examples \dontrun{fars_summarize_years(c(2013,2014,2015))}

#'@importFrom dplyr bind_rows
#'@importFrom dplyr group_by
#'@importFrom dplyr summarize
#'@importFrom tidyr spread
#'@importFrom dplyr n
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}
##'name
#'name fars_map_state
#'
##'TITLE
#'title function to draw a map
#'@note -fars_map_state- draw a map for a given year and state
#'@details  #’  Function to check if there is data for a state in a particular year file. If data
#'is present and plot longitude against latitude for observations where
#' longitude is > 900 and latitude is >90
#' Add Points to a Plot
#'@param-a vector containing statenumber and year or state and year e.g. fars_map_state(20,2015)

#'@return NULL

#'@importFrom maps map
#'@importFrom dplyr filter
#'@importFrom graphics points
#'@examples \dontrun{fars_map_state(20,2015)}
#'@export
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
