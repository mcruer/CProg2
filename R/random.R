#' Determine School Year
#'
#' This function takes a date and returns a string representing the school year in the format "YYYY-YY".
#' The school year runs from September of one year to August of the next year.
#'
#' @param date A date object or a string that can be coerced to a date.
#' @return A character string representing the school year, in the format "YYYY-YY".
#' If the input date is invalid or `NA`, the function returns `NA_character_`.
#'
#' @examples
#' school_year("2024-05-15")
#' # Returns: "2023-24"
#'
#' school_year("2023-09-01")
#' # Returns: "2023-24"
#'
#' school_year(NA)
#' # Returns: NA_character_
#'
#' @importFrom lubridate as_date month year
#' @importFrom stringr str_c
#' @export
school_year <- function(date) {
  date <- lubridate::as_date(date)

  year_start <- dplyr::if_else(lubridate::month(date) < 9, lubridate::year(date) - 1, lubridate::year(date))
  year_suffix <- sprintf("%02d", year_start %% 100 + 1)
  stringr::str_c(year_start, "-", year_suffix)
}


#' Determine Fiscal Year
#'
#' This function takes a date and returns a string representing the fiscal year in the format "YYYY-YY".
#' The fiscal year is assumed to run from April of one year to March of the next year.
#'
#' @param date A date object or a string that can be coerced to a date.
#' @return A character string representing the fiscal year, in the format "YYYY-YY".
#' If the input date is invalid or `NA`, the function returns `NA_character_`.
#'
#' @examples
#' fiscal_year("2024-03-31")
#' # Returns: "2023-24"
#'
#' fiscal_year("2023-04-01")
#' # Returns: "2023-24"
#'
#' fiscal_year(NA)
#' # Returns: NA_character_
#'
#' @importFrom lubridate as_date month year
#' @importFrom stringr str_c
#' @export
fiscal_year <- function(date) {
  date <- lubridate::as_date(date)

  year_start <- dplyr::if_else(lubridate::month(date) < 4, lubridate::year(date) - 1, lubridate::year(date))
  year_suffix <- sprintf("%02d", year_start %% 100 + 1)
  stringr::str_c(year_start, "-", year_suffix)
}


#' Calculate Infant Child Care Spaces
#'
#' Multiplies the number of rooms by 10 to estimate available infant child care spaces.
#'
#' @param rooms Number of rooms allocated for infant care.
#'
#' @return Estimated number of infant child care spaces.
#' @export
cc_spaces_infant <- function(rooms){
  return(rooms * 10)
}

#' Calculate Toddler Child Care Spaces
#'
#' Multiplies the number of rooms by 15 to estimate available toddler child care spaces.
#'
#' @param rooms Number of rooms allocated for toddler care.
#'
#' @return Estimated number of toddler child care spaces.
#' @export
cc_spaces_toddler  <- function(rooms){
  return(rooms * 15)
}

#' Calculate Preschool Child Care Spaces
#'
#' Multiplies the number of rooms by 24 to estimate available preschool child care spaces.
#'
#' @param rooms Number of rooms allocated for preschool care.
#'
#' @return Estimated number of preschool child care spaces.
#' @export
cc_spaces_preschool <- function(rooms){
  return(rooms * 24)
}

#' Calculate Family Group Child Care Spaces
#'
#' Multiplies the number of rooms by 15 to estimate available family group child care spaces.
#'
#' @param rooms Number of rooms allocated for family group care.
#'
#' @return Estimated number of family group child care spaces.
#' @export
cc_spaces_fg <- function(rooms){
  return(rooms * 15)
}
