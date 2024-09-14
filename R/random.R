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

  # Ensure the input date is valid
  if (is.na(date)) {
    return(NA_character_)
  }

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

  # Ensure the input date is valid
  if (is.na(date)) {
    return(NA_character_)
  }

  year_start <- dplyr::if_else(lubridate::month(date) < 4, lubridate::year(date) - 1, lubridate::year(date))
  year_suffix <- sprintf("%02d", year_start %% 100 + 1)
  stringr::str_c(year_start, "-", year_suffix)
}
