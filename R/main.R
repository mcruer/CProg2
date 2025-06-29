utils::globalVariables(c(
  "panel", "panel_long", "project_type", "scope_otg_added", "scope_cc_spaces_added",
  "project_name", "project_category", "location_city",
  "description_project_type", "description_child_care", "description_category",
  "primary_capital_project", "project_id", "parent_id"
))

#' Retrieve Monthly Data
#'
#' This function retrieves data from the specified SQL table.
#'
#' @param sql_table_name The name of the SQL table (default: "Monthly").
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the data from the specified SQL table.
#' @importFrom ezekiel ezql_table
#' @export
monthly <- function(sql_table_name = "Monthly", schema = NULL, database = NULL, address = NULL) {
  ezekiel::ezql_table(table = sql_table_name, schema, database, address)
}


#' Retrieve Events Data
#'
#' This function retrieves data from the specified SQL table.
#'
#' @param sql_table_name The name of the SQL table (default: "Events").
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the data from the specified SQL table.
#' @importFrom ezekiel ezql_table
#' @export
events <- function(sql_table_name = "Events", schema = NULL, database = NULL, address = NULL) {
  ezekiel::ezql_table(table = sql_table_name, schema, database, address)
}

#' Convert Event-Level Data to Project-Level Data
#'
#' This function takes event-level data and converts it to project-level data by summarizing
#' specific columns and filling down other columns.
#'
#' @param sql_table_name The name of the SQL table (default: "Events").
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the project-level data.
#' @importFrom dplyr select starts_with all_of group_by summarize ungroup last
#' @importFrom tidyr fill
#' @export
pt <- function(sql_table_name = "Events", schema = NULL, database = NULL, address = NULL) {
  events_data <- events(sql_table_name, schema = schema, database = database, address = address)

  columns_to_sum <- events_data %>%
    dplyr::select(dplyr::starts_with("scope"), dplyr::starts_with("funding")) %>%
    names()

  columns_to_exclude_from_pt <- c(
    "event_date",
    "event_type",
    "atp_amount",
    "approval_note"
  )

  columns_to_fill_down <- events_data %>%
    dplyr::select(-project_id, -parent_id,
                  -dplyr::all_of(columns_to_sum),
                  -dplyr::all_of(columns_to_exclude_from_pt)) %>%
    names()

  events_data %>%
    dplyr::group_by(project_id, parent_id) %>%
    tidyr::fill(dplyr::all_of(columns_to_fill_down), .direction = "down") %>%
    dplyr::summarize(
      dplyr::across(dplyr::all_of(columns_to_fill_down), dplyr::last),
      dplyr::across(dplyr::all_of(columns_to_sum), sum)
    ) %>%
    dplyr::ungroup()
}


#' Retrieve Frankenstein Data
#'
#' This function retrieves data from the specified SQL table.
#'
#' @param sql_table_name The name of the SQL table (default: "Frankenstein").
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the data from the specified SQL table.
#' @importFrom ezekiel ezql_get
#' @export
frank <- function(sql_table_name = "Frankenstein", schema = NULL, database = NULL, address = NULL) {
  ezekiel::ezql_table (table = sql_table_name, schema = schema, database = database, address = address)
}

