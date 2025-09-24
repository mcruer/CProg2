utils::globalVariables(c(
  "panel", "panel_long", "project_type", "scope_otg_added", "scope_cc_spaces_added",
  "project_name", "project_category", "location_city",
  "description_project_type", "description_child_care", "description_category",
  "primary_capital_project", "project_id", "parent_id"
))

#' Column set for Events table
#'
#' Returns the canonical column order for the Events table used by downstream helpers.
#' @return A character vector of column names.
#' @examples
#' \dontrun{
#' events_columns()
#' }
#' @export
events_columns <- function(){
  c(
    "event_id",
    "project_id",
    "parent_id",
    "event_number",
    "event_date",
    "event_type",
    "shared",
    "board_number",
    "board_name",
    "project_name",
    "panel",
    "project_type",
    "project_category",
    "Result",
    "project_description",
    "date_ori",
    "ori_sy_drop",
    "ori_fy_drop",
    "scope_otg_added",
    "scope_cc_rooms_added",
    "scope_cc_spaces_added",
    "scope_eo_rooms_added",
    "funding_total",
    "funding_cp",
    "funding_cc",
    "funding_eo",
    "funding_lp",
    "funding_edc",
    "funding_pod",
    "funding_as",
    "funding_sra",
    "funding_sci",
    "funding_fed",
    "funding_fdk",
    "funding_eer",
    "funding_ccsf",
    "funding_chmra",
    "funding_ta",
    "funding_mun",
    "funding_reg",
    "funding_prov",
    "funding_pri",
    "funding_other_sb",
    "funding_npp_gplc",
    "funding_gplr",
    "funding_eec",
    "funding_chr",
    "funding_other_min",
    "location_address",
    "location_city",
    "location_postal_code",
    "location_municipality_code",
    "location_municipality_legal",
    "location_subregion",
    "location_latitude",
    "location_longitude",
    "cmsm_dssab",
    "sfis_id",
    "campus_id",
    "building_id",
    "CP_2024_25",
    "CP_ID",
    "budget_construction_cc",
    "budget_construction_school",
    "budget_total",
    "square_footage_building",
    "square_footage_cc",
    "square_footage_demo",
    "square_footage_retrofit",
    "square_footage_school",
    "square_footage_total",
    "cost_per_sf_cc_construction",
    "cost_per_sf_school_construction",
    "cost_per_sf_total",
    "project_category_other",
    "project_type_other",
    "scope_cc_rooms_fg_added",
    "scope_cc_rooms_fg_demo",
    "scope_cc_rooms_fg_existing",
    "scope_cc_rooms_fg_net_change",
    "scope_cc_rooms_fg_reno",
    "scope_cc_rooms_fg_resulting",
    "scope_cc_rooms_infant_added",
    "scope_cc_rooms_infant_demo",
    "scope_cc_rooms_infant_existing",
    "scope_cc_rooms_infant_net_change",
    "scope_cc_rooms_infant_reno",
    "scope_cc_rooms_infant_resulting",
    "scope_cc_rooms_preschool_added",
    "scope_cc_rooms_preschool_demo",
    "scope_cc_rooms_preschool_existing",
    "scope_cc_rooms_preschool_net_change",
    "scope_cc_rooms_preschool_reno",
    "scope_cc_rooms_preschool_resulting",
    "scope_cc_rooms_toddler_added",
    "scope_cc_rooms_toddler_demo",
    "scope_cc_rooms_toddler_existing",
    "scope_cc_rooms_toddler_net_change",
    "scope_cc_rooms_toddler_reno",
    "scope_cc_rooms_toddler_resulting",
    "scope_otg_demo",
    "scope_otg_existing",
    "scope_otg_net_change",
    "scope_otg_reno",
    "scope_otg_resulting",
    "site_size_acers",
    "approval_note",
    "analyst_assessment"
  )
}

#' Column set for project-level (pt) output
#'
#' Returns the canonical column order for project-level summaries.
#' @return A character vector of column names.
#' @examples
#' \dontrun{
#' pt_columns()
#' }
#' @export
pt_columns <- function() {
  setdiff(
    events_columns(),
    c(
      "event_id",
      "event_number",
      "event_date",
      "event_type"
    )
  ) %>%
    append("date_most_recent_event", after = 2)
}


#' Retrieve Monthly Data
#'
#' This function retrieves data from the specified SQL table.
#'
#' @param as_of The date of the Monthly data to be pulled.
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the data from the specified SQL table.
#' @importFrom ezekiel ezql_table
#' @export
monthly <- function(as_of = NULL, schema = NULL, database = NULL, address = NULL) {
  ezekiel::ezql_table(table = "Monthly", as_of = as_of, schema=schema, database=database, address=address)
}


#' Retrieve Events Data
#'
#' This function retrieves data from the specified SQL table.
#'
#' @param as_of The date of the Events data to be pulled.
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the data from the specified SQL table.
#' @importFrom ezekiel ezql_table
#' @export
events <- function(as_of = NULL, schema = NULL, database = NULL, address = NULL) {
  ezekiel::ezql_table(table = "Events", as_of, schema, database, address) %>%
    select(all_of(events_columns()))
}

#' Convert Event-Level Data to Project-Level Data
#'
#' This function takes event-level data and converts it to project-level data by summarizing
#' specific columns and filling down other columns.
#'
#' @param as_of The date of the Events data to be pulled.
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the project-level data.
#' @importFrom rlang .data
#' @importFrom dplyr group_by mutate arrange summarize ungroup last across all_of any_of starts_with select
#' @importFrom tidyr fill
#' @export
pt <- function(as_of = NULL, schema = NULL, database = NULL, address = NULL) {
  events_data <- events(as_of = as_of, schema = schema, database = database, address = address) %>%
    suppressWarnings()

  columns_to_sum <- events_data %>%
    dplyr::select(dplyr::starts_with("scope"), dplyr::starts_with("funding")) %>%
    names()


  columns_to_fill_down <- events_data %>%
    #We need "any of" here because "date_most_recent_event" isn't yet included
    #in this data.
    dplyr::select(dplyr::any_of(pt_columns())) %>%
    dplyr::select(-.data$project_id, -.data$parent_id,
                  -dplyr::all_of(columns_to_sum)
                  ) %>%
    names()%>%
    #This one will be constant across the whole row, but including it here
    #for the sake of explicitness.
    append("date_most_recent_event", after = 0)

  events_data %>%
    dplyr::group_by(.data$project_id, .data$parent_id) %>%
    dplyr::mutate(date_most_recent_event = {
      if (all(is.na(.data$event_date))) as.Date(NA) else max(.data$event_date, na.rm = TRUE)
    }) %>%
    dplyr::arrange(.data$event_number, .by_group = TRUE) %>%
    tidyr::fill(dplyr::all_of(columns_to_fill_down), .direction = "down") %>%
    dplyr::summarize(
      dplyr::across(dplyr::all_of(columns_to_fill_down), dplyr::last),
      dplyr::across(dplyr::all_of(columns_to_sum), sum)
    ) %>%
    dplyr::ungroup() %>%
    select(all_of(pt_columns()))
}


#' Retrieve Frankenstein Data
#'
#' This function retrieves data from the specified SQL table.
#'
#' @param as_of The date of the Frankenstein data to be pulled.
#' @param schema The schema name (default: NULL, will use `ezql_details_schema`).
#' @param database The database name (default: NULL, will use `ezql_details_db`).
#' @param address The server address (default: NULL, will use `ezql_details_add`).
#' @return A tibble containing the data from the specified SQL table.
#' @importFrom ezekiel ezql_get
#' @export
frank <- function(as_of = NULL, schema = NULL, database = NULL, address = NULL) {
  ezekiel::ezql_table (table = "Frankenstein", as_of = as_of, schema = schema, database = database, address = address)
}

