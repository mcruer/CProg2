#' @keywords internal
get_panel_long_e <- function(panel) {
  case_when(
    panel == "ELE" ~ "elementary",
    panel == "SEC" ~ "secondary",
    panel == "ELE/SEC" ~ "elementary/secondary",
    TRUE ~ ""
  )
}

#' @keywords internal
get_description_project_type_e <- function(project_type, scope_otg_added, panel_long, project_name) {
  case_when(
    project_type == "New Construction" ~ str_c(
      "A new ",
      if_else(scope_otg_added > 0, str_c(scope_otg_added, " pupil place "), ""),
      panel_long, " school (", project_name, ") "
    ),
    project_type == "Addition" ~ str_c(
      "An addition at ", project_name,
      if_else(scope_otg_added > 0, str_c(" adding ", scope_otg_added, " pupil places "), "")
    ),
    project_type == "Retrofit" ~ str_c(
      "A retrofit at ", project_name,
      if_else(scope_otg_added > 0, str_c(" adding ", scope_otg_added, " pupil places "), "")
    ),
    project_type == "Purchase" ~ str_c(
      "The purchase of a ", panel_long, " school",
      if_else(scope_otg_added > 0, str_c(" adding ", scope_otg_added, " pupil places "), "")
    ),
    project_type == "Demolition" ~ str_c("A demolition at ", project_name, " "),
    project_type == "Land Acquisition" ~ str_c("A land acquisition at ", project_name, " "),
    TRUE ~ str_c(
      "A project at ", project_name,
      if_else(scope_otg_added > 0, str_c(" adding ", scope_otg_added, " pupil places "), "")
    )
  )
}

#' @keywords internal
get_description_child_care_e <- function(scope_cc_spaces_added) {
  case_when(
    scope_cc_spaces_added > 0 ~ str_c("(including ", scope_cc_spaces_added, " licenced child care spaces) "),
    TRUE ~ ""
  )
}

#' @keywords internal
get_description_category_e <- function(project_category, location_city) {
  city_part <- if_else(!is.na(location_city), str_c(" in ", location_city), "")
  case_when(
    project_category == "ASD Pilot" ~ str_c("to address ASD needs", city_part, "."),
    project_category == "Accommodation Pressure" ~ str_c("to address accommodation pressure", city_part, "."),
    project_category %in% c("Child Care", "Community Based Child Care") ~ str_c("to address child care needs", city_part, "."),
    project_category == "Condition" ~ str_c("to address facility condition", city_part, "."),
    project_category == "Consolidation" ~ str_c("to consolidate local schools", city_part, "."),
    project_category == "FDK" ~ str_c("to provide FDK capacity", city_part, "."),
    project_category %in% c("FL Capital Transitional", "French Language Access") ~ str_c("to address French Language Access", city_part, "."),
    TRUE ~ str_c(city_part)
  ) %>%
    str_trim()
}


#' Add an English Project Description Column
#'
#' Creates a descriptive English-language summary of a school capital project based on key inputs
#' such as project type, number of pupil places added, school panel, project name,
#' number of child care spaces added, and project category.
#'
#' The function standardizes project summaries into a consistent format used in reporting
#' and communication materials. It also formats city names in title case and removes redundant
#' occurrences of the word "Addition" in project names (e.g., "Maple School Addition" becomes "Maple School").
#'
#' @param pt A tibble or data frame containing project-level information with the following required columns:
#' \describe{
#'   \item{panel}{Character. One of "ELE", "SEC", or "ELE/SEC".}
#'   \item{project_type}{Character. One of: "New Construction", "Addition", "Retrofit", "Purchase", "Demolition", or "Other".}
#'   \item{scope_otg_added}{Numeric. Number of pupil places being added.}
#'   \item{scope_cc_spaces_added}{Numeric. Number of licensed child care spaces being added.}
#'   \item{project_name}{Character. Name of the project. Trailing " Addition" is removed automatically.}
#'   \item{project_category}{Character. E.g., "Accommodation Pressure", "FDK", "Condition", etc.}
#'   \item{location_city}{Character. Municipality where the project is located. Will be title-cased automatically.}
#' }
#'
#' @return A tibble with all original columns and a new column:
#' \describe{
#'   \item{description_e}{A formatted English project description string.}
#' }
#'
#' @details
#' Project names are sanitized by removing trailing " Addition" and collapsing extra spaces.
#' Internally uses helper functions to expand panel codes, construct description phrases,
#' append child care information, and provide project context by category and location.
#'
#' @importFrom dplyr mutate case_when if_else select
#' @importFrom stringr str_c str_remove str_squish str_replace str_to_title str_trim
#' @importFrom magrittr %>%
#' @importFrom purrr pmap_chr map discard
#' @importFrom tidyr replace_na
#'
#' @examples
#' \dontrun{
#' tibble::tibble(
#'   panel = "ELE",
#'   project_type = "Addition",
#'   scope_otg_added = 138,
#'   scope_cc_spaces_added = 24,
#'   project_name = "Maple School Addition",
#'   project_category = "Accommodation Pressure",
#'   location_city = "toronto"
#' ) %>%
#'   add_description_e()
#' }
#'
#' @export
add_description_e <- function(pt) {
  pt %>%
    mutate(
      panel_long = get_panel_long_e(panel),
      description_project_type = get_description_project_type_e(project_type,
                                                                scope_otg_added,
                                                                panel_long,
                                                                project_name %>%
                                                                  str_remove(" Addition$") %>%
                                                                  str_squish()
      ),
      description_child_care = get_description_child_care_e(scope_cc_spaces_added),
      description_category = get_description_category_e(project_category, location_city),
      description_e = if_else(is.na(project_name)|panel == "OFF-ADM", NA_character_,
                              pmap_chr(
                                list(description_project_type, description_child_care, description_category) %>%
                                  map(replace_na, ""),
                                ~ c(..1, ..2, ..3) %>%
                                  discard(~ .x == "") %>%
                                  str_c(collapse = " ") %>%
                                  str_replace_all("\\)\\s*\\(", ", ") %>%
                                  str_squish()
                              ))
    ) %>%
    select(-panel_long, -description_project_type, -description_child_care, -description_category)
}



#' @keywords internal
get_panel_long_f <- function(panel) {
  case_when(
    panel == "ELE" ~ "élémentaire",
    panel == "SEC" ~ "secondaire",
    panel == "ELE/SEC" ~ "élémentaire/secondaire",
    TRUE ~ ""
  )
}

#' @keywords internal
get_description_project_type_f <- function(project_type, scope_otg_added, panel_long, project_name) {
  case_when(
    project_type == "New Construction" ~ str_c(
      "Une nouvelle école ",
      panel_long, " de ",
      if_else(scope_otg_added > 0, str_c(scope_otg_added, " places "), ""),
      "(", project_name, ") "
    ),
    project_type == "Addition" ~ str_c(
      "Un agrandissement à ", project_name,
      if_else(scope_otg_added > 0, str_c(" ajoutant ", scope_otg_added, " places "), "")
    ),
    project_type == "Retrofit" ~ str_c(
      "Une rénovation à ", project_name,
      if_else(scope_otg_added > 0, str_c(" ajoutant ", scope_otg_added, " places "), "")
    ),
    project_type == "Purchase" ~ str_c(
      "L'achat d'une école ", panel_long,
      if_else(scope_otg_added > 0, str_c(" ajoutant ", scope_otg_added, " places "), "")
    ),
    project_type == "Demolition" ~ str_c("Une démolition à ", project_name, " "),
    project_type == "Land Acquisition" ~ str_c("L'acquisition d'un terrain à ", project_name, " "),
    TRUE ~ str_c(
      "Un projet à ", project_name,
      if_else(scope_otg_added > 0, str_c(" ajoutant ", scope_otg_added, " places "), "")
    )
  )
}

#' @keywords internal
get_description_child_care_f <- function(scope_cc_spaces_added) {
  case_when(
    scope_cc_spaces_added > 0 ~ str_c("(y compris ", scope_cc_spaces_added, " places en services de garde agréés) "),
    TRUE ~ ""
  )
}

#' @keywords internal
get_description_category_f <- function(project_category, location_city) {
  city_part <- if_else(!is.na(location_city), str_c(" à ", location_city), "")
  case_when(
    project_category == "ASD Pilot" ~ str_c("pour répondre aux besoins liés aux TSA", city_part, "."),
    project_category == "Accommodation Pressure" ~ str_c("pour répondre à la pression d'accueil", city_part, "."),
    project_category %in% c("Child Care", "Community Based Child Care") ~ str_c("pour répondre aux besoins en services de garde", city_part, "."),
    project_category == "Condition" ~ str_c("pour améliorer l'état des installations", city_part, "."),
    project_category == "Consolidation" ~ str_c("pour regrouper des écoles locales", city_part, "."),
    project_category == "FDK" ~ str_c("pour offrir des places en Maternelle à temps plein", city_part, "."),
    project_category %in% c("FL Capital Transitional", "French Language Access") ~ str_c("pour améliorer l'accès à l'éducation en langue française", city_part, "."),
    TRUE ~ str_c(city_part)
  ) %>%
    str_trim()
}

#' Add a French Project Description Column
#'
#' Creates a French-language project summary based on project type, panel, pupil places added,
#' child care spaces, and project category. This function is designed for internal reporting
#' and generates standardized, grammatically correct descriptions in French.
#'
#' @param pt A tibble or data frame with project-level information. Must include the following columns:
#' \describe{
#'   \item{panel}{Character. One of "ELE", "SEC", or "ELE/SEC".}
#'   \item{project_type}{Character. One of: "New Construction", "Addition", "Retrofit", "Purchase", "Demolition", or "Other".}
#'   \item{scope_otg_added}{Numeric. Number of pupil places being added.}
#'   \item{scope_cc_spaces_added}{Numeric. Number of licensed child care spaces being added.}
#'   \item{project_name}{Character. Name of the project (school/site). Trailing " Addition" is removed automatically.}
#'   \item{project_category}{Character. E.g., "Accommodation Pressure", "FDK", "Condition", "French Language Access", etc.}
#'   \item{location_city}{Character. Municipality where the project is located. Will be title-cased automatically.}
#' }
#'
#' @return A tibble with all original columns and a new column:
#' \describe{
#'   \item{description_f}{A formatted project description in French.}
#' }
#'
#' @details
#' Project names are sanitized by removing trailing " Addition" and collapsing extra spaces.
#' Internally uses helper functions to translate panel codes, construct project phrases,
#' append child care details, and contextualize the location.
#'
#' @importFrom dplyr mutate case_when if_else select
#' @importFrom stringr str_c str_remove str_squish str_replace str_to_title str_trim
#' @importFrom magrittr %>%
#' @importFrom purrr pmap_chr map discard
#' @importFrom tidyr replace_na
#'
#' @examples
#' \dontrun{
#' tibble::tibble(
#'   panel = "ELE",
#'   project_type = "Addition",
#'   scope_otg_added = 162,
#'   scope_cc_spaces_added = 88,
#'   project_name = "Janet Lee Elementary School Addition",
#'   project_category = "Accommodation Pressure",
#'   location_city = "stoney creek"
#' ) %>%
#'   add_description_f() %>%
#'   dplyr::pull(description_f)
#' }
#'
#' @export
add_description_f <- function(pt) {
  pt %>%
    mutate(
      panel_long = get_panel_long_f(panel),
      description_project_type = get_description_project_type_f(
        project_type,
        scope_otg_added,
        panel_long,
        project_name %>%
          str_remove(" Addition$") %>%
          str_squish()
      ),
      description_child_care = get_description_child_care_f(scope_cc_spaces_added),
      description_category = get_description_category_f(project_category, location_city),
      description_f = if_else(is.na(project_name)|panel == "OFF-ADM", NA_character_,
                              pmap_chr(
                                list(description_project_type, description_child_care, description_category) %>%
                                  map(replace_na, ""),
                                ~ c(..1, ..2, ..3) %>%
                                  discard(~ .x == "") %>%
                                  str_c(collapse = " ") %>%
                                  str_replace_all("\\)\\s*\\(", ", ") %>%
                                  str_squish()
                              ))
    ) %>%
    select(-panel_long, -description_project_type, -description_child_care, -description_category)
}
