# CProg2

CProg2 is an R package for retrieving, reshaping, and describing school **capital
program** data. It reads project tables from a SQL backend (through the
[`ezekiel`](https://github.com/mcruer/ezekiel) package), rolls event-level
records up to the project level, and generates standardized, bilingual
(English / French) project descriptions for reporting and communications.

## Overview

The package is organized around a handful of tasks:

- **Pulling tables** — thin wrappers around `ezekiel::ezql_table()` that fetch
  named tables (`Events`, `Monthly`, `Boards`, `Contacts`, `Commitments`,
  `Frankenstein`, etc.), with optional `as_of` point-in-time snapshots.
- **Reshaping data** — converting event-level data into project-level summaries,
  summing scope/funding columns and filling down descriptive columns.
- **Describing projects** — building consistent English and French narrative
  descriptions of each project from its type, panel, pupil places, child care
  spaces, category, and location.
- **Utilities** — small helpers for school/fiscal years and child care space
  estimates.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mcruer/CProg2")
```

CProg2 depends on two companion packages that are also installed from GitHub:

```r
remotes::install_github("mcruer/ezekiel")
remotes::install_github("mcruer/gplyr")
```

Other dependencies (`dplyr`, `tidyr`, `purrr`, `stringr`, `lubridate`, `rlang`,
`magrittr`) are available from CRAN.

## Data access functions

These functions retrieve tables from the SQL backend. Each accepts an optional
`as_of` date for point-in-time snapshots, and optional `schema`, `database`, and
`address` arguments (which default to the connection details configured for
`ezekiel`).

| Function             | Table retrieved      | Notes |
|----------------------|----------------------|-------|
| `monthly()`          | `Monthly`            | |
| `monthly_all()`      | `monthly_all`        | |
| `boards()`           | `boards`             | |
| `contacts()`         | `contacts`           | |
| `commitments()`      | `commitments`        | |
| `commitments_all()`  | `commitments_all`    | |
| `events()`           | `Events`             | Returns the `events_columns()` set unless `everything = TRUE`. |
| `frank()`            | `Frankenstein`       | Returns the `frank_columns()` set unless `everything = TRUE`. |

The canonical column sets are exposed via `events_columns()`, `pt_columns()`,
and `frank_columns()`.

```r
# Pull the current Events table (default column set)
ev <- events()

# Pull a point-in-time snapshot with every column
ev_full <- events(as_of = "2024-04-01", everything = TRUE)
```

## Event-level to project-level

`pt()` converts event-level data into one row per project. Scope and funding
columns (those prefixed `scope` and `funding`) are summed across a project's
events, while descriptive columns are filled down and the last value retained. A
`date_most_recent_event` column is added.

```r
projects <- pt(as_of = "2024-04-01")
```

`comms_data()` builds a filtered project list for communications use — keeping
primary capital projects, optionally restricting to projects with an ORI date
after June 1, 2018, dropping cancelled and specific excluded projects, and
flagging child care and French-language-board projects.

## Project descriptions

`add_description_e()` and `add_description_f()` add a narrative description
column (`description_e` / `description_f`) to a project-level tibble. Each
expects the following columns: `panel`, `project_type`, `scope_otg_added`,
`scope_cc_spaces_added`, `project_name`, `project_category`, and
`location_city`.

```r
library(CProg2)

tibble::tibble(
  panel = "ELE",
  project_type = "Addition",
  scope_otg_added = 138,
  scope_cc_spaces_added = 24,
  project_name = "Maple School Addition",
  project_category = "Accommodation Pressure",
  location_city = "toronto"
) |>
  add_description_e() |>
  dplyr::pull(description_e)
#> "An addition at Maple School adding 138 pupil places
#>  (including 24 licenced child care spaces) to address
#>  accommodation pressure in Toronto."
```

`add_description_f()` produces the equivalent French description.

## Utility functions

| Function                  | Description |
|---------------------------|-------------|
| `school_year(date)`       | School year (Sep–Aug) as `"YYYY-YY"`. |
| `fiscal_year(date)`       | Fiscal year (Apr–Mar) as `"YYYY-YY"`. |
| `cc_spaces_infant(rooms)` | Estimated infant child care spaces (`rooms * 10`). |
| `cc_spaces_toddler(rooms)`| Estimated toddler child care spaces (`rooms * 15`). |
| `cc_spaces_preschool(rooms)` | Estimated preschool child care spaces (`rooms * 24`). |
| `cc_spaces_fg(rooms)`     | Estimated family-group child care spaces (`rooms * 15`). |

```r
school_year("2024-05-15")   # "2023-24"
fiscal_year("2023-04-01")   # "2023-24"
cc_spaces_toddler(3)        # 45
```

The pipe operator `%>%` is re-exported from `magrittr`.

## License

MIT. See [LICENSE](LICENSE.md).
