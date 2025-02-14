# chaching environment to prevent excessive HTTP requests
cache <- new.env(parent = emptyenv())


#' @importFrom purrr keep_at map_chr
#' @importFrom rlang := .data
#' @importFrom rvest html_form_set html_form_submit read_html
#'
populate_html <- function(html, base_url = default_base_url(), project = NULL, year = NULL) {
  orig_form <- first_form(html, base_url = base_url)

  orig_form_input <- get_form_input_values(orig_form)

  new_form <- orig_form |>
    html_form_set(
      r"({form_fields_dict["project"]})" := project %||% orig_form_input[form_fields_dict["project"]],
      r"({form_fields_dict["year"]})" := year %||% orig_form_input[form_fields_dict["year"]]
    )

  new_form_input <- get_form_input_values(new_form)

  # when the form is not changed, return the original html, already populated with the links
  # otherwise ASP.NET server won't return any links (for the second time with the same values)
  if (identical(orig_form_input, new_form_input)) {
    return(html)
  }

  if (be_verbose()) {
    cli::cli_progress_step("Fetching HTML with links for {.strong {project}} / {.emph {year}}...")
  }

  resp <- new_form |> html_form_submit()
  read_html(resp)
}




#' @importFrom rvest read_html
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom dplyr mutate last_col
#'
get_proj_year_combinations <- function(base_url = default_base_url()) {
  base_html <- read_html(base_url)
  base_form <- first_form(base_html, base_url = base_url)

  project_options <- base_form |> extract_field_opts(form_fields_dict["project"])


  d <- tibble(project = project_options)

  d <- d |>
    mutate(
      html = map(
        .data$project,
        \(project) {
          populate_html(html = base_html, project = project)
        }
      )
    )

  # this comes for free, without any HTTP communication
  d |>
    mutate(
      year = map(
        .data$html,
        \(html) {
          html |>
            first_form() |>
            extract_field_opts(form_fields_dict["year"])
        }
      )
    ) |>
    unnest(.data$year)

  # html in the tibble returned is project-specific (two unique values),
  # use it for subsequent scraping to avoid unnecessary HTTP requests
  # (populate_html() ensures that the form is not resubmitted with the same values)
}


#' Get the exam events from the CZVV website
#'
#' Fetches all the project-year combinations you can possibly set using the form
#' fields at CZVV site [`r default_base_url()`](`r default_base_url()`). Useful
#' if you plan to mine only a subset of the data in `mine_links()`.
#'
#' @param force Force a refresh of the data, even if cached. Defaults to
#'   `FALSE`.
#' @param base_url Base URL of the CERMAT website. Defaults to
#'   [`r default_base_url()`](`r default_base_url()`). Can be set session-wide
#'   with `options(cermine.base_url = <your_url>)`.
#'
#' @returns A tibble with the project and year options returned by the server.
#' @export
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' get_exam_events()
#' }
#'
get_exam_events <- function(force = FALSE, base_url = default_base_url()) {
  out <- with_cache(
    get_proj_year_combinations,
    base_url = base_url,
    .key = "exam_events",
    .msg = "Extracting exam events...",
    force = force
  )

  out |> select(-"html")
}



#' Get the links to files from the CZVV website
#'
#' From the default URL, ([`r default_base_url()`](`r default_base_url()`)), the
#' function first gets all the project-year combinations you can possibly set
#' using the form fields `Projekt` and `Rok`, then proceeds to fetch the HTMLs
#' returned by the server. Finally, it extracts the links to the files and
#' returns a `tibble` with project, year, data type (either "item" or
#' "aggregated") and the link to the data file you can use to download the data.
#'
#' Note that `Rok` is a slight misnomer -- it actually refers to the exam
#' "event" (usually it comes in year/season format, e.g. "2025 - Jaro" or
#' "2025 - Podzim"), however, for "Jednotné přijímací zkoušky", all terms are
#' presented under single year option. Note further that naming of `Rok` field
#' options is not consistent.
#'
#' @param projects Projects to extract the links for. If `NULL` (the default),
#'   all projects available are used. Possible values are `Maturitní zkoušky`
#'   and `Jednotná přijímací zkouška` (but that may be a subject to change).
#'   You'll get the up-to-date options listed if you type in incorrect
#'   option(s). To get the current options, you may also call
#'   `get_exam_events()` first and use the `project` column from the result.
#' @param years Years to extract the links for. If `NULL` (the default), all
#'   years available are used. You'll get the up-to-date options listed if you
#'   type in incorrect option(s). To get the current options, you may also call
#'   `get_exam_events()` first and use the `year` column from the result. Note
#'   that if you opt for some impossible combination of project and year, the
#'   function will inform you.
#' @param data_type Type of the data to return the links for. `item` (the
#'   default) extracts only item data for every student, `aggregated` extracts
#'   only aggregated data, and `both` returns both types.
#' @param force Force a refresh of the data, even if cached. Defaults to
#'   `FALSE`.
#' @param base_url Base URL of the CERMAT website. Defaults to
#'   [`r default_base_url()`](`r default_base_url()`).
#'
#' @importFrom rvest html_elements html_text html_attr
#' @importFrom tibble enframe
#' @importFrom stringr str_c
#' @importFrom dplyr select across inner_join filter
#' @importFrom purrr pmap
#' @importFrom rlang .env hash
#'
#' @return A tibble with the `project`, `year`, `data_type`, `file_name`, and `file_url`.
#'
#' @export
mine_links <- function(projects = NULL, years = NULL, data_type = "item", force = FALSE, base_url = default_base_url()) {
  data_type <- rlang::arg_match(data_type, c("item", "agreggated", "both"))
  projects <- unique(projects)
  years <- unique(years)

  d <- with_cache(get_proj_year_combinations,
    base_url = base_url,
    .key = "exam_events",
    .msg = "Extracting exam events...",
    force = force
  )

  if (!is.null(projects)) {
    if (!all(projects %in% unique(d$project))) {
      cli::cli_abort("Some of the {.arg projects} are not available. Possible values are {.val {unique(d$project)}}.")
    }
    d <- d |> filter(.data$project %in% projects)
  }

  if (!is.null(years)) {
    if (!all(years %in% unique(d$year))) {
      cli::cli_abort("Some of the {.arg years} are not available for {.val {unique(d$project)}} project{?s}. Possible values are {.val {unique(d$year)}}.")
    }
    d <- d |> filter(.data$year %in% years)

    if (length(d$project) == 1L) {
      cli::cli_alert_warning("For {.val {unique(d$year)}} {.arg years} option{?s}, only the {.val {unique(d$project)}} project was kept.")
    }
  }

  if (nrow(d) == 0L) {
    cli::cli_abort("No data available for the selected project-year combination.")
  }

  # this is the most HTTP expensive operation, so cache based on project/year combo
  # we'll use 128-bit hash, so collisions are possible, but very unlikely (+ we add prefix)
  d <- d |> mutate(.hash = map_chr(str_c(.data$project, .data$year), \(x) str_c("final_html_", hash(x))))

  d <- d |>
    mutate(
      html = pmap(
        list(.data$html, .data$project, .data$year, .data$.hash),
        \(html, project, year, .hash) {
          with_cache(populate_html, html = html, project = project, year = year, .key = .hash, force = force)
        },
        .progress = "Fetching HTMLs with links..."
      )
    )

  # this is HTTP free
  d <- d |>
    mutate(
      files = map(.data$html, \(html) {
        c(
          agreggated = html_elements(html, "#dataAgregovanaSkryte"),
          item = html_elements(html, "#dataNeagregovanaSkryte")
        ) |>
          enframe("data_type", "node") |>
          mutate(
            nodeset = map(.data$node, \(x) html_elements(x, "a[target=\"frmFile\"]")),
            file_name = map(.data$nodeset, html_text),
            file_href = map(.data$nodeset, \(x) html_attr(x, "href")),
          ) |>
          select(-"node", -"nodeset") |>
          unnest(c("file_name", "file_href"))
      })
    )

  # drop the html, we are done with it
  d <- d |> select(-"html", -".hash")

  d <- d |> unnest("files")

  if (data_type == "both") {
    data_type <- c("item", "agreggated")
  }

  d <- d |> filter(.data$data_type == .env$data_type)

  d |>
    mutate(
      file_url = str_c(base_url, .data$file_href),
      across(c("project", "year", "data_type"), as.factor)
    ) |>
    select(-"file_href")
}
