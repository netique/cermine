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


  d <- tibble(proj = project_options)

  d <- d |>
    mutate(
      html = map(
        .data$proj,
        \(proj) {
          populate_html(html = base_html, project = proj)
        }
      )
    )

  # this comes for free, without any HTTP communication
  d |>
    mutate(
      year_opts = map(
        .data$html,
        \(html) {
          html |>
            first_form() |>
            extract_field_opts(form_fields_dict["year"])
        }
      )
    ) |>
    unnest(.data$year_opts)

  # html in the tibble returned is project-specific (two unique values),
  # use it for subsequent scraping to avoid unnecessary HTTP requests
  # (populate_html() ensures that the form is not resubmitted with the same values)
}



get_exam_events <- function(base_url = default_base_url()) {
  with_cache(get_proj_year_combinations,
    base_url = base_url,
    .key = "exam_events",
    .msg = "Extracting exam events..."
  )
}







#' Get the links to files from the CZVV website
#'
#' From the default URL,
#' ([`r default_base_url()`](`r default_base_url()`)), the function first gets all the
#' project-year combinations you can possibly set using the form fields
#' `Projekt` and `Rok`, then proceeds to fetch the HTMLs returned by the
#' server. Finally, it extracts the links to the files and returns a table. To download the files,
#' you can use `{cermine}`'s function `download_data()` on the result of `mine_links()`.
#'
#' Note that `Rok` is a slight misnomer -- it actually refers to the exam
#' "event" (usually it comes in year/season format, e.g. "2025 - Jaro" or "2025 - Podzim"),
#' however, for "Jednotné přijímací zkoušky", all terms are
#' presented under single year option. Note further that naming of `Rok` field options
#' is not consistent.
#'
#' @param projects
#' @param years
#' @param data_type Type of the data to return the links for. `item` (the default)
#'  extracts only item data for every student, `aggregated` extracts only aggregated data, and `both`
#'  returns both types.
#'
#' @param base_url Base URL of the CERMAT website. Defaults to
#'   [`r default_base_url()`](`r default_base_url()`).
#'
#' @importFrom rvest html_elements html_text html_attr
#' @importFrom tibble enframe
#' @importFrom stringr str_c
#' @importFrom dplyr select across inner_join filter
#' @importFrom purrr pmap
#'
#' @export
mine_links <- function(projects = NULL, years = NULL, data_type = "item", base_url = default_base_url()) {

  data_type <- rlang::arg_match(data_type, c("item", "agreggated", "both"))

  d <- with_cache(get_proj_year_combinations,
    base_url = base_url,
    .key = "exam_events",
    .msg = "Extracting exam events..."
  )

  if (!is.null(projects)) {
    if (!all(projects %in% unique(d$proj))) {
      cli::cli_abort("Some of the {.arg projects} are not available. Possible values are {.val {unique(d$proj)}}.")
    }
    d <- d |> filter(proj %in% projects)
  }

  if (!is.null(years)) {
    if (!all(years %in% unique(d$year_opts))) {
      cli::cli_abort("Some of the {.arg years} are not available for {.val {unique(d$proj)}} project{?s}. Possible values are {.val {unique(d$year_opts)}}.")
    }
    d <- d |> filter(year_opts %in% years)

    if (length(d$proj) == 1L) {
      cli::cli_alert_warning("For {.val {unique(d$year_opts)}} {.arg years} option{?s}, only the {.val {unique(d$proj)}} project was kept.")
    }
  }

  if (nrow(d) == 0L) {
    cli::cli_abort("No data available for the selected project-year combination.")
  }

  # this is the most HTTP expensive operation

  d <- d |> mutate(hash = map_chr(str_c(proj, year_opts), rlang::hash))

    d <- d |>
      mutate(
        html = pmap(
          list(.data$html, .data$proj, .data$year_opts, .data$hash),
          \(html, proj, year_opts, hash) {
            with_cache(populate_html,html = html, project = proj, year = year_opts, .key = hash)
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
  d <- d |> select(-"html")

  d <- d |> unnest("files")

  d <- d |> filter(.data$data_type == .env$data_type)

  d |>
    mutate(
      file_url = str_c(base_url, .data$file_href),
      across(c("proj", "year_opts", "data_type"), as.factor)
    ) |>
    select(-"file_href")
}



download_data <- function() {
  # of class `cermine_data`
}
