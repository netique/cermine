default_base_url <- function() {
  getOption("cermine.base_url", default = "https://vysledky.cermat.cz/statistika/")
}

be_verbose <- function() {
  op <- as.logical(getOption("cermine.verbose", default = FALSE))
  if (is.na(op)) {
    cli::cli_alert_warning("{.field cermine.verbose} option has to be either {.val TRUE} or {.val FALSE}, defaulting to {.val FALSE}.")
    return(FALSE)
  }
  op
}



form_fields_dict <- c(
  project = "ctl00$LeftContent$ddProjekt",
  year = "ctl00$LeftContent$ddRok"
)


extract_field_opts <- function(form, field) {
  form$fields[[field]]$options
}

#' @importFrom rvest html_form
#' @importFrom purrr pluck
#'
first_form <- function(html, base_url = default_base_url()) {
  html |>
    html_form(base_url = base_url) |>
    pluck(1L)
}

get_form_input_values <- function(form) {
  form[["fields"]] |>
    keep_at(form_fields_dict) |>
    map_chr("value")
}




with_cache <- function(.f, ..., .key, .msg = NULL, force = FALSE) {
  if (!force && exists(.key, envir = cache)) {
    # cli::cli_alert_info("Returning cached results...")
    return(get(.key, envir = cache))
  }

  res <- .f(...)

  if (!is.null(.msg)) {
    cli::cli_progress_step(.msg, msg_done = paste0("Done ", tolower(.msg)))
  }

  if (be_verbose()) {
    cli::cli_progress_step("Caching to {.val {(.key)}}...")
  }

  assign(.key, res, envir = cache)

  res
}



`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
