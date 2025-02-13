library(tidyverse)
library(here)
library(rvest)

base_url <- "https://maturita.cermat.cz/menu/testy-a-zadani-z-predchozich-obdobi/anglicky-jazyk/testy-a-zadani-anglicky-jazyk"

base_html <- read_html(base_url)

file_links <- base_html |> html_elements("a.wf_file")

file_links_tbl <- file_links |>
  map(
    \(x) {
      tibble(
        h1_title = x |> html_element(xpath = "preceding::h1[1]") |> html_text(),
        h2_title = x |> html_element(xpath = "preceding::h2[1]") |> html_text(),
        h3_title = x |> html_element(xpath = "preceding::h3[1]") |> html_text(),
        link_text = x |> html_text(),
        href = x |> html_attr("href")
      )
    }
  ) |>
  list_rbind()

file_links_tbl <- file_links_tbl |>
  mutate(
    url = url_absolute(href, base_url),
    across(c(contains("title"), link_text), str_squish),
    year = str_extract(h2_title, "\\d{4}"),
    season = str_extract(h2_title, regex("jaro|podzim", ignore_case = TRUE)),
    term = case_when(
      str_detect(h3_title, regex("mimo", ignore_case = TRUE)) ~ "mimořádný",
      .default = "řádný"
    )
  ) |>
  select(exam = h1_title, year, season, term, file_description = link_text, file_url = url)

# only ordinary term, only the assignments, only spring season
files_to_download <- file_links_tbl |>
  filter(
    season == "jaro",
    term == "řádný",
    str_detect(file_description, "[Z|z]ad[a|á]n[í|i]*.didaktick")
  )

files_to_download <- files_to_download |>
  mutate(destfile = here(
    "data/input/cermat/pdfs",
    # make a sensible filename (and append the original to be sure)
    str_c(exam, year, season, term, file_description,
      fs::path_file(file_url),
      sep = " "
    ) |> str_replace_all(" ", "_")
  ))

files_to_download$destfile |>
  fs::path_dir() |>
  unique() |>
  fs::dir_create()

files_to_download |>
  pmap(
    \(file_url, destfile, ...) {
      download.file(file_url, destfile)
    }
  )

