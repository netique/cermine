% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mine.R
\name{get_exam_events}
\alias{get_exam_events}
\title{Get the exam events from the CZVV website}
\usage{
get_exam_events(force = FALSE, base_url = default_base_url())
}
\arguments{
\item{force}{Force a refresh of the data, even if cached. Defaults to
\code{FALSE}.}

\item{base_url}{Base URL of the CERMAT website. Defaults to
\url{https://vysledky.cermat.cz/statistika/}. Can be set session-wide
with \verb{options(cermine.base_url = <your_url>)}.}
}
\value{
A tibble with the project and year options returned by the server.
}
\description{
Fetches all the project-year combinations you can possibly set using the form
fields at CZVV site \url{https://vysledky.cermat.cz/statistika/}. Useful
if you plan to mine only a subset of the data in \code{mine_links()}.
}
\examples{
\dontrun{
get_exam_events()
}

}
