#' Read Licor data
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Read respirometry data from a variety of Licor devices.
#'
#' @param filepath Path to file
#' @param model LiCor model
#'
#' @import dplyr
#' @importFrom janitor make_clean_names
#' @importFrom readr read_tsv
#'
#' @return Tidy LiCor data
#' @export
read_licor <- function(filepath, model) {
  if (model == "7000"){
    df <- read_li7000(filepath)
    ensure_column_types(df)
  } else if (model == "850"){
    df <- read_li850(filepath)
    ensure_column_types(df)
  }

  # Avoids issues when column names have been loaded partway through
  # This really should be left as a decision for the user
  # df <- df |>
  #   stats::na.omit() |>
  #   dplyr::mutate_all(as.numeric)

  # Relocate the time column
  df <- df |>
    dplyr::relocate("time", .before = 1)

  return(df)
}

#' Read from LI-7000
#' @description
#' `r lifecycle::badge('experimental')`
#' @inheritParams read_licor
#' @keywords internal
read_li7000 <- function(filepath){
  filepath |>
    readr::read_tsv(skip = 2,
             name_repair = janitor::make_clean_names,
             show_col_types = FALSE) |>
    dplyr::rename(time = "time_s")
}

#' Read from LI-850
#' @description
#' `r lifecycle::badge('experimental')`
#' @inheritParams read_licor
#' @importFrom hms as_hms
#' @keywords internal
read_li850 <- function(filepath){
  filepath |>
    readr::read_tsv(skip = 1,
             name_repair = janitor::make_clean_names,
             show_col_types = FALSE) |>
    suppressWarnings() |>
    dplyr::select(where(.not_any_na)) |>
    dplyr::mutate(system_time_h_m_s = hms::as_hms(.data$system_time_h_m_s)) |>
    dplyr::mutate(time = .data$system_time_h_m_s - min(.data$system_time_h_m_s))
}
