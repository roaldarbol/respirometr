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
#' @importFrom purrr map
#' @importFrom stats na.omit
#'
#' @return Tidy LiCor data
#' @export
read_licor <- function(filepath, model = "7000") {
  if (model == "7000"){
    df <- read_li7000(filepath)
  } else if (model == "850"){
    df <- read_li850(filepath)
  }

  # Avoids issues when column names have been loaded partway through
  df <- df |>
    stats::na.omit() |>
    dplyr::mutate_all(as.numeric)

  return(df)
}

#' Read from LI-7000
#' @inheritParams read_licor
read_li7000 <- function(filepath){
  filepath |>
    read_tsv(skip = 2, name_repair = janitor::make_clean_names)
}

#' Read from LI-7000
#' @inheritParams read_licor
read_li850 <- function(filepath){
  filepath |>
    read_tsv(skip = 1,
             name_repair = janitor::make_clean_names,
             show_col_types = FALSE) |>
    suppressWarnings() |>
    dplyr::select(where(.not_any_na)) |>
    dplyr::mutate(time_s = .data$system_time_h_m_s - min(.data$system_time_h_m_s)) |>
    dplyr::relocate("time_s", .before = 1)
}
