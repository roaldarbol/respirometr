#' Read Licor data
#'
#' @description
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
    df <- read_licor_li7000(filepath)
    ensure_column_types(df)
  } else if (model == "850"){
    df <- read_licor_li850(filepath)
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
