#' Read Licor data
#'
#' @param filepath Path to file
#' @param model LiCor model
#'
#' @importFrom janitor make_clean_names
#' @importFrom readr read_tsv
#' @importFrom purrr map
#' @importFrom dplyr mutate_all
#'
#' @return Tidy LiCor data
#' @export
#'
read_licor <- function(filepath, model = "LI7000") {
  if (model == "LI7000"){
    df <- filenames |>
      purrr::map(~ read_tsv(.x, skip = 2, name_repair = janitor::make_clean_names))
  }

  # Avoids issues when column names have been loaded partway through
  df <- df |>
    na.omit() |>
    dplyr::mutate_all(as.numeric)

  return(df)
}
