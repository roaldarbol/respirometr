#' Normalise Traces
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' @param data Data from trial
#' @param pre Pre-trial level. Case be either a single value (if known) or a vector of values, from which the mean is calculated.
#' @param post Post-trial level. Case be either a single value (if known) or a vector of values, from which the mean is calculated.
#' @param method Which method is used to normalise the data
#'
#' @return Normalised data
#' @export
#'
normalise_prepost <- function(data, pre, post, method = "means_linear"){
  # Make pre-mean value
  if (length(pre > 1)){
    pre <- mean(pre, na.rm = TRUE)
  }

  # Make post-mean value
  if (length(post > 1)){
    post <- mean(post, na.rm = TRUE)
  }

  corrected_vals <- c()
  if (method == "means_linear"){
      subtracted_vals <- seq(from = pre, to = post, length.out = length(data))
      corrected_vals <- data - subtracted_vals
  }
  return(corrected_vals)
}

