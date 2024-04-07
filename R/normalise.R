#' Normalise Breathing Traces
#'
#' @param data Data from trial
#' @param data_pre Data, pre-trial
#' @param data_post Data, post-trial
#' @param method Which method is used to normalise the data
#'
#' @return Normalised data
#' @export
#'
normalise_prepost <- function(data, data_pre, data_post, method = "means_linear"){
  cols <- c("co2d_um_m", "h2od_dp_c", "h2od_mm_m")
  if (method == "means_linear"){
    for (i in cols){
      pre_mean <- mean(data_pre[[i]])
      post_mean <- mean(data_post[[i]])
      subtracted_vals <- seq(from = pre_mean, to = post_mean, length.out = nrow(data))
      data[[i]] <- data[[i]] - subtracted_vals
    }
  }
  return(data)
}

