# function correctly averages multiple pre/post values

    Code
      result <- normalise_drift_means(data, pre, post)
    Message
      Using mean of 3 pre-measurements: 10
      Using mean of 3 post-measurements: 20
      Corrected for linear drift between 10 and 20

# function handles NA values in pre/post correctly

    Code
      result <- normalise_drift_means(data, pre, post)
    Message
      Using mean of 3 pre-measurements: 10
      Using mean of 3 post-measurements: 19.5
      Corrected for linear drift between 10 and 19.5

