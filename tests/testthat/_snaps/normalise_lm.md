# function warns about small datasets

    Code
      normalise_lm(data = test_data$data, data_pre = small_pre, data_post = small_post)
    Condition
      Warning:
      Very few observations in `data_pre` may lead to unstable baseline estimation
      Warning:
      Very few observations in `data_post` may lead to unstable baseline estimation
    Message
      Normalizing test_data$data using linear regression with time and co2d_um_m
    Output
         time  co2d_um_m
      1     0  4.3805310
      2     1  3.9079646
      3     2  3.4353982
      4     3  2.9628319
      5     4  2.4902655
      6     5  2.0176991
      7     6  1.5451327
      8     7  1.0725664
      9     8  0.6000000
      10    9  0.1274336
      11   10 -0.3451327

