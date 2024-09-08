analyse_mean_fraction_of_sleep_day_vs_night <- function(dt, output_dt, output_folder, ID, do_print=FALSE) {

  . <- asleep <- id <- phase <- NULL

  summary_dt <- rejoin(
    dt[,
      .(
        A2_sleep_fraction_l = mean(asleep[phase == "L"]),
        A2_sleep_fraction_d = mean(asleep[phase == "D"])
      ),
      by = id
    ]
  )

  output_dt$A2_sleep_fraction_day <- summary_dt$A2_sleep_fraction_l
  output_dt$A2_sleep_fraction_night <- summary_dt$A2_sleep_fraction_d

  return(output_dt)
}