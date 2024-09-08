#' First round of plots in script
#' @import ggplot2
#' @import ggetho
make_plots <- function(dt_curated, output_folder, ID) {  
  print(ggetho(dt_curated, aes(y=asleep, colour=genotype)) +
    stat_pop_etho() +
    stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
    facet_grid(fly_no ~ .))


  print(ggetho(dt_curated, aes(y=moving, colour=genotype)) +
          stat_pop_etho() +
          stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
          facet_grid(fly_no ~ .))

  print(ggetho(dt_curated, aes(y=max_velocity, colour=genotype)) +
          stat_pop_etho() +
          stat_ld_annotations(height = 1, alpha=0.3, outline=NA) +
          facet_grid(fly_no ~ .))


  if(metadata[, any(optomotor=="YES")]) {
    print(ggetho(dt_curated, aes(y=interactions, colour=genotype)) +
          stat_pop_etho() +
          stat_ld_annotations() +
          facet_grid(fly_no ~ .))
  }



  #summary with offset
  gg <- ggetho(dt_curated, aes(y=asleep, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
    stat_pop_etho() +
    stat_ld_annotations() +
    scale_y_continuous(name= "Fraction of time sleeping", labels = scales::percent)
  ggsave(
    plot=gg,
    filename=file.path(
      output_folder,
      paste("ID",ID, "_graph_", "A0_percentage_sleep_over_time", ".png", sep="")
    )
  )

  gg <- ggetho(dt_curated, aes(y=moving, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
    stat_pop_etho() +
    stat_ld_annotations() +
    scale_y_continuous(name= "Fraction of time moving",limits = c(0,1),labels = scales::percent)

  ggsave(
    plot=gg,
    filename=file.path(
      output_folder,
      paste("ID",ID, "_graph_", "A0_percentage_activity_over_time", ".png", sep="")
    )
  )


  gg <- ggetho(dt_curated, aes(y=max_velocity, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
    stat_pop_etho() +
    stat_ld_annotations() +
    scale_y_continuous(name= "max_velocity",labels = scales::percent)


  gg <- ggetho(dt_curated, aes(y=beam_crosses, colour=genotype), time_wrap = hours(24), time_offset=hours(18)) +
    stat_pop_etho() +
    stat_ld_annotations() +
    scale_y_continuous(name= "beam crosses")
}