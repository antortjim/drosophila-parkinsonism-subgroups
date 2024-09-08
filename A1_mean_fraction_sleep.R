analyse_mean_fraction_of_sleep <- function(dt, output_folder, ID, do_print=FALSE){

  . <- asleep <- id <- NULL
  
  summary_dt <- rejoin(dt[,
    .(A1_sleep_fraction = mean(asleep)),
    by = id]
  )


  if (do_print) {
    gg <- ggplot(summary_dt, aes(x=genotype, y=A1_sleep_fraction, fill=genotype)) +
      geom_boxplot(outlier.colour = "red", notch=TRUE, notchwidth = 0.5, varwidth = TRUE) +
      geom_jitter(alpha=.5, width = 0.15) +
      stat_summary(fun.y=mean, geom="point", shape=13, size=5, color="red", fill="red") +
      scale_y_continuous(name= "A1_sleep_fraction",labels = scales::percent)
    
    ggsave(
      filename=file.path(
        output_folder, 
        paste("ID",ID,"_graph_","A1_sleep_fraction",".png", sep="")
      ),
      plot=gg
    )
    print(gg)
  }
  return(summary_dt)
}

