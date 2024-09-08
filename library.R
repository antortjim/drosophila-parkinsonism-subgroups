create_directory_structure <- function(data_dir) {
  # Instead use a local drive (not a network drive)
  dir.create(paste(data_dir, "/output", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/arousal", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/quality_control", sep = ""), showWarnings = F)
  dir.create(paste(data_dir, "/output/freq_velocity_table", sep = ""), showWarnings = F)
}

get_t_after_sd <- function(meta_row) {
  as.numeric(
    as.Date(substr(meta_row$SD_end_datetime, 1, 10)) -
    as.Date(substr(meta_row$datetime, 1, 10))
  )
}