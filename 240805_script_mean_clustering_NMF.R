DATA_DIR <- paste("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/ethoscope_results_Pieter/analysis/")

library(data.table)

dt_parameter <- read.csv("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/ethoscope_results_Pieter/analysis/summary_results_PDcollection_stats240627.csv")
scale_sleep5 <- read.csv("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/ethoscope_results_Pieter/analysis/scale_sleep5_240627.csv")

setDT(dt)
#####
col_names_df_to_remove<-c("morning_anticipation","evening_anticipation","peak_velocity", "latency")

dt_parameter_2 <- dt_parameter[, ! names(dt_parameter) %in% col_names_df_to_remove, drop = F]



combined_scale_sleep5_parameter<-merge(scale_sleep5, dt_parameter_2, by="genotype")

write_xlsx(combined_scale_sleep5_parameter,"L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/ethoscope_results_Pieter/analysis/combined_scale_sleep5_parameter240627.xlsx")

library(pheatmap)
data <- as.matrix(combined_scale_sleep5_parameter[,2:13])
rownames(data) <- combined_scale_sleep5_parameter$genotype

pheatmap(data,
         clustering_distance_rows="correlation",
         clustering_method="ward.D",
         main = "ward.D",
         cluster_cols = T, cellwidth = 15, cellheight = 15, angle_col = 45, treeheight_row = 100, cutree_rows = 6,
         width = 10, height = 20, units="cm", fontsize=11,
         color=colorRampPalette(c("blue","white","red"))(50))

