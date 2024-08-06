


DATA_DIR <- paste("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/ethoscope_results_Pieter/analysis/")
dt_combined <- read.csv("L:/GBW-0138_PAVER/Natalie/C2_ethoscope/ethoscope_results/___AI/ethoscope_results_Pieter/analysis/combined_scale_sleep5_parameter240627.csv")

dt_combined

#df.pca <- prcomp(dt_combined[,2:13], center = TRUE,scale. = TRUE)
df.pca <- prcomp(dt_combined[,2:13], center = FALSE,scale. = FALSE)

x <- as.data.frame(df.pca$x)
x$genotype <- dt_combined$genotype
x
library(ggplot2)
ggplot(data=x, aes(x=PC1, y=PC2, label=genotype)) + geom_point() + geom_text()


summary(df.pca)

typeof(dt_combined)
class(dt_combined)

install.packages("ggbiplot")
library(ggbiplot)

ggbiplot(df.pca, labels=dt_combined$genotype, alpha = 1, varname.adjust = 0.8, var.axes = TRUE)
ggbiplot(df.pca, alpha = 1, varname.adjust = 0.8, var.axes = TRUE)
ggbiplot(df.pca, choices = 1:2, labels=dt_combined$genotype)





#### Quality of representation

# Squared loadings (squared coordinates of variables on principal components)
loadings_squared <- df.pca$rotation^2

# Variance explained by each principal component
variance_explained <- df.pca$sdev^2

# Calculate the cos2 values
cos2 <- sweep(loadings_squared, 2, variance_explained, FUN = "*")

library(corrplot)
corrplot(cos2, is.corr = FALSE, tl.col = 'black', col = COL1('Reds', 100))







### Create the Scree Plot
#calculate total variance explained by each principal component
var_explained = df.pca$sdev^2 / sum(df.pca$sdev^2)

#create scree plot
library(ggplot2)

qplot(c(1:12), var_explained) + 
  geom_col() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)

