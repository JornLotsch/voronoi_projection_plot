############### Libraries ##############################
library(mixOmics)
library(ggplot2)
library(cowplot)
library(ggpubr)
library(ggfortify)
library(ggthemes)

############### Constants ##############################
PROJECTION_TYPE <- "PLS-DA"  # Default projection type

############### Functions ##############################
perform_projection_analysis <- function(data_frame, projection_method = PROJECTION_TYPE) {
  switch(projection_method,
         "PLS-DA" = mixOmics::plsda(X = data_frame[,-1], Y = data_frame$class, scale = TRUE, ncomp = 2),
         "PCA" = mixOmics::pca(X = data_frame[,-1], scale = TRUE),
         mixOmics::plsda(X = data_frame[,-1], Y = data_frame$class, scale = TRUE))
}

source("create_projection_plots.R")

############### Read the pain data ##############################
pain_tests_data <- read.csv(file = paste0("/home/joern/Aktuell/QST_DIB/DataSetPublished/qst_pain_data_transformed", ".csv"), row.names = 1)
dim(pain_tests_data)
pain_tests_metadata <- read.csv(file = paste0("/home/joern/Aktuell/QST_DIB/DataSetPublished/qst_pain_metadata", ".csv"), row.names = 1)
pain_tests_data_complete <- na.omit(cbind.data.frame(class = pain_tests_metadata$Sex_m1, pain_tests_data))
dim(pain_tests_data_complete)
table(pain_tests_data_complete$class)

############### Parameters ##############################
projection_method <- "PLS-DA"

############### Project and plot the data ##############################

set.seed(42)
proj_pain_tests_data_complete <- perform_projection_analysis(
  data_frame = pain_tests_data_complete,
  projection_method = projection_method
)

proj_pain_tests_data_complete_data <- mixOmics::plotIndiv(
  proj_pain_tests_data_complete,
  ellipse = FALSE,
  legend = TRUE,
  style = "graphics"
)

pain_tests_data_complete_plot <- create_projection_plots(
  data = proj_pain_tests_data_complete_data$df,
  class_column = "group",
  case_labels = rownames(pain_tests_data_complete),
  show_labels = TRUE
)

# Create individual plots
pain_ellipse_plot <- pain_tests_data_complete_plot$ellipse_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("Pain data: ", projection_method, " projection"),
    subtitle = "Confidence ellipses for prior classes"
  )

pain_voronoi_plot <- pain_tests_data_complete_plot$voronoi_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("Pain data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation for prior classes"
  )

pain_voronoi_ellipse_plot <- pain_tests_data_complete_plot$voronoi_plot_plus_ellipse +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("Pain data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation and confidence ellipses for prior classes"
  )

# Combine plots
pain_combined_visualization_6 <- cowplot::plot_grid(
  pain_ellipse_plot,
  pain_voronoi_plot,
  pain_voronoi_ellipse_plot,
  labels = "AUTO",
  nrow = 1
)
print(pain_combined_visualization_6)

ggsave(
  filename = "pain_combined_visualization_6.svg",
  plot = pain_combined_visualization_6,
  width = 18,
  height = 6
)

############### PCA and Ward ##############################
df <- pain_tests_data_complete[,-1]
pca_res <- prcomp(df, scale. = TRUE)
autoplot(pca_res, loadings = TRUE, loadings.label = TRUE)
autoplot(pca_res, variance_percentage = TRUE)

# PCA
nPC <- sum(pca_res$sdev^2 > 1)

# Ward's Method
pain_ward <- hclust(dist(pca_res$x[,1:nPC]), method = "ward.D2")
plot(pain_ward)
ward_clusters <- cutree(pain_ward, 2)

wilcox.p <- -log10(apply(pain_tests_data_complete[,-1], 2, function(x) wilcox.test(x ~ ward_clusters)$p.value))
barplot(sort(wilcox.p), las = 2)
pain_tests_data_complete_clusters <- cbind.data.frame(
  cluster = ward_clusters,
  class = pain_tests_data_complete$class,
  pca_res$x[,1:nPC]
)

create_projection_plots(
  data = pain_tests_data_complete_clusters,
  class_column = "cluster",
  coordinate_columns = c("PC1", "PC2"),
  case_labels = rownames(pain_tests_data_complete_clusters),
  alternative_class_column = "class",
  show_labels = TRUE,
  fill_voronoi = "alternative"
)