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

############### Read the PsA data ##############################
PsA_tests_data <- read.csv(file = paste0("/home/joern/Aktuell/RheumaMetabolomicsFFM/DataSetPublished/PsA_lipids_BC", ".csv"), row.names = 1)
dim(PsA_tests_data)
PsA_tests_metadata <- read.csv(file = paste0("/home/joern/Aktuell/RheumaMetabolomicsFFM/DataSetPublished/PsA_classes", ".csv"), row.names = 1)
PsA_tests_data_complete <- na.omit(cbind.data.frame(class = PsA_tests_metadata$PsA, PsA_tests_data))
dim(PsA_tests_data_complete)
table(PsA_tests_data_complete$class)

############### Parameters ##############################
projection_method <- "PLS-DA"

############### Project and plot the data ##############################

set.seed(42)
proj_PsA_tests_data_complete <- perform_projection_analysis(
  data_frame = PsA_tests_data_complete,
  projection_method = projection_method
)

proj_PsA_tests_data_complete_data <- mixOmics::plotIndiv(
  proj_PsA_tests_data_complete,
  ellipse = FALSE,
  legend = TRUE,
  style = "graphics"
)

PsA_tests_data_complete_plot <- create_projection_plots(
  data = proj_PsA_tests_data_complete_data$df,
  class_column = "group",
  case_labels = rownames(PsA_tests_data_complete),
  show_labels = TRUE
)

# Create individual plots
PsA_ellipse_plot <- PsA_tests_data_complete_plot$ellipse_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("PsA data: ", projection_method, " projection"),
    subtitle = "Confidence ellipses for prior classes"
  )

PsA_voronoi_plot <- PsA_tests_data_complete_plot$voronoi_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("PsA data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation for prior classes"
  )

PsA_voronoi_ellipse_plot <- PsA_tests_data_complete_plot$voronoi_plot_plus_ellipse +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("PsA data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation and confidence ellipses for prior classes"
  )

# Combine plots
PsA_combined_visualization_6 <- cowplot::plot_grid(
  PsA_ellipse_plot,
  PsA_voronoi_plot,
  PsA_voronoi_ellipse_plot,
  labels = "AUTO",
  nrow = 1
)
print(PsA_combined_visualization_6)

ggsave(
  filename = "PsA_combined_visualization_6.svg",
  plot = PsA_combined_visualization_6,
  width = 18,
  height = 6
)
