############### Read the COVID data ##############################
covid_metaboanalyst_pca_score <- read.csv("covid_metabolomics_pca_score.csv", row.names = 1)
covid_metaboanalyst_pca_score$covid <- c(rep(1,39), rep(0,20))

############### Parameters ##############################
projection_method <- "PCA"

############### Create projection plots ##############################
set.seed(42)
covid_tests_data_complete_plot <- create_projection_plots(
  data = covid_metaboanalyst_pca_score,
  class_column = "covid",
  case_labels = rownames(covid_metaboanalyst_pca_score),
  show_labels = TRUE
)

# Create individual plots
covid_ellipse_plot <- covid_tests_data_complete_plot$ellipse_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "Confidence ellipses for prior classes"
  )

covid_voronoi_plot <- covid_tests_data_complete_plot$voronoi_plot +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation for prior classes"
  )

covid_voronoi_ellipse_plot <- covid_tests_data_complete_plot$voronoi_plot_plus_ellipse +
  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  labs(
    title = paste0("COVID data: ", projection_method, " projection"),
    subtitle = "Voronoi tesselation and confidence ellipses for prior classes"
  )

# Combine plots
covid_combined_visualization_6 <- cowplot::plot_grid(
  covid_ellipse_plot,
  covid_voronoi_plot,
  covid_voronoi_ellipse_plot,
  labels = "AUTO",
  nrow = 1
)
print(covid_combined_visualization_6)

ggsave(
  filename = "covid_combined_visualization_6.svg",
  plot = covid_combined_visualization_6,
  width = 27,
  height = 9
)